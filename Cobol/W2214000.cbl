000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W2214000                                             
000300 AUTHOR.             IDK, GÖTEBORG.                                       
000400 DATE-WRITTEN.       NOV  1978.                                           
000500     SKIP2                                                                
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*            PROGRAMMET UTFÖR LEVERANSPLANEBERÄKNINGAR.                   
001000*            HUVUDPROGRAMMET LÄSER ARTIKELREGISTRET I SEKVENS.            
001100*            SUBMODULER ANROPAS FÖR VARJE ARTIKEL,                        
001200*            VAREFTER FÖRÄNDRADE DATA UPDATERAS PÅ DATA-                  
001300*            BASERNA.                                                     
001400*                                                                         
001500*            ALLA UPPDATERINGAR SKER I BMP W2214A00.                      
001600*                                                                         
001700*    SUBPROGRAM.                                                          
001800*            W2214010    IMS SUBMODUL                                     
001900*            W2214030    PUNKTBERÄKNINGS MODUL                            
002000*            W2214040    BESTÄLLNINGSKONTROLL MODUL                       
002100*            W2214060    KONTROLL KORRIGERING LEVPLAN                     
002200*            W22222      BERÄKNING AV VECKOBEHOV                          
002300*            W009VADD    ADD AV VECKOR TILL DATUM                         
002400*            W221LPAD    ADD AV ORSAKSKODER TILL TABELL                   
002500*            DATKORT                                                      
002600*            POSTSUM                                                      
002700*    RETURKOD                                                             
002800*            +25     TABELLER FÖR SMÅ                                     
002900*                                                                         
003000*                                                                         
003100*    ÄNDRINGAR: E-TRACKER 715406                                          
003200*               LÄGGER IN BORTTAG AV ARTIKEL FRÅN WDD601, SAM-            
003300*               TIDIGT SOM BORTTAG AV FÖR GAMLA OMSPECAR GÖRS.//CE        
003400*               PBI 1465788 DT 20190925 FLYTTAR UT WDD6 TILL EN           
003500*               NY BMP W2214A00.                                          
003600*                                                                         
003700*               E-TRACKER 6490267                                         
003800*               ÄNDRA ANTAL INLEV-DAGAR FÖR FPTYP 06 OCH 07 TILL          
003900*               ATT VARA 5 DAGAR.  (MOTSV. ÄNDR I W2211600)               
004000*                  (SE "FIX FÖR BESTÄMD VECKA")                           
004100*                  (= TILLFÄLLIG KÖRNING VID VISST DATUM)  //CE           
004200*                                                                         
004300*               150422 - E'TRACKER 10130993                               
004400*               REDUCE NUMBER OF DELIVERY SCHEDULES                       
004500*                                                                         
004600*               AZURE PBI 1465788 2019-10-18                              
004700*               REWRITE BATCH PGM TO BMP RTN W200V1                       
004800*               FLYTTAT ALLA UPPDATERINGAR TILL BMP W2214A00.             
004900*                                                                         
005000*                                                                         
005100     EJECT                                                                
005200 ENVIRONMENT DIVISION.                                                    
005300 INPUT-OUTPUT SECTION.                                                    
005400 FILE-CONTROL.                                                            
005500     SKIP3                                                                
005600*                            *** W.PROD.CONSTANT(W22140LN)     ***        
005700*                            *** INPUT          (KDLEVPLF = G) ***        
005800      SELECT W221LN  ASSIGN  UT-S-W22140DA.                               
005900*                                                                         
006000*                            *** HÄNDELSEFIL 2204              ***        
006100*                            *** INPUT                         ***        
006200      SELECT W22131  ASSIGN  UT-S-W22140D2.                               
006300*                                                                         
006400*                            *** LEVNR - SPEC.KONTROLLVECKA    ***        
006500*                            *** INPUT                         ***        
006600      SELECT W22133  ASSIGN  UT-S-W22140D3.                               
006700*                                                                         
006800*                            *** ANSKAFFARE ANNUL. LEV.PLAN    ***        
006900*                            *** INPUT                         ***        
007000      SELECT W22135  ASSIGN  UT-S-W22140DC.                               
007100*                                                                         
007200*                            *** BEGÄRAN AV OMSPEC             ***        
007300*                            *** OUTPUT                        ***        
007400      SELECT W22142  ASSIGN  UT-S-W22140D4.                               
007500*                                                                         
007600*                            *** UPPDATERING LEVERANSPLANER    ***        
007700*                            *** OUTPUT TILL BMP W2214A00      ***        
007800      SELECT W2214A  ASSIGN  UT-S-W22140D5.                               
007900*                                                                         
008000*                            *** HF-FIL                        ***        
008100*                            *** OUTPUT                        ***        
008200      SELECT W22199  ASSIGN  UT-S-W22140D7.                               
008300*                                                                         
008400*                            *** FIL TILL W21802 -> WDR5       ***        
008500*                            *** OUTPUT                        ***        
008600*                            *** OMR. AV DISP.DAT NÄSTA INLEV  ***        
008700      SELECT W22148  ASSIGN  UT-S-W22140DD.                               
008800*                                                                         
008900*                            *** PB-ÄNDRINGAR                  ***        
009000      SELECT W22149  ASSIGN  UT-S-W22140DE.                               
009100                                                                          
009200     EJECT                                                                
009300 DATA DIVISION.                                                           
009400 FILE SECTION.                                                            
009500     SKIP3                                                                
009600 FD  W221LN                                                               
009700     RECORDING F                                                          
009800     BLOCK 0                                                              
009900     LABEL RECORD STANDARD.                                               
010000 01  POST                 PIC X(80).                                      
010100     SKIP3                                                                
010200 FD  W22131                                                               
010300     RECORDING F                                                          
010400     BLOCK 0                                                              
010500     LABEL RECORD STANDARD.                                               
010600*01  POST -COPY W2212204   -L.                                            
010700     SKIP3                                                                
010800 FD  W22133                                                               
010900     RECORDING F                                                          
011000     BLOCK 0                                                              
011100     LABEL RECORD STANDARD.                                               
011200*01  POST -COPY W221002    -L.                                            
011300     SKIP3                                                                
011400 FD  W22135                                                               
011500     RECORDING F                                                          
011600     BLOCK 0                                                              
011700     LABEL RECORD STANDARD.                                               
011800*01  POST -COPY W22135     -L.                                            
011900     SKIP3                                                                
012000 FD  W22142                                                               
012100     RECORDING F                                                          
012200     BLOCK 0                                                              
012300     LABEL RECORD STANDARD.                                               
012400*01  POST -COPY W221LI42   -L  -PRE U42BEG-.                              
012500     SKIP3                                                                
012600 FD  W2214A                                                               
012700     RECORDING F                                                          
012800     BLOCK 0                                                              
012900     LABEL RECORD STANDARD.                                               
013000*01  POST -COPY W2214A     -L  -PRE U4A-.                                 
013100     SKIP3                                                                
013200 FD  W22149                                                               
013300     RECORDING F                                                          
013400     BLOCK 0                                                              
013500     LABEL RECORD STANDARD.                                               
013600*01  POST -COPY W22149     -L  -PRE W22149-.                              
013700     SKIP3                                                                
013800 FD  W22199                                                               
013900     RECORDING F                                                          
014000     BLOCK 0                                                              
014100     LABEL RECORD STANDARD.                                               
014200 01  U99-POST                   PIC X(400).                               
014300     SKIP3                                                                
014400                                                                          
014500 FD  W22148                                                               
014600     RECORDING F                                                          
014700     BLOCK 0                                                              
014800     LABEL RECORD STANDARD.                                               
014900*01  POST -COPY W21801     -L  -PRE U22148-.                              
015000     SKIP3                                                                
015100     EJECT                                                                
015200                                                                          
015300 WORKING-STORAGE SECTION.                                                 
015400     SKIP2                                                                
015500*    -COPY WY2000W3                                                       
015600     SKIP3                                                                
015700*    -COPY WY2000W2                                                       
015800     SKIP3                                                                
015900*    -COPY WY2000W1                                                       
016000     SKIP3                                                                
016100*    -COPY WY2000W9                                                       
016200     SKIP3                                                                
016300*    -COPY WWPRODSL                                                       
016400     SKIP3                                                                
016500 77  IDPGM                       PIC X(8)    VALUE 'W2214000'.            
016600 77  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
016700 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
016800     SKIP3                                                                
016900 01  W-KDLPORS               PIC 9(03).                                   
017000*    --- VARIABLER TILL SUBPROGRAM W221LPAD                               
017100 01  W-W221LP-CTX            PIC X(08) VALUE 'W221LP01'.                  
017200 01  W-KDLPORS-GRP.                                                       
017300     03 W-KDLPORS-TAB OCCURS 4 PIC 9(03).                                 
017400*                                                                         
017500 01  KONSTANTER.                                                          
017600     03  JA                  PIC X       VALUE 'J'.                       
017700     03  NEJ                 PIC X       VALUE 'N'.                       
017800*                                                                         
017900*      --- VALID IDDC CODES                                               
018000*01  -COPY WWDCKONS                                                       
018100*                                                                         
018200     03  RAETT               PIC X       VALUE 'R'.                       
018300     03  FEL                 PIC X       VALUE 'F'.                       
018400*                                                                         
018500     03  RV-FIX-1            PIC S9(1)   COMP-3    VALUE ZERO.            
018600*                                                                         
018700     03  INDX                PIC S9(4)   VALUE +0    COMP SYNC.           
018800     03  MAX-INDX            PIC S9(4)   VALUE +156  COMP SYNC.           
018900*                                                                         
019000     03  OPPNA               PIC S9(3)   VALUE +1    COMP-3.              
019100     03  BEARBETA            PIC S9(3)   VALUE +2    COMP-3.              
019200     03  AVSLUTA             PIC S9(3)   VALUE +3    COMP-3.              
019300     03  LAES-ARTIKEL-DATA   PIC S9(3)   VALUE +401  COMP-3.              
019400     03  LAES-LEVERANTOER    PIC S9(3)   VALUE +402  COMP-3.              
019500     03  LAES-OMSPEC         PIC S9(3)   VALUE +403  COMP-3.              
019600     03  LAES-ARTIKEL        PIC S9(3)   VALUE +404  COMP-3.              
019700     03  UPPDAT-CLAG         PIC S9(3)   VALUE +405  COMP-3.              
019800     03  BORTTAG-OMSPEC      PIC S9(3)   VALUE +410  COMP-3.              
019900     03  BORTTAG-KOPPLING-LP PIC S9(3)   VALUE +413  COMP-3.              
020000     SKIP1                                                                
020100     03  LAES-AVROP-FIRST    PIC S9(3)   VALUE +420  COMP-3.              
020200     03  LAES-AVROP-NEXT     PIC S9(3)   VALUE +421  COMP-3.              
020300     03  LAES-AVROP-KVAL     PIC S9(3)   VALUE +422  COMP-3.              
020400     03  UPPDAT-AVROP        PIC S9(3)   VALUE +423  COMP-3.              
020500     03  BORTTAG-AVROP       PIC S9(3)   VALUE +424  COMP-3.              
020600     03  NYUPPL-AVROP        PIC S9(3)   VALUE +425  COMP-3.              
020700     03  LAES-AVROP-LEV-F    PIC S9(3)   VALUE +426  COMP-3.              
020800     03  LAES-AVROP-LEV-N    PIC S9(3)   VALUE +427  COMP-3.              
020900     03  LAES-WDB6-DC-INFO   PIC S9(3)   VALUE +428  COMP-3.              
021000     SKIP1                                                                
021100*--------------------------------------------------------------           
021200*--- TYP AV UPPDATERINGAR TILL BMP W2214A00 UT-FIL W2214A.                
021300*--------------------------------------------------------------           
021400     03  UPPDAT-CLAG-4A          PIC X(3)    VALUE '001'.                 
021500     03  BORTTAG-KOPPLING-LP-4A  PIC X(3)    VALUE '002'.                 
021600     03  BORTTAG-OMSPEC-4A       PIC X(3)    VALUE '003'.                 
021700     03  BORTTAG-AVROP-4A        PIC X(3)    VALUE '004'.                 
021800                                                                          
021900*--------------------------------------------------------------           
022000     SKIP1                                                                
022100     03  SEMESTER-VECKA-START                                             
022200                             PIC S9(3)   VALUE +999    COMP-3.            
022300     03  SEMESTER-VECKA-SLUT PIC S9(3)   VALUE +0      COMP-3.            
022400     03  OLIKA-LEV           PIC X(5).                                    
022500       88 RENAULT-LEVNR                 VALUE '3868'                      
022600                                              'K8M6A'.                    
022700       88 VOLKSWAGEN-LEVNR              VALUE '6453'                      
022800                                              'Q09EB'.                    
022900       88 ALLISON-LEVNR                 VALUE '4175'.                     
023000       88 EATON-LEVNR                   VALUE '5333'.                     
023100       88 SOMA-LEVNR                    VALUE '3680'.                     
023200       88 TRW-LEVNR                     VALUE '5362'                      
023300                                              'R9K2A'.                    
023400       88 SATS-LEVNR                    VALUE '1002'.                     
023500       88 GEMEN-LEVNR                   VALUE '14489'                     
023600                                              'DL7YA'.                    
023700       88 EGET-LEVNR                    VALUE '1441'                      
023800                                              'BP2TW'.                    
023900     03  SEP-SATS-TPO-LEV-SDC-NDC                                         
024000                             PIC X(2)    VALUE '19'.                      
024100     03  SATS-TPO-LEV-SDC-NDC                                             
024200                             PIC X(2)    VALUE '18'.                      
024300     SKIP2                                                                
024400 01  ARTNR-FIX               PIC 9(9).                                    
024500 01  FILLER REDEFINES ARTNR-FIX.                                          
024600     03  FILLER              PIC 9(8).                                    
024700     03  SLUTSIFFRA          PIC 9.                                       
024800     SKIP3                                                                
024900 01  UDDA-DATUM              PIC 9(4)V9.                                  
025000 01  FILLER  REDEFINES UDDA-DATUM.                                        
025100     03  FILLER              PIC 9(4).                                    
025200     03  RESTEN              PIC 9.                                       
025300     SKIP2                                                                
025400 01  ARBETSAREOR.                                                         
025500     03  IX                  PIC S9(9)   VALUE +0    COMP SYNC.           
025600     03  IX-SUM              PIC S9(9)   VALUE +0    COMP SYNC.           
025700     03  IX-LN               PIC S9(9)   VALUE +0    COMP SYNC.           
025800     03  IX-DAG              PIC S9(9)   VALUE +0    COMP SYNC.           
025900     SKIP1                                                                
026000     03  W-NUM4              PIC 9(4).                                    
026100     03  W-DATUM-AAVV        PIC 9(4).                                    
026200     03  W-DAT-AAVV          REDEFINES W-DATUM-AAVV.                      
026300         05  W-DATUM-AA      PIC 9(2).                                    
026400         05  W-DATUM-VV      PIC 9(2).                                    
026500     03  W-DATUM-AAMMDD      PIC 9(6).                                    
026600     03  W-DAT2-AAMMDD       REDEFINES W-DATUM-AAMMDD.                    
026700         05  W-DATUM2-AA     PIC 9(2).                                    
026800         05  W-DATUM2-MM     PIC 9(2).                                    
026900         05  W-DATUM2-DD     PIC 9(2).                                    
027000     03  W-HELA-DATUMET      PIC 9(8).                                    
027100     03  FILLER              REDEFINES W-HELA-DATUMET.                    
027200         05  W-HELA-DATUMET-SEKEL   PIC 9(2).                             
027300         05  W-HELA-DATUMET-AAMMDD  PIC 9(6).                             
027400     SKIP1                                                                
027500     03  W-DATUM-GRAENS      PIC S9(5)               COMP-3.              
027600     03  W-ANTAL-VECKOR      PIC S9(3)               COMP-3.              
027700     03  W-IDLEVNR           PIC X(5).                                    
027800     03  W-KVBR-TOT          PIC S9(7)               COMP-3.              
027900     03  W-KVBR              PIC S9(7)               COMP-3.              
028000     03  W-ANTAL             PIC S9(7)               COMP-3.              
028100     03  W-IDLPKOLL          PIC S9(1)               COMP-3.              
028200     03  W-TIFINLV-AAVV      PIC S9(5)               COMP-3.              
028300     03  W-TIFINLV           PIC 9(5).                                    
028400     03  FILLER REDEFINES W-TIFINLV.                                      
028500         05  W-TIFINLV-AA    PIC 9(2).                                    
028600         05  W-TIFINLV-VV    PIC 9(2).                                    
028700         05  FILLER          PIC 9.                                       
028800     03  ANTAL-VV            PIC S9(5)   COMP-3  VALUE ZERO.              
028900     03  AKT-DATUM-AAVV      PIC 9(4).                                    
029000     03  FILLER REDEFINES AKT-DATUM-AAVV.                                 
029100         05  AKT-DATUM-AA    PIC 9(2).                                    
029200         05  AKT-DATUM-VV    PIC 9(2).                                    
029300     SKIP1                                                                
029400     03  W-SEMESTER-VV-START PIC S9(5)               COMP-3.              
029500     03  W-SEMESTER-VV-SLUT  PIC S9(5)               COMP-3.              
029600     03  W-KVVECKOR-INLEV    PIC S9(5)               COMP-3.              
029700     03  W-KVDAGAR-FFH       PIC S9(5)               COMP-3.              
029800     03  W-KVVECKOR-FFH      PIC S9(5)               COMP-3.              
029900     03  W-KVVECKOR-TEMP1    PIC S9(5)               COMP-3.              
030000     03  W-KVVECKOR-TEMP2    PIC S9(5)               COMP-3.              
030100     SKIP1                                                                
030200     03  WS-TIDISPIN-C1      PIC S9(6).                                   
030300     03  WS-TIDISPIN-C2      PIC S9(6).                                   
030400     03  WS-TIFINLV          PIC S9(6).                                   
030500                                                                          
030600     03  FLANSK-TEST         PIC X        VALUE SPACE.                    
030700         88 FLANSK-TRAEFF                 VALUE 'J'.                      
030800                                                                          
030900     03  WS-INDATA-TEST      PIC X        VALUE SPACE.                    
031000         88 WS-INDATA-RAETT               VALUE 'R'.                      
031100                                                                          
031200     03  SW-TRAEFF           PIC X        VALUE 'N'.                      
031300         88 TRAEFF                        VALUE 'J'.                      
031400                                                                          
031500     03  WS-SUB-IDANSK       PIC 9(3)     VALUE ZERO.                     
031600     03  W-KVDAGAR-FORP      PIC S9(3).                                   
031700                                                                          
031800     03  WART-TILEVDAG OCCURS 5  PIC S9   COMP-3.                         
031900     SKIP3                                                                
032000 01  EOF-SWITCHAR.                                                        
032100     03  W22131-EOF          PIC X        VALUE 'N'.                      
032200     03  W22133-EOF          PIC X        VALUE 'N'.                      
032300     03  W22135-EOF          PIC X        VALUE 'N'.                      
032400     03  W221LN-EOF          PIC X        VALUE 'N'.                      
032500                                                                          
032600     03 FL22148POST-C1       PIC X        VALUE 'N'.                      
032700     03 FL22148POST-C2       PIC X        VALUE 'N'.                      
032800     03 FL-LEVPLAN-SAKNAS    PIC X        VALUE 'N'.                      
032900                                                                          
033000     SKIP1                                                                
033100     03  SW-W2214060         PIC X        VALUE 'N'.                      
033200     SKIP3                                                                
033300 01  DYNAMISKA-SUBPROGRAM.                                                
033400     03  W2214010            PIC X(8)     VALUE 'W2214010'.               
033500     03  W2214030            PIC X(8)     VALUE 'W2214030'.               
033600     03  W2214040            PIC X(8)     VALUE 'W2214040'.               
033700     03  W2214060            PIC X(8)     VALUE 'W2214060'.               
033800     03  W22222              PIC X(8)     VALUE 'W22222  '.               
033900     03  W222PBTO            PIC X(8)     VALUE 'W222PBTO'.               
034000     03  W009VADD            PIC X(8)     VALUE 'W009VADD'.               
034100     03  W221LPAD            PIC X(8)     VALUE 'W221LPAD'.               
034200     03  DATKORT             PIC X(8)     VALUE 'DATKORT '.               
034300     03  WDATKONV            PIC X(8)     VALUE 'WDATKONV'.               
034400     03  POSTSUM             PIC X(8)     VALUE 'POSTSUM '.               
034500     EJECT                                                                
034600*------------------------------------- PARAMETRAR TILL KVPB-PLAN          
034700*01  -COPY W222PBTO                                                       
034800     EJECT                                                                
034900*----------------------------------------- BYTES-ARTIKEL.                 
035000 01  FILLER                  PIC X(16)   VALUE 'BYTES-ARTIKEL'.           
035100 01  TEST-IDARTNR            PIC 9(9)    COMP-3.                          
035200*01  FILLER     -COPY WWBYT02    -RED  TEST-IDARTNR.                      
035300     EJECT                                                                
035400*01  FILLER     -COPY WWBYT03    -RED  TEST-IDARTNR.                      
035500     EJECT                                                                
035600*----------------------------------------- LEVERANTÖRNR MED               
035700*                                          UNDANTAG AUTOPLAN              
035800 01  LN-TABELL.                                                           
035900     03  LN-TAB-MAX          PIC S9(9)   VALUE +620  COMP SYNC.           
036000     SKIP1                                                                
036100     03  LN-TAB-INGANG       OCCURS 620.                                  
036200         05  LN-TAB-IDLEVNR      PIC X(5).                                
036300         05  LN-TAB-IDANSK-START PIC S9(3)          COMP-3.               
036400         05  LN-TAB-IDANSK-SLUT  PIC S9(3)          COMP-3.               
036500     SKIP3                                                                
036600*----------------------------------------- LEVERANTÖRNR MED SPEC.         
036700*                                          KONTROLLVECKA                  
036800 01  LEVERANTOER-TABELL.                                                  
036900     03  LEVTAB-MAX          PIC S9(9)   VALUE +512  COMP SYNC.           
037000     SKIP1                                                                
037100     03  LEVTAB-INGANG       OCCURS 512                                   
037200                             ASCENDING KEY IS LEVTAB-IDLEVNR              
037300                             INDEXED BY LEVTAB-IX.                        
037400         05  LEVTAB-IDLEVNR  PIC X(5).                                    
037500         05  LEVTAB-IDLPKOLL PIC S9(1)               COMP-3.              
037600     SKIP3                                                                
037700*----------------------------------------- 2204 - TABELL                  
037800*                                          HÄNDELSE-TABELL                
037900 01  TAB2204-TABELL.                                                      
038000     03  TAB2204-MAX         PIC S9(9)   VALUE +25000 COMP SYNC.          
038100     SKIP1                                                                
038200     03  TAB2204-INGANG      OCCURS 25000                                 
038300                             ASCENDING KEY IS TAB2204-IDARTNR             
038400                             INDEXED BY TAB2204-IX.                       
038500         05  TAB2204-IDARTNR PIC S9(9)               COMP-3.              
038600         05  TAB2204-KDLPORS-1                                            
038700                             PIC 9(2).                                    
038800         05  TAB2204-KDLPORS-2                                            
038900                             PIC 9(2).                                    
039000         05  TAB2204-KDLPORS-3                                            
039100                             PIC 9(2).                                    
039200     EJECT                                                                
039300*-----------------------------------------  TABELL                        
039400*                        ANSKAFFARE SOM INTE SKALL HA LEV-PLAN            
039500 01  TABANSK-TABELL.                                                      
039600     03  TABANSK-MAX         PIC S9(9)   VALUE +100 COMP SYNC.            
039700     SKIP1                                                                
039800     03  TABANSK-INGANG      OCCURS 100                                   
039900                             ASCENDING KEY IS TABANSK-IDANSK              
040000                             INDEXED BY TABANSK-IX.                       
040100         05  TABANSK-IDANSK  PIC S9(3)               COMP-3.              
040200     EJECT                                                                
040300*    -COPY W221SLEV                                                       
040400     EJECT                                                                
040500*                            *** PARAMETRAR TILL DATUMKORT                
040600 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W22140'.                  
040700 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
040800*01  -COPY WDATKORT.                                                      
040900     EJECT                                                                
041000*                            *** PARAMETRAR TILL DATUMKORT                
041100*01  -COPY W0005      -PRE POSTSUM-.                                      
041200     EJECT                                                                
041300*                            *** PARAMETRAR TILL DATKONV                  
041400*01  -COPY WDATAREA                                                       
041500     EJECT                                                                
041600*                            *************************************        
041700*                            ** AREA FÖR W221LN  INPUT          **        
041800*                            ** LEVNR SOM SKALL FÅ KDLEVPLF = G **        
041900*                            *************************************        
042000 01  INLN-AREA.                                                           
042100     03  INLN-IDLEVNR        PIC X(5).                                    
042200     03  FILLER              PIC X(1).                                    
042300     03  INLN-IDANSK-START   PIC 9(3).                                    
042400     03  FILLER              PIC X(1).                                    
042500     03  INLN-IDANSK-SLUT    PIC 9(3).                                    
042600     03  FILLER              PIC X(67).                                   
042700     EJECT                                                                
042800*                            *************************************        
042900*                            ** AREA FÖR W22131  INPUT          **        
043000*                            ** HÄNDELSE-FIL                    **        
043100*                            *************************************        
043200*01  AREA  -COPY W2212204   -PRE I2204-.                                  
043300     EJECT                                                                
043400*                            *************************************        
043500*                            ** AREA FÖR W22133  INPUT          **        
043600*                            ** LEVNR - KONTROLLVECKA           **        
043700*                            *************************************        
043800*01  AREA  -COPY W221002    -PRE I33LEV-.                                 
043900     EJECT                                                                
044000*                            *************************************        
044100*                            ** AREA FÖR W22135  INPUT          **        
044200*                            ** ANSK  SOM EJ SKALL HA LEV.PLAN  **        
044300*                            *************************************        
044400*01  SUB-AREA  -COPY W22135.                                              
044500     EJECT                                                                
044600*                            *************************************        
044700*                            ** AREA FÖR W22142  OUTPUT         **        
044800*                            ** BEGÄRAN AV OMSPEC               **        
044900*                            *************************************        
045000*01  AREA  -COPY W221LI42   -PRE U42BEG-.                                 
045100     EJECT                                                                
045200*                            *************************************        
045300*                            ** AREA FÖR W2214A  OUTPUT         **        
045400*                            ** UPPDATERINGSPOSTER TILL W2214A00**        
045500*                            *************************************        
045600*01  AREA  -COPY W2214A     -PRE U4A-.                                    
045700     EJECT                                                                
045800*                            *************************************        
045900*                            ** AREA FÖR W22149  OUTPUT         **        
046000*                            ** PB-FIL                          **        
046100*                            *************************************        
046200*01  AREA  -COPY W22149     -PRE W22149-.                                 
046300     EJECT                                                                
046400*                            *************************************        
046500*                            ** AREA FÖR W22148  OUTPUT         **        
046600*                            ** FIL TILL W218V1                 **        
046700*                            *************************************        
046800*01  AREA  -COPY W21801     -PRE U22148-.                                 
046900     EJECT                                                                
047000*                            *************************************        
047100*                            ** AREA FÖR KONTOLL OM ÄNDRINGAR   **        
047200*                            ** HAR GJORTS                      **        
047300*                            *************************************        
047400*01  AREA  -COPY W221L401   -PRE WLINK-.                                  
047500     EJECT                                                                
047600*                            *************************************        
047700*                            ** LINK-AREA                       **        
047800*                            ** LEVERANSPLANE-BERÄKNINGAR       **        
047900*                            *************************************        
048000 01  FILLER                  PIC X(16) VALUE 'LINKLINKLINKLINK'.          
048100*01  AREA  -COPY W221L401   -PRE LINK-.                                   
048200     EJECT                                                                
048300*                            *************************************        
048400*                            ** LNK2-AREA                       **        
048500*                            ** VECKOBEHOVS-BERÄKNING           **        
048600*                            *************************************        
048700*01  AREA  -COPY W222L222   -PRE LNK2-.                                   
048800     EJECT                                                                
048900*                            *************************************        
049000*                            ** LINK3-AREA                      **        
049100*                            ** LEVERANSPLAN AVROP              **        
049200*                            *************************************        
049300*01  AREA  -COPY W221L402   -PRE LINK3-.                                  
049400     EJECT                                                                
049500 01  FILLER                  PIC X(16) VALUE 'SPARAREA W22222'.           
049600*01  AREA  -COPY W222L222   -PRE SPAR-.                                   
049700     EJECT                                                                
049800 LINKAGE SECTION.                                                         
049900     SKIP3                                                                
050000*01  -COPY W0008 -PRE WDK6-.                                              
050100     05  FILLER              PIC X.                                       
050200     EJECT                                                                
050300*01  -COPY W0008 -PRE WDD9-.                                              
050400     05  FILLER              PIC X.                                       
050500     EJECT                                                                
050600 01  W222-AA-PCB             PIC X.                                       
050700     EJECT                                                                
050800*01  -COPY W0008 -PRE WDK7-.                                              
050900     05  FILLER              PIC X.                                       
051000     EJECT                                                                
051100*01  -COPY W0008 -PRE ARTM-.                                              
051200     05  FILLER              PIC X.                                       
051300     EJECT                                                                
051400*01  -COPY W0008 -PRE WDL2-.                                              
051500     05  FILLER              PIC X.                                       
051600     EJECT                                                                
051700 01  W222-2501-PCB           PIC X.                                       
051800     EJECT                                                                
051900 01  W222-WDB6R-PCB          PIC X.                                       
052000     EJECT                                                                
052100 01  W222-WDK7R-PCB          PIC X.                                       
052200     EJECT                                                                
052300*01  -COPY W0008 -PRE WDF1-.                                              
052400     05  FILLER              PIC X.                                       
052500     EJECT                                                                
052600*01  -COPY W0008 -PRE WDP6-.                                              
052700     05  FILLER              PIC X.                                       
052800     EJECT                                                                
052900*01  -COPY W0008      -PRE WDB6-                                          
053000     05  FILLER              PIC X.                                       
053100     EJECT                                                                
053200 01  W222-WDD7-PCB           PIC X.                                       
053300 01  W222-WDK7E-PCB          PIC X.                                       
053400 01  W222-UTIL-WDK6-PCB      PIC X.                                       
053500 01  W222-UTIL-WDK7-PCB      PIC X.                                       
053600 01  W222-UTIL-WDB6-PCB      PIC X.                                       
053700 01  W222-UTUP-WDK7-PCB      PIC X.                                       
053800 01  W222-UTUP-WDB6-PCB      PIC X.                                       
053900 01  W222-UTUP-UTIL-WDK6-PCB PIC X.                                       
054000 01  W222-UTUP-UTIL-WDK7-PCB PIC X.                                       
054100 01  W222-UTUP-UTIL-WDB6-PCB PIC X.                                       
054200     EJECT                                                                
054300 PROCEDURE DIVISION  USING WDK6-PCB WDD9-PCB W222-AA-PCB WDK7-PCB         
054400                           ARTM-PCB WDL2-PCB W222-2501-PCB                
054500                           W222-WDB6R-PCB W222-WDK7R-PCB                  
054600                           WDF1-PCB WDP6-PCB WDB6-PCB                     
054700                           W222-WDD7-PCB  W222-WDK7E-PCB                  
054800                           W222-UTIL-WDK6-PCB                             
054900                           W222-UTIL-WDK7-PCB                             
055000                           W222-UTIL-WDB6-PCB                             
055100                           W222-UTUP-WDK7-PCB                             
055200                           W222-UTUP-WDB6-PCB                             
055300                           W222-UTUP-UTIL-WDK6-PCB                        
055400                           W222-UTUP-UTIL-WDK7-PCB                        
055500                           W222-UTUP-UTIL-WDB6-PCB                        
055600                           .                                              
055700     SKIP1                                                                
055800     ENTRY 'DLITCBL' USING WDK6-PCB WDD9-PCB W222-AA-PCB WDK7-PCB         
055900                           ARTM-PCB WDL2-PCB W222-2501-PCB                
056000                           W222-WDB6R-PCB W222-WDK7R-PCB                  
056100                           WDF1-PCB WDP6-PCB WDB6-PCB                     
056200                           W222-WDD7-PCB  W222-WDK7E-PCB                  
056300                           W222-UTIL-WDK6-PCB                             
056400                           W222-UTIL-WDK7-PCB                             
056500                           W222-UTIL-WDB6-PCB                             
056600                           W222-UTUP-WDK7-PCB                             
056700                           W222-UTUP-WDB6-PCB                             
056800                           W222-UTUP-UTIL-WDK6-PCB                        
056900                           W222-UTUP-UTIL-WDK7-PCB                        
057000                           W222-UTUP-UTIL-WDB6-PCB                        
057100                           .                                              
057200     SKIP3                                                                
057300     PERFORM A-INITIERA                                                   
057400     SKIP1                                                                
057500     MOVE LAES-ARTIKEL-DATA TO LINK-KDCALL                                
057600     CALL W2214010 USING LINK-AREA WDK6-PCB WDD9-PCB                      
057700                          WDK7-PCB ARTM-PCB WDL2-PCB WDF1-PCB             
057800                          WDB6-PCB                                        
057900     SKIP1                                                                
058000     PERFORM UNTIL NOT(LINK-ANROP-OK)                                     
058100*  - - - - - - - -  FIX SEMESTERJUSTERING - - - - - - - - - - *           
058200         MOVE 28 TO SEMESTER-VECKA-START                                  
058300         MOVE 31 TO SEMESTER-VECKA-SLUT                                   
058400                                                                          
058500         MOVE LINK-TILEVDAG  (1)  TO WART-TILEVDAG (1)                    
058600         MOVE LINK-TILEVDAG  (2)  TO WART-TILEVDAG (2)                    
058700         MOVE LINK-TILEVDAG  (3)  TO WART-TILEVDAG (3)                    
058800         MOVE LINK-TILEVDAG  (4)  TO WART-TILEVDAG (4)                    
058900         MOVE LINK-TILEVDAG  (5)  TO WART-TILEVDAG (5)                    
059000                                                                          
059100       PERFORM B-LAES-DATA                                                
059200                                                                          
059300*---------FIX FÖR BESTÄMD VECKA ---------------****                       
059400                                                                          
059500       IF LINK-TIAAVV-AKT = 0951                                          
059600                                                                          
059700         IF LINK-KDHF > ZERO                                              
059800         OR LINK-IDLEVNR = '1002'                                         
059900           MOVE ZERO TO LINK-KVDAGAR-INLEV                                
060000         ELSE                                                             
060100           MOVE ZERO         TO W-KVDAGAR-FORP                            
060200*                   NEDAN:  LÅNGSAM MÅLNING                               
060300           IF  LINK-BEFT  = +44                                           
060400             MOVE +30 TO W-KVDAGAR-FORP                                   
060500           END-IF                                                         
060600                                                                          
060700           IF  LINK-BEFT  = +57                                           
060800             MOVE +15 TO W-KVDAGAR-FORP                                   
060900           END-IF                                                         
061000                                                                          
061100           IF  LINK-BEFT  = +46                                           
061200           OR  LINK-BEFT  = +49                                           
061300             MOVE +7 TO W-KVDAGAR-FORP                                    
061400           END-IF                                                         
061500*                       NEDAN: MÅLNING = FÖRP.KOD 41-49                   
061600           IF  LINK-BEFT  =  +6                                           
061700           OR  LINK-BEFT  =  +9                                           
061800           OR  LINK-BEFT  = +20                                           
061900           OR  LINK-BEFT  = +38                                           
062000           OR  LINK-BEFT  = +41                                           
062100           OR  LINK-BEFT  = +42                                           
062200           OR  LINK-BEFT  = +43                                           
062300           OR  LINK-BEFT  = +45                                           
062400           OR  LINK-BEFT  = +47                                           
062500           OR  LINK-BEFT  = +66                                           
062600           OR  LINK-BEFT  = +79                                           
062700             MOVE +5 TO W-KVDAGAR-FORP                                    
062800           END-IF                                                         
062900                                                                          
063000           IF  LINK-BEFT  = +10                                           
063100           OR (LINK-BEFT >= +12 AND <= +16)                               
063200           OR  LINK-BEFT  = +18                                           
063300           OR  LINK-BEFT  = +19                                           
063400           OR (LINK-BEFT >= +21 AND <= +28)                               
063500           OR (LINK-BEFT >= +32 AND <= +37)                               
063600           OR  LINK-BEFT  = +39                                           
063700           OR (LINK-BEFT >= +50 AND <= +54)                               
063800           OR  LINK-BEFT  = +56                                           
063900           OR (LINK-BEFT >= +58 AND <= +65)                               
064000           OR  LINK-BEFT  = +67                                           
064100           OR  LINK-BEFT  = +68                                           
064200           OR  LINK-BEFT  = +69                                           
064300           OR (LINK-BEFT >= +81 AND <= +89)                               
064400             MOVE +4 TO W-KVDAGAR-FORP                                    
064500           END-IF                                                         
064600                                                                          
064700           IF  LINK-BEFT  =  +7                                           
064800           OR  LINK-BEFT  =  +8                                           
064900           OR  LINK-BEFT  = +48                                           
065000           OR  LINK-BEFT  = +55                                           
065100           OR  LINK-BEFT  = +74                                           
065200           OR  LINK-BEFT  = +77                                           
065300           OR  LINK-BEFT  = +78                                           
065400           OR  LINK-BEFT  = +97                                           
065500           OR  LINK-BEFT  = +98                                           
065600             MOVE +3 TO W-KVDAGAR-FORP                                    
065700           END-IF                                                         
065800                                                                          
065900           IF  LINK-BEFT  = +29                                           
066000           OR  LINK-BEFT  = +30                                           
066100           OR  LINK-BEFT  = +31                                           
066200           OR  LINK-BEFT  = +73                                           
066300           OR  LINK-BEFT  = +90                                           
066400           OR  LINK-BEFT  = +91                                           
066500             MOVE +2 TO W-KVDAGAR-FORP                                    
066600           END-IF                                                         
066700                                                                          
066800           IF  LINK-BEFT <=  +5                                           
066900           OR  LINK-BEFT  = +11                                           
067000           OR  LINK-BEFT  = +17                                           
067100           OR  LINK-BEFT  = +70                                           
067200           OR  LINK-BEFT  = +71                                           
067300           OR  LINK-BEFT  = +72                                           
067400           OR  LINK-BEFT  = +80                                           
067500           OR  LINK-BEFT  = +92                                           
067600           OR  LINK-BEFT  = +94                                           
067700           OR  LINK-BEFT  = +96                                           
067800           OR  LINK-BEFT  = +99                                           
067900             MOVE +1 TO W-KVDAGAR-FORP                                    
068000           END-IF                                                         
068100                                                                          
068200           IF  LINK-BEFT  = +75                                           
068300           OR  LINK-BEFT  = +76                                           
068400           OR  LINK-BEFT  = +93                                           
068500           OR  LINK-BEFT  = +95                                           
068600              CONTINUE                                                    
068700*             -- FÅR INITIERINGSVÄRDET = ZERO                             
068800           END-IF                                                         
068900                                                                          
069000                                                                          
069100           MOVE W-KVDAGAR-FORP                                            
069200                             TO LINK-KVDAGAR-INLEV                        
069300                                                                          
069400           COMPUTE LINK-KVDAGAR-FFH =                                     
069500                   LINK-KVDAGAR-TT + LINK-KVDAGAR-INLEV                   
069600           COMPUTE LINK-KVVECKOR-FT ROUNDED =                             
069700                   LINK-KVVECKOR-LT + (LINK-KVDAGAR-FFH / 5)              
069800           COMPUTE LINK-KVVECKOR-BT ROUNDED =                             
069900                   LINK-KVVECKOR-AT + (LINK-KVDAGAR-FFH / 5) + 2          
070000         END-IF                                                           
070100       END-IF                                                             
070200                                                                          
070300*                                                                         
070400*-------- SLUT FIX  -----------------------------****                     
070500*                                                                         
070600       SKIP2                                                              
070700*******  FIX TILL DESS ATT 2131, 2132, W2170230 ÄNDRAS ****               
070800       IF LINK-KDHF > 0                                                   
070900         IF LINK-FLMANAT = NEJ                                            
071000           IF LINK-KDHF = 1                                               
071100             MOVE 16 TO LINK-KVVECKOR-AT                                  
071200           ELSE                                                           
071300             MOVE 16 TO LINK-KVVECKOR-AT                                  
071400           END-IF                                                         
071500         END-IF                                                           
071600       END-IF                                                             
071700*******  SLUT DENNA FIX                                                   
071800       SKIP2                                                              
071900       PERFORM C-DIVERSE-KONTROLLER-DEL1                                  
072000       PERFORM D-PUNKTBERAKNING                                           
072100       SKIP2                                                              
072200*******  FIX FÖR LP-3-MÄRKNING AV RENAULT-ARTIKLAR M FL                   
072300*                                 GÄLLER TILLS VIDARE                     
072400      IF LINK-KDLPSP NOT = 5                                              
072500       IF LINK-IDLEVNR = '3899' OR '3786' OR 'AH1KA'                      
072600           MOVE 3    TO LINK-KDLPSP                                       
072700           MOVE 9999 TO LINK-TILPSP                                       
072800       END-IF                                                             
072900      END-IF                                                              
073000*******  SLUT DENNA FIX                                                   
073100       SKIP2                                                              
073200       PERFORM O-DIVERSE-DATA                                             
073300       PERFORM M-ANSK-KONTROLL                                            
073400                                                                          
073500       IF  FLANSK-TRAEFF                                                  
073600                                                                          
073700         PERFORM G-EXTRALEVERANS-KONTROLL                                 
073800         PERFORM I-KONTROLL-MOT-KOEPPKT                                   
073900                                                                          
074000*        CONTINUE                                                         
074100       ELSE                                                               
074200         PERFORM E-BESTALLNINGS-KONTROLL                                  
074300         PERFORM F-DIVERSE-KONTROLLER-DEL2                                
074400         IF LINK-KDLPORS-TAB (1) = +18 OR                                 
074500            LINK-KDLPORS-TAB (2) = +18 OR                                 
074600            LINK-KDLPORS-TAB (3) = +18                                    
074700            CONTINUE                                                      
074800         ELSE                                                             
074900            PERFORM G-EXTRALEVERANS-KONTROLL                              
075000         END-IF                                                           
075100         PERFORM H-KONTROLL-KORR-LEVPLAN                                  
075200         PERFORM I-KONTROLL-MOT-KOEPPKT                                   
075300       END-IF                                                             
075400       PERFORM J-SKAPA-UTFILER                                            
075500       SKIP1                                                              
075600       PERFORM P-KONTROLL-EJ-AUT-GODK-FORSLAG                             
075700                                                                          
075800       PERFORM K-KONTROLL-UPPDATERING                                     
075900       SKIP1                                                              
076000       PERFORM N-KONTROLL-DISPDAT                                         
076100       SKIP1                                                              
076200       MOVE LAES-ARTIKEL-DATA TO LINK-KDCALL                              
076300       CALL W2214010 USING LINK-AREA WDK6-PCB WDD9-PCB                    
076400                            WDK7-PCB ARTM-PCB WDL2-PCB WDF1-PCB           
076500                            WDB6-PCB                                      
076600     END-PERFORM                                                          
076700     SKIP1                                                                
076800     PERFORM Z-AVSLUTA                                                    
076900     MOVE ZERO TO RETURN-CODE                                             
077000     GOBACK                                                               
077100     .                                                                    
077200     EJECT                                                                
077300 A-INITIERA SECTION.                                                      
077400     MOVE 'A-INITIERA              ' TO CURRENT-SECTION                   
077500                                                                          
077600     OPEN INPUT                                                           
077700                 W22131                                                   
077800                 W22133                                                   
077900                 W22135                                                   
078000                 W221LN                                                   
078100          OUTPUT W22142                                                   
078200                 W2214A                                                   
078300                 W22199                                                   
078400                 W22148                                                   
078500                 W22149                                                   
078600     SKIP1                                                                
078700     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
078800     MOVE D-AAR TO W-DATUM-AA W-DATUM2-AA AKT-DATUM-AA                    
078900     MOVE D-MAANAD TO W-DATUM2-MM                                         
079000     MOVE D-DAG    TO W-DATUM2-DD                                         
079100     MOVE D-VECKA TO W-DATUM-VV           AKT-DATUM-VV                    
079200     MOVE W-DATUM-AAVV TO LINK-TIAAVV-AKT                                 
079300                          LINK-TIAAVVD-AKT                                
079400     MULTIPLY 10 BY LINK-TIAAVVD-AKT                                      
079500                                                                          
079600* FYLLER DC-TABELLEN I W2214010 MED ALL IDDC-INFO                         
079700     MOVE LAES-WDB6-DC-INFO TO LINK-KDCALL                                
079800     CALL W2214010 USING LINK-AREA WDK6-PCB WDD9-PCB                      
079900                          WDK7-PCB ARTM-PCB WDL2-PCB WDF1-PCB             
080000                          WDB6-PCB                                        
080100                                                                          
080200                                                                          
080300     MOVE OPPNA TO LINK-KDCALL                                            
080400                                                                          
080500     CALL W2214040 USING LINK-AREA LNK2-AREA                              
080600     SKIP1                                                                
080700     PERFORM AB-LAES-LEVNR-KONTROLLVECKA                                  
080800     PERFORM AC-LAES-HAENDELSER                                           
080900     PERFORM AD-LAES-ANSKFIL                                              
081000     PERFORM AE-LAES-LEVNR-G                                              
081100                                                                          
081200                                                                          
081300     MOVE W-DATUM-AAMMDD TO DAT-I-TIDATUM                                 
081400     MOVE 'AAMMDD'       TO DAT-KDDATFORM                                 
081500     CALL WDATKONV USING DAT-KDDATFORM,                                   
081600                         DAT-I-TIDATUM,                                   
081700                         DAT-O-TIDATUM,                                   
081800                         DAT-KDSVAR                                       
081900     MOVE DAT-TISEKEL    TO W-HELA-DATUMET-SEKEL                          
082000     MOVE W-DATUM-AAMMDD TO W-HELA-DATUMET-AAMMDD                         
082100     MOVE DAT-KVVIPER    TO W-IDLPKOLL                                    
082200     MOVE ZERO           TO LINK-TIAAVV-NEXT-QF                           
082300     .                                                                    
082400     EJECT                                                                
082500 AB-LAES-LEVNR-KONTROLLVECKA SECTION.                                     
082600     MOVE 'AB-LAES-LEVNR-KONTROLLVECKA' TO CURRENT-SECTION                
082700                                                                          
082800     PERFORM ABA-LAES-W22133                                              
082900     SET LEVTAB-IX TO 1                                                   
083000     PERFORM UNTIL NOT(                                                   
083100        W22133-EOF = NEJ                                                  
083200     AND LEVTAB-IX NOT > LEVTAB-MAX)                                      
083300     SKIP1                                                                
083400         MOVE I33LEV-IDLEVNR TO LEVTAB-IDLEVNR (LEVTAB-IX)                
083500         MOVE I33LEV-IDLPKOLL TO LEVTAB-IDLPKOLL (LEVTAB-IX)              
083600         SET LEVTAB-IX UP BY 1                                            
083700     SKIP1                                                                
083800         PERFORM ABA-LAES-W22133                                          
083900     END-PERFORM                                                          
084000     SKIP1                                                                
084100     IF  LEVTAB-IX > LEVTAB-MAX                                           
084200     AND  W22133-EOF = NEJ                                                
084300         DISPLAY 'TABELL W221002 FULL'                                    
084400         MOVE 25 TO RETURN-CODE                                           
084500         PERFORM ABA-LAES-W22133                                          
084600     END-IF                                                               
084700     SKIP1                                                                
084800     PERFORM UNTIL NOT(                                                   
084900        LEVTAB-IX NOT > LEVTAB-MAX)                                       
085000         MOVE '99999' TO LEVTAB-IDLEVNR (LEVTAB-IX)                       
085100         MOVE 9 TO LEVTAB-IDLPKOLL (LEVTAB-IX)                            
085200         SET LEVTAB-IX UP BY 1                                            
085300     END-PERFORM                                                          
085400     .                                                                    
085500     EJECT                                                                
085600 ABA-LAES-W22133 SECTION.                                                 
085700     MOVE 'ABA-LAES-W22133            ' TO CURRENT-SECTION                
085800                                                                          
085900     READ W22133 INTO I33LEV-AREA                                         
086000          AT END                                                          
086100          MOVE JA TO W22133-EOF                                           
086200     END-READ                                                             
086300     SKIP1                                                                
086400     IF  W22133-EOF = NEJ                                                 
086500         MOVE 'W22133' TO POSTSUM-FDNAMN                                  
086600         MOVE 'W22140D3' TO POSTSUM-DDNAMN2                               
086700         MOVE '002' TO POSTSUM-TRANSTYP                                   
086800         CALL POSTSUM USING POSTSUM-PARM                                  
086900      END-IF                                                              
087000     .                                                                    
087100     EJECT                                                                
087200 AC-LAES-HAENDELSER SECTION.                                              
087300     MOVE 'AC-LAES-HAENDELSER         ' TO CURRENT-SECTION                
087400                                                                          
087500     PERFORM ACA-LAES-W22131                                              
087600     SET TAB2204-IX TO 1                                                  
087700     MOVE ZERO TO IX                                                      
087800     IF  W22131-EOF = NEJ                                                 
087900     MOVE I2204-IDARTNR TO   TAB2204-IDARTNR (TAB2204-IX)                 
088000     END-IF                                                               
088100     SKIP1                                                                
088200     PERFORM UNTIL NOT(                                                   
088300        W22131-EOF = NEJ                                                  
088400     AND (TAB2204-IX < TAB2204-MAX                                        
088500     OR   (TAB2204-IX = TAB2204-MAX                                       
088600      AND  I2204-IDARTNR = TAB2204-IDARTNR (TAB2204-IX))))                
088700     SKIP1                                                                
088800         IF  I2204-IDARTNR = TAB2204-IDARTNR (TAB2204-IX)                 
088900             ADD 1 TO IX                                                  
089000         ELSE                                                             
089100             IF  IX < 2                                                   
089200                 MOVE 99 TO TAB2204-KDLPORS-2 (TAB2204-IX)                
089300             END-IF                                                       
089400             IF  IX < 3                                                   
089500                 MOVE 99 TO TAB2204-KDLPORS-3 (TAB2204-IX)                
089600             END-IF                                                       
089700             MOVE 1 TO IX                                                 
089800             SET TAB2204-IX UP BY 1                                       
089900         END-IF                                                           
090000     SKIP1                                                                
090100         MOVE I2204-IDARTNR TO TAB2204-IDARTNR (TAB2204-IX)               
090200         EVALUATE IX                                                      
090300              WHEN 1                                                      
090400                     MOVE I2204-KDLPORS                                   
090500                     TO TAB2204-KDLPORS-1 (TAB2204-IX)                    
090600              WHEN 2                                                      
090700                     MOVE I2204-KDLPORS                                   
090800                     TO TAB2204-KDLPORS-2 (TAB2204-IX)                    
090900              WHEN 3                                                      
091000                     MOVE I2204-KDLPORS                                   
091100                     TO TAB2204-KDLPORS-3 (TAB2204-IX)                    
091200         END-EVALUATE                                                     
091300     SKIP1                                                                
091400         PERFORM ACA-LAES-W22131                                          
091500     END-PERFORM                                                          
091600     SKIP1                                                                
091700     IF  IX < 2                                                           
091800         MOVE 99 TO TAB2204-KDLPORS-2 (TAB2204-IX)                        
091900     END-IF                                                               
092000     IF  IX < 3                                                           
092100         MOVE 99 TO TAB2204-KDLPORS-3 (TAB2204-IX)                        
092200     END-IF                                                               
092300     SKIP1                                                                
092400     IF  TAB2204-IX = TAB2204-MAX                                         
092500     AND  W22131-EOF = NEJ                                                
092600         DISPLAY 'HÄNDELSE-TABELL FULL'                                   
092700         MOVE 25 TO RETURN-CODE                                           
092800         PERFORM ACA-LAES-W22131                                          
092900     END-IF                                                               
093000     SKIP1                                                                
093100     PERFORM UNTIL NOT(                                                   
093200        TAB2204-IX < TAB2204-MAX)                                         
093300         SET TAB2204-IX UP BY 1                                           
093400         MOVE 999999999 TO TAB2204-IDARTNR (TAB2204-IX)                   
093500         MOVE 99 TO TAB2204-KDLPORS-1 (TAB2204-IX)                        
093600                    TAB2204-KDLPORS-2 (TAB2204-IX)                        
093700                    TAB2204-KDLPORS-3 (TAB2204-IX)                        
093800     END-PERFORM                                                          
093900     .                                                                    
094000     EJECT                                                                
094100 ACA-LAES-W22131 SECTION.                                                 
094200     MOVE 'ACA-LAES-W22131            ' TO CURRENT-SECTION                
094300                                                                          
094400     READ W22131 INTO I2204-AREA                                          
094500          AT END                                                          
094600          MOVE JA TO W22131-EOF                                           
094700     END-READ                                                             
094800     SKIP1                                                                
094900     IF  W22131-EOF = NEJ                                                 
095000         MOVE 'W22131' TO POSTSUM-FDNAMN                                  
095100         MOVE 'W22140D2' TO POSTSUM-DDNAMN2                               
095200         MOVE '2204' TO POSTSUM-TRANSTYP                                  
095300         CALL POSTSUM USING POSTSUM-PARM                                  
095400      END-IF                                                              
095500                                                                          
095600     .                                                                    
095700     EJECT                                                                
095800 AD-LAES-ANSKFIL SECTION.                                                 
095900     MOVE 'AD-LAES-ANSKFIL            ' TO CURRENT-SECTION                
096000                                                                          
096100     SET TABANSK-IX   TO 1                                                
096200                                                                          
096300     PERFORM UNTIL NOT(                                                   
096400        TABANSK-IX < TABANSK-MAX                                          
096500          OR TABANSK-IX = TABANSK-MAX)                                    
096600        MOVE HIGH-VALUE  TO TABANSK-INGANG (TABANSK-IX)                   
096700        SET TABANSK-IX   UP BY 1                                          
096800     END-PERFORM                                                          
096900                                                                          
097000     PERFORM ADA-LAES-W22135                                              
097100     SET TABANSK-IX   TO 1                                                
097200     MOVE RAETT       TO WS-INDATA-TEST                                   
097300                                                                          
097400     PERFORM UNTIL NOT(                                                   
097500        (W22135-EOF = NEJ)                                                
097600     AND (TABANSK-IX < TABANSK-MAX OR                                     
097700          TABANSK-IX = TABANSK-MAX))                                      
097800                                                                          
097900        IF SUB-IDANSK NOT NUMERIC                                         
098000           MOVE FEL       TO WS-INDATA-TEST                               
098100        END-IF                                                            
098200                                                                          
098300        IF SUB-TIAAVV-FOM NOT NUMERIC                                     
098400           MOVE FEL       TO WS-INDATA-TEST                               
098500        END-IF                                                            
098600                                                                          
098700        IF SUB-TIAAVV-TOM = SPACE                                         
098800           MOVE SUB-TIAAVV-FOM   TO SUB-TIAAVV-TOM                        
098900        END-IF                                                            
099000                                                                          
099100        IF SUB-TIAAVV-TOM  NUMERIC                                        
099200           CONTINUE                                                       
099300        ELSE                                                              
099400           MOVE FEL       TO WS-INDATA-TEST                               
099500        END-IF                                                            
099600                                                                          
099700        IF WS-INDATA-RAETT                                                
099800                                                                          
099900            MOVE W-DATUM-AAVV      TO TMP1-YYWW                           
100000            MOVE SUB-TIAAVV-FOM    TO W-NUM4                              
100100            MOVE W-NUM4            TO TMP2-YYWW                           
100200            MOVE SUB-TIAAVV-TOM    TO W-NUM4                              
100300            MOVE W-NUM4            TO TMP3-YYWW                           
100400            PERFORM WY2000Q3                                              
100500           IF (W-DAT-AAVV = SUB-TIAAVV-FOM)                               
100600           OR (TMP1-YYWW  > TMP2-YYWW      AND                            
100700               TMP1-YYWW  < TMP3-YYWW     )                               
100800           OR (W-DAT-AAVV = SUB-TIAAVV-TOM)                               
100900              PERFORM ADB-LADDA-TABANSK                                   
101000           ELSE                                                           
101100              DISPLAY 'ANSKAFFARE EJ LADDAD  ' SUB-IDANSK                 
101200                       SUB-TIAAVV-FOM SUB-TIAAVV-TOM                      
101300           END-IF                                                         
101400                                                                          
101500        ELSE                                                              
101600           DISPLAY 'ANSKAFFARE FORM. FEL  ' SUB-IDANSK                    
101700                    SUB-TIAAVV-FOM SUB-TIAAVV-TOM                         
101800        END-IF                                                            
101900                                                                          
102000        PERFORM ADA-LAES-W22135                                           
102100        MOVE RAETT       TO WS-INDATA-TEST                                
102200     END-PERFORM                                                          
102300                                                                          
102400     IF TABANSK-IX > TABANSK-MAX                                          
102500        DISPLAY 'ANSKAFFARE-TABELL FULL'                                  
102600     END-IF                                                               
102700                                                                          
102800     .                                                                    
102900     EJECT                                                                
103000 ADA-LAES-W22135 SECTION.                                                 
103100     MOVE 'ADA-LAES-W22135            ' TO CURRENT-SECTION                
103200                                                                          
103300     READ W22135 INTO SUB-AREA                                            
103400          AT END                                                          
103500          MOVE JA TO W22135-EOF                                           
103600     END-READ                                                             
103700     SKIP1                                                                
103800     IF  W22135-EOF = NEJ                                                 
103900         MOVE 'W22135' TO POSTSUM-FDNAMN                                  
104000         MOVE 'W22140DC' TO POSTSUM-DDNAMN2                               
104100         MOVE 'ANSK' TO POSTSUM-TRANSTYP                                  
104200         CALL POSTSUM USING POSTSUM-PARM                                  
104300      END-IF                                                              
104400                                                                          
104500     .                                                                    
104600     EJECT                                                                
104700 ADB-LADDA-TABANSK SECTION.                                               
104800     MOVE 'ADB-LADDA-TABANSK          ' TO CURRENT-SECTION                
104900                                                                          
105000     IF SUB-IDANSK NOT = WS-SUB-IDANSK                                    
105100        MOVE SUB-IDANSK  TO TABANSK-IDANSK (TABANSK-IX)                   
105200        MOVE SUB-IDANSK  TO WS-SUB-IDANSK                                 
105300        SET TABANSK-IX UP BY 1                                            
105400     END-IF                                                               
105500                                                                          
105600     .                                                                    
105700     EJECT                                                                
105800 AE-LAES-LEVNR-G SECTION.                                                 
105900     MOVE 'AE-LAES-LEVNR-G            ' TO CURRENT-SECTION                
106000                                                                          
106100     PERFORM AEA-LAES-W221LN                                              
106200     MOVE +1 TO IX-LN                                                     
106300     PERFORM UNTIL W221LN-EOF = JA OR                                     
106400                   IX-LN > LN-TAB-MAX                                     
106500                                                                          
106600         MOVE INLN-IDLEVNR      TO LN-TAB-IDLEVNR      (IX-LN)            
106700         MOVE INLN-IDANSK-START TO LN-TAB-IDANSK-START (IX-LN)            
106800         MOVE INLN-IDANSK-SLUT  TO LN-TAB-IDANSK-SLUT  (IX-LN)            
106900         ADD +1 TO IX-LN                                                  
107000                                                                          
107100         PERFORM AEA-LAES-W221LN                                          
107200     END-PERFORM                                                          
107300                                                                          
107400     IF  IX-LN > LN-TAB-MAX                                               
107500     AND  W221LN-EOF = NEJ                                                
107600         DISPLAY 'TABELL W221LN  FULL'                                    
107700         MOVE 26 TO RETURN-CODE                                           
107800     END-IF                                                               
107900                                                                          
108000     SUBTRACT +1 FROM IX-LN                                               
108100     MOVE IX-LN TO LN-TAB-MAX                                             
108200     .                                                                    
108300     EJECT                                                                
108400 AEA-LAES-W221LN SECTION.                                                 
108500     MOVE 'AEA-LAES-W221LN            ' TO CURRENT-SECTION                
108600                                                                          
108700     READ W221LN INTO INLN-AREA                                           
108800          AT END                                                          
108900          MOVE JA TO W221LN-EOF                                           
109000     END-READ                                                             
109100                                                                          
109200     IF  W221LN-EOF = NEJ                                                 
109300         MOVE 'W221LN' TO POSTSUM-FDNAMN                                  
109400         MOVE 'W22140DA' TO POSTSUM-DDNAMN2                               
109500         MOVE 'LEVN' TO POSTSUM-TRANSTYP                                  
109600         CALL POSTSUM USING POSTSUM-PARM                                  
109700      END-IF                                                              
109800     .                                                                    
109900     EJECT                                                                
110000 B-LAES-DATA SECTION.                                                     
110100     MOVE 'B-LAES-DATA                ' TO CURRENT-SECTION                
110200                                                                          
110300     PERFORM BA-LAES-BEHOVS-TABELL                                        
110400     PERFORM BB-LAES-LEVERANTOER                                          
110500     PERFORM BC-SOEK-I-ORSAKSTABELL                                       
110600     SKIP3                                                                
110700     .                                                                    
110800 BA-LAES-BEHOVS-TABELL SECTION.                                           
110900     MOVE 'BA-LAES-BEHOVS-TABELL      ' TO CURRENT-SECTION                
111000                                                                          
111100     MOVE LINK-IDARTNR TO LNK2-IDARTNR                                    
111200     MOVE SPACE        TO LNK2-IDDC                                       
111300     MOVE SEP-SATS-TPO-LEV-SDC-NDC  TO LNK2-KDBEHOV                       
111400     MOVE LINK-KVVECKOR-BT TO LNK2-KVVECKOR-BEHOV                         
111500     ADD 62 TO LNK2-KVVECKOR-BEHOV                                        
111600     IF  LNK2-KVVECKOR-BEHOV > 156                                        
111700         MOVE 156 TO LNK2-KVVECKOR-BEHOV                                  
111800     END-IF                                                               
111900     MOVE LINK-TIAAVV-AKT TO LNK2-TIAAVV-AKTUELL                          
112000                             LNK2-TIBEHOV-START                           
112100     MOVE 1 TO W-ANTAL-VECKOR                                             
112200     CALL W009VADD USING LNK2-TIBEHOV-START  W-ANTAL-VECKOR               
112300     MOVE +6  TO LNK2-TID-AKTUELL                                         
112400     MOVE NEJ                 TO LNK2-FLINKLDIRLEV                        
112500*                                                                         
112600     IF LINK-KDERS-UTG = +0                                               
112700        CALL W22222 USING LNK2-AREA W222-AA-PCB WDK7-PCB                  
112800                                    ARTM-PCB    W222-2501-PCB             
112900                                    W222-WDB6R-PCB                        
113000                                    W222-WDK7R-PCB                        
113100                                    WDB6-PCB    W222-WDD7-PCB             
113200                                    W222-WDK7E-PCB                        
113300                                    W222-UTIL-WDK6-PCB                    
113400                                    W222-UTIL-WDK7-PCB                    
113500                                    W222-UTIL-WDB6-PCB                    
113600                                    W222-UTUP-WDK7-PCB                    
113700                                    W222-UTUP-WDB6-PCB                    
113800                                    W222-UTUP-UTIL-WDK6-PCB               
113900                                    W222-UTUP-UTIL-WDK7-PCB               
114000                                    W222-UTUP-UTIL-WDB6-PCB               
114100                                                                          
114200        IF LNK2-ANROP-FEL                                                 
114300           PERFORM S04-NOLLA-W22222                                       
114400        END-IF                                                            
114500     ELSE                                                                 
114600        PERFORM S04-NOLLA-W22222                                          
114700     END-IF                                                               
114800*                                                                         
114900     IF LINK-KDHF > ZERO                                                  
115000       OR LINK-IDARTNR = 271794 OR 271788                                 
115100        MOVE LINK-IDLEVNR    TO OLIKA-LEV                                 
115200        IF NOT SATS-LEVNR                                                 
115300            PERFORM BAA-JUSTERA-FOR-SEMESTER                              
115400        END-IF                                                            
115500     END-IF                                                               
115600     MOVE LINK-IDLEVNR     TO OLIKA-LEV                                   
115700     IF  ALLISON-LEVNR OR EATON-LEVNR OR SOMA-LEVNR                       
115800         OR TRW-LEVNR                                                     
115900         PERFORM BAB-JUST-BEHOV-VW-RENAULT                                
116000     END-IF                                                               
116100     .                                                                    
116200     EJECT                                                                
116300 BAA-JUSTERA-FOR-SEMESTER SECTION.                                        
116400     MOVE 'BAA-JUSTERA-FOR-SEMESTER   ' TO CURRENT-SECTION                
116500                                                                          
116600******************************************************************        
116700*                                                                *        
116800*    BEHOV UNDER SEMESTERN FLYTTAS FÖRE SEMESTERN                *        
116900*                                                                *        
117000* O B S   KOPIA AV SEKTIONEN FINNS I PROGRAM W2215000.           *        
117100*         GLÖM EJ ATT UPPDATERA ÄVEN DETTA PROGRAM.      O B S   *        
117200*                                                                *        
117300******************************************************************        
117400                                                                          
117500** FIX FÖR SKÖVDE   ****                                                  
117600     IF LINK-IDARTNR = 271794 OR 271788                                   
117700        MOVE 29 TO SEMESTER-VECKA-START                                   
117800        MOVE 30 TO SEMESTER-VECKA-SLUT                                    
117900     END-IF                                                               
118000** FIX SLUT                                                               
118100     IF LINK-KDHF = ZERO                                                  
118200        COMPUTE W-KVDAGAR-FFH =                                           
118300        LINK-KVDAGAR-TT + LINK-KVDAGAR-INLEV                              
118400     ELSE                                                                 
118500        MOVE +0 TO W-KVVECKOR-FFH                                         
118600                   W-KVDAGAR-FFH                                          
118700     END-IF                                                               
118800                                                                          
118900     COMPUTE W-SEMESTER-VV-START ROUNDED =                                
119000             SEMESTER-VECKA-START + (W-KVDAGAR-FFH / 5)                   
119100     COMPUTE W-SEMESTER-VV-SLUT  ROUNDED =                                
119200             SEMESTER-VECKA-SLUT  + (W-KVDAGAR-FFH / 5)                   
119300                                                                          
119400     MOVE LNK2-TIBEHOV-START TO W-DATUM-AAVV                              
119500     SUBTRACT W-DATUM-VV FROM W-SEMESTER-VV-START                         
119600     SUBTRACT W-DATUM-VV FROM W-SEMESTER-VV-SLUT                          
119700     ADD 1 TO W-SEMESTER-VV-START                                         
119800              W-SEMESTER-VV-SLUT                                          
119900     SKIP1                                                                
120000     PERFORM UNTIL NOT(                                                   
120100        W-SEMESTER-VV-START < LNK2-KVVECKOR-BEHOV)                        
120200         MOVE W-SEMESTER-VV-START TO IX                                   
120300                                     IX-SUM                               
120400         SUBTRACT 1 FROM IX-SUM                                           
120500         PERFORM UNTIL NOT(                                               
120600            IX NOT > W-SEMESTER-VV-SLUT                                   
120700         AND IX NOT > LNK2-KVVECKOR-BEHOV)                                
120800           IF IX-SUM > ZERO                                               
120900             ADD LNK2-KVBEHOV-VECKA (IX)                                  
121000                             TO LNK2-KVBEHOV-VECKA (IX-SUM)               
121100           END-IF                                                         
121200           IF IX > ZERO                                                   
121300             MOVE ZERO TO LNK2-KVBEHOV-VECKA (IX)                         
121400           END-IF                                                         
121500           ADD 1 TO IX                                                    
121600         END-PERFORM                                                      
121700         ADD 52 TO W-SEMESTER-VV-START                                    
121800                   W-SEMESTER-VV-SLUT                                     
121900     END-PERFORM                                                          
122000     .                                                                    
122100     EJECT                                                                
122200 BAB-JUST-BEHOV-VW-RENAULT SECTION.                                       
122300     MOVE 'BAB-JUST-BEHOV-VW-RENAULT  ' TO CURRENT-SECTION                
122400                                                                          
122500******************************************************************        
122600*                                                                *        
122700*    OM VW/RENAULT/ALLISON/EATON/SOMA/TRW SKALL ALLA BEHOV       *        
122800*    LIGGA I BEHOVSVECKOR                                        *        
122900*    SÅ ATT RESTEN (BEHOVSVECKA/4) = FRAMFÖRHÅLLNING             *        
123000*                                                                *        
123100* O B S   KOPIA AV SEKTIONEN FINNS I PROGRAM W2215000.           *        
123200*         GLÖM EJ ATT UPPDATERA ÄVEN DETTA PROGRAM.      O B S   *        
123300*                                                                *        
123400******************************************************************        
123500                                                                          
123600                                                                          
123700     COMPUTE W-KVDAGAR-FFH =                                              
123800             LINK-KVDAGAR-TT + LINK-KVDAGAR-INLEV                         
123900                                                                          
124000     COMPUTE W-KVVECKOR-FFH ROUNDED =                                     
124100            (LINK-KVDAGAR-FFH / 5)                                        
124200                                                                          
124300     PERFORM UNTIL W-KVVECKOR-FFH < 4                                     
124400         SUBTRACT 4 FROM W-KVVECKOR-FFH                                   
124500     END-PERFORM                                                          
124600                                                                          
124700     MOVE LNK2-TIBEHOV-START TO W-DATUM-AAVV                              
124800     DIVIDE W-DATUM-VV BY 4 GIVING W-ANTAL                                
124900     MULTIPLY 4 BY W-ANTAL                                                
125000     SUBTRACT W-ANTAL FROM W-DATUM-VV                                     
125100     SKIP1                                                                
125200     MOVE 1 TO IX-SUM                                                     
125300     SUBTRACT W-DATUM-VV FROM IX-SUM                                      
125400     ADD W-KVVECKOR-FFH TO IX-SUM                                         
125500     PERFORM UNTIL NOT(                                                   
125600        IX-SUM < 1)                                                       
125700        ADD 4 TO IX-SUM                                                   
125800     END-PERFORM                                                          
125900     SKIP1                                                                
126000     MOVE 1 TO IX                                                         
126100     PERFORM UNTIL NOT(                                                   
126200        IX < IX-SUM)                                                      
126300        ADD LNK2-KVBEHOV-VECKA (IX) TO LNK2-KVBEHOV-DESSUTOM              
126400        SUBTRACT LNK2-KVBEHOV-VECKA (IX)                                  
126500                                     FROM LNK2-KVBEHOV-SUMMA              
126600        MOVE ZERO TO LNK2-KVBEHOV-VECKA (IX)                              
126700        ADD 1 TO IX                                                       
126800     END-PERFORM                                                          
126900     SKIP1                                                                
127000     PERFORM UNTIL NOT(                                                   
127100        IX-SUM < LNK2-KVVECKOR-BEHOV)                                     
127200         MOVE IX-SUM TO IX                                                
127300         ADD 1 TO IX                                                      
127400         PERFORM UNTIL NOT(                                               
127500            IX < IX-SUM + 4)                                              
127600             ADD LNK2-KVBEHOV-VECKA (IX)                                  
127700                             TO LNK2-KVBEHOV-VECKA (IX-SUM)               
127800             MOVE ZERO TO LNK2-KVBEHOV-VECKA (IX)                         
127900             ADD 1 TO IX                                                  
128000         END-PERFORM                                                      
128100         ADD 4 TO IX-SUM                                                  
128200     END-PERFORM                                                          
128300     .                                                                    
128400     EJECT                                                                
128500 BB-LAES-LEVERANTOER SECTION.                                             
128600     MOVE 'BB-LAES-LEVERANTOER        ' TO CURRENT-SECTION                
128700                                                                          
128800     MOVE LINK-IDLEVNR TO W-IDLEVNR                                       
128900     MOVE LAES-LEVERANTOER TO LINK-KDCALL                                 
129000     CALL W2214010 USING LINK-AREA WDK6-PCB WDD9-PCB                      
129100                          WDK7-PCB ARTM-PCB WDL2-PCB WDF1-PCB             
129200                          WDB6-PCB                                        
129300     MOVE ZERO TO LINK-KVANTAL-LEVPLAN                                    
129400                  LINK-KVAL-BR                                            
129500                  LINK-KVBR-TOT                                           
129600                  LINK-KVOEKORR                                           
129700                  W-KVBR                                                  
129800                  LINK-KVBEST-PL                                          
129900     SKIP1                                                                
130000*  SEG-LEVEL = 02  MEDFÖR ATT KOPPLING TILL LP FINNS   ******             
130100*                         MEN BARN SAKNAS              ******             
130200*                                                      ******             
130300     IF LINK-ANROP-FEL                                                    
130400       IF WDD9-SEG-LEVEL = '01'                                           
130500        PERFORM BBA-BORTTAG-KOPPLING-LP                                   
130600       END-IF                                                             
130700     ELSE                                                                 
130800*                                                                         
130900       PERFORM UNTIL NOT(                                                 
131000          LINK-ANROP-OK)                                                  
131100                                                                          
131200          ADD LINK-KVBR TO LINK-KVBR-TOT                                  
131300          ADD 1 TO LINK-KVANTAL-LEVPLAN                                   
131400          IF  LINK-KVBR > ZERO                                            
131500              ADD 1 TO LINK-KVAL-BR                                       
131600          END-IF                                                          
131700          IF  LINK-IDLEVNR = W-IDLEVNR                                    
131800              MOVE LINK-KVBR TO W-KVBR                                    
131900              MOVE LAES-OMSPEC TO LINK-KDCALL                             
132000              CALL W2214010 USING LINK-AREA WDK6-PCB WDD9-PCB             
132100                                   WDK7-PCB ARTM-PCB WDL2-PCB             
132200                                   WDF1-PCB WDB6-PCB                      
132300                                                                          
132400          END-IF                                                          
132500          MOVE LAES-LEVERANTOER TO LINK-KDCALL                            
132600          CALL W2214010 USING LINK-AREA WDK6-PCB WDD9-PCB                 
132700                               WDK7-PCB ARTM-PCB WDL2-PCB                 
132800                               WDF1-PCB WDB6-PCB                          
132900       END-PERFORM                                                        
133000*                                                                         
133100     END-IF                                                               
133200     SKIP1                                                                
133300     MOVE W-KVBR TO LINK-KVBR                                             
133400     MOVE W-IDLEVNR TO LINK-IDLEVNR                                       
133500     SKIP1                                                                
133600     MOVE LINK-AREA TO WLINK-AREA                                         
133700     SKIP1                                                                
133800     MOVE +0 TO LINK-KDLPORS-TAB (1)                                      
133900     MOVE +0 TO LINK-KDLPORS-TAB (2)                                      
134000     MOVE +0 TO LINK-KDLPORS-TAB (3)                                      
134100     .                                                                    
134200     EJECT                                                                
134300 BBA-BORTTAG-KOPPLING-LP SECTION.                                         
134400     MOVE 'BBA-BORTTAG-KOPPLING-LP '  TO CURRENT-SECTION                  
134500                                                                          
134600     PERFORM S100-NOLLA-W2214A-AREA                                       
134700                                                                          
134800     MOVE BORTTAG-KOPPLING-LP-4A TO U4A-UPD-IDPTYP                        
134900     MOVE LINK-IDARTNR           TO U4A-UPD-IDARTNR                       
135000     MOVE WC-CDC-SE              TO U4A-UPD-IDDC                          
135100     MOVE LINK-IDLEVNR           TO U4A-UPD-IDLEVNR                       
135200                                                                          
135300     PERFORM S05-SKRIV-W2214A                                             
135400                                                                          
135500     MOVE JA TO LINK-FLJANEJ-ANROP                                        
135600     .                                                                    
135700     EJECT                                                                
135800 BC-SOEK-I-ORSAKSTABELL SECTION.                                          
135900     MOVE 'BC-SOEK-I-ORSAKSTABELL     ' TO CURRENT-SECTION                
136000                                                                          
136100     SEARCH ALL TAB2204-INGANG                                            
136200     WHEN                                                                 
136300       TAB2204-IDARTNR (TAB2204-IX) = LINK-IDARTNR                        
136400         MOVE TAB2204-KDLPORS-1 (TAB2204-IX)                              
136500                  TO LINK-KDLPORS-TAB (1)                                 
136600         MOVE TAB2204-KDLPORS-2 (TAB2204-IX)                              
136700                  TO LINK-KDLPORS-TAB (2)                                 
136800         MOVE TAB2204-KDLPORS-3 (TAB2204-IX)                              
136900                  TO LINK-KDLPORS-TAB (3)                                 
137000     END-SEARCH                                                           
137100     SKIP1                                                                
137200     IF  LINK-KDLPORS-TAB (1) = 99                                        
137300         MOVE ZERO TO LINK-KDLPORS-TAB (1)                                
137400     END-IF                                                               
137500     IF  LINK-KDLPORS-TAB (2) = 99                                        
137600         MOVE ZERO TO LINK-KDLPORS-TAB (2)                                
137700     END-IF                                                               
137800     IF  LINK-KDLPORS-TAB (3) = 99                                        
137900         MOVE ZERO TO LINK-KDLPORS-TAB (3)                                
138000     END-IF                                                               
138100     .                                                                    
138200     EJECT                                                                
138300 C-DIVERSE-KONTROLLER-DEL1 SECTION.                                       
138400     MOVE 'C-DIVERSE-KONTROLLER-DEL1  ' TO CURRENT-SECTION                
138500                                                                          
138600     MOVE -3 TO W-ANTAL-VECKOR                                            
138700     MOVE LINK-TIAAVV-AKT TO W-DATUM-GRAENS                               
138800     CALL W009VADD USING W-DATUM-GRAENS W-ANTAL-VECKOR                    
138900     SKIP1                                                                
139000     MOVE LINK-TIOMSPEC    TO TMP1-YYWW                                   
139100     MOVE W-DATUM-GRAENS   TO TMP2-YYWW                                   
139200     PERFORM WY2000P3                                                     
139300     IF (LINK-KDLPSP = 5                                                  
139400     AND TMP1-YYWW NOT > TMP2-YYWW) OR                                    
139500     (LINK-KDLPSP = 5                                                     
139600     AND LINK-TIOMSPEC  = 0103) OR                                        
139700     (LINK-KDLPSP = 5                                                     
139800     AND LINK-KDERS(1) > 20)                                              
139900         MOVE 71                     TO W-KDLPORS                         
140000         PERFORM S03-ADD-TILL-ORSAKSTABELL                                
140100                                                                          
140200         MOVE +0 TO WLINK-KDLPORS-TAB (1)                                 
140300         MOVE +0 TO WLINK-KDLPORS-TAB (2)                                 
140400         MOVE +0 TO WLINK-KDLPORS-TAB (3)                                 
140500                                                                          
140600         MOVE ZERO TO LINK-KDLPSP                                         
140700                      LINK-KVBEST-PL                                      
140800     SKIP1                                                                
140900         PERFORM CA-BORTTAG-OMSPEC                                        
141000     SKIP1                                                                
141100         MOVE 1 TO LINK3-KDAVROP                                          
141200         MOVE LINK-IDARTNR TO LINK3-IDARTNR                               
141300         PERFORM CB-BORTTAG-AVROP                                         
141400     END-IF                                                               
141500     SKIP1                                                                
141600     MOVE LINK-TILPSP       TO TMP1-YYWW                                  
141700     MOVE LINK-TIAAVV-AKT   TO TMP2-YYWW                                  
141800     PERFORM WY2000P3                                                     
141900     IF  (LINK-KDLPSP = 3                                                 
142000     OR   LINK-KDLPSP = 6)                                                
142100     AND TMP1-YYWW <= TMP2-YYWW                                           
142200         MOVE ZERO TO LINK-KDLPSP                                         
142300     END-IF                                                               
142400     SKIP2                                                                
142500     IF LINK-KDERS (1) > 20                                               
142600         MOVE +0 TO LINK-KVPB-SEP (1)                                     
142700     END-IF                                                               
142800                                                                          
142900     IF LINK-DAPBPLAN < W-HELA-DATUMET                                    
143000        MOVE ZERO TO LINK-DAPBPLAN                                        
143100     END-IF                                                               
143200                                                                          
143300     IF LINK-DASEASON < W-HELA-DATUMET                                    
143400        MOVE ZERO TO LINK-DASEASON                                        
143500        MOVE ZERO TO LINK-RESEASON-PLAN (1)                               
143600        MOVE ZERO TO LINK-RESEASON-PLAN (2)                               
143700        MOVE ZERO TO LINK-RESEASON-PLAN (3)                               
143800        MOVE ZERO TO LINK-RESEASON-PLAN (4)                               
143900        MOVE ZERO TO LINK-RESEASON-PLAN (5)                               
144000        MOVE ZERO TO LINK-RESEASON-PLAN (6)                               
144100        MOVE ZERO TO LINK-RESEASON-PLAN (7)                               
144200        MOVE ZERO TO LINK-RESEASON-PLAN (8)                               
144300        MOVE ZERO TO LINK-RESEASON-PLAN (9)                               
144400        MOVE ZERO TO LINK-RESEASON-PLAN (10)                              
144500        MOVE ZERO TO LINK-RESEASON-PLAN (11)                              
144600        MOVE ZERO TO LINK-RESEASON-PLAN (12)                              
144700     END-IF                                                               
144800     .                                                                    
144900     EJECT                                                                
145000 CA-BORTTAG-OMSPEC SECTION.                                               
145100     MOVE 'CA-BORTTAG-OMSPEC '  TO CURRENT-SECTION                        
145200                                                                          
145300     PERFORM S100-NOLLA-W2214A-AREA                                       
145400                                                                          
145500     MOVE BORTTAG-OMSPEC-4A  TO U4A-UPD-IDPTYP                            
145600     MOVE WC-CDC-SE          TO U4A-UPD-IDDC                              
145700     MOVE LINK-IDLEVNR       TO U4A-UPD-IDLEVNR                           
145800     MOVE LINK-IDARTNR       TO U4A-UPD-IDARTNR                           
145900                                                                          
146000     PERFORM S05-SKRIV-W2214A                                             
146100     .                                                                    
146200     EJECT                                                                
146300 CB-BORTTAG-AVROP SECTION.                                                
146400     MOVE 'CB-BORTTAG-AVROP '  TO CURRENT-SECTION                         
146500                                                                          
146600     PERFORM S100-NOLLA-W2214A-AREA                                       
146700                                                                          
146800     MOVE BORTTAG-AVROP-4A   TO U4A-UPD-IDPTYP                            
146900     MOVE WC-CDC-SE          TO U4A-UPD-IDDC                              
147000     MOVE LINK-IDLEVNR       TO U4A-UPD-IDLEVNR                           
147100     MOVE LINK-IDARTNR       TO U4A-UPD-IDARTNR                           
147200     MOVE +1                 TO U4A-UPD-KDAVROP                           
147300                                                                          
147400     PERFORM S05-SKRIV-W2214A                                             
147500     .                                                                    
147600     EJECT                                                                
147700 D-PUNKTBERAKNING  SECTION.                                               
147800     MOVE 'D-PUNKTBERAKNING           ' TO CURRENT-SECTION                
147900                                                                          
148000******************************************************************        
148100*                                                                *        
148200*    PUNKTBERÄKNING                                              *        
148300*                                                                *        
148400******************************************************************        
148500     IF LINK-DAPBPLAN = ZERO                                              
148600**           TAG FRAM ETT MASKINELLT KVPB-PLAN  **                        
148700        MOVE LINK-IDARTNR TO PBTO-IDARTNR                                 
148800        CALL W222PBTO USING PBTO-W222PBTO                                 
148900                            W222-AA-PCB WDK7-PCB                          
149000                            ARTM-PCB    W222-2501-PCB                     
149100                            W222-WDB6R-PCB W222-WDK7R-PCB                 
149200                            WDB6-PCB    W222-WDD7-PCB                     
149300                            W222-WDK7E-PCB                                
149400                            W222-UTIL-WDK6-PCB                            
149500                            W222-UTIL-WDK7-PCB                            
149600                            W222-UTIL-WDB6-PCB                            
149700                            W222-UTUP-WDK7-PCB                            
149800                            W222-UTUP-WDB6-PCB                            
149900                            W222-UTUP-UTIL-WDK6-PCB                       
150000                            W222-UTUP-UTIL-WDK7-PCB                       
150100                            W222-UTUP-UTIL-WDB6-PCB                       
150200                                                                          
150300        IF PBTO-KDSVAR = JA                                               
150400           MOVE PBTO-KVPB-PLAN TO LINK-KVPB-PLAN                          
150500        ELSE                                                              
150600           MOVE ZERO           TO LINK-KVPB-PLAN                          
150700        END-IF                                                            
150800     END-IF                                                               
150900                                                                          
151000     MOVE LINK-TIFINLV       TO TMP1-YYWWD                                
151100     MOVE LINK-TIAAVVD-AKT   TO TMP2-YYWWD                                
151200     PERFORM WY2000P2                                                     
151300     IF TMP1-YYWWD > TMP2-YYWWD                                           
151400        MOVE LNK2-AREA TO SPAR-AREA                                       
151500        MOVE SATS-TPO-LEV-SDC-NDC  TO LNK2-KDBEHOV                        
151600        MOVE +6  TO LNK2-TID-AKTUELL                                      
151700        MOVE NEJ                   TO LNK2-FLINKLDIRLEV                   
151800                                                                          
151900        IF LINK-KDERS-UTG = +0                                            
152000           CALL W22222 USING LNK2-AREA W222-AA-PCB WDK7-PCB               
152100                                       ARTM-PCB W222-2501-PCB             
152200                                       W222-WDB6R-PCB                     
152300                                       W222-WDK7R-PCB                     
152400                                       WDB6-PCB W222-WDD7-PCB             
152500                                       W222-WDK7E-PCB                     
152600                                       W222-UTIL-WDK6-PCB                 
152700                                       W222-UTIL-WDK7-PCB                 
152800                                       W222-UTIL-WDB6-PCB                 
152900                                       W222-UTUP-WDK7-PCB                 
153000                                       W222-UTUP-WDB6-PCB                 
153100                                       W222-UTUP-UTIL-WDK6-PCB            
153200                                       W222-UTUP-UTIL-WDK7-PCB            
153300                                       W222-UTUP-UTIL-WDB6-PCB            
153400                                                                          
153500           IF LNK2-ANROP-FEL                                              
153600              PERFORM S04-NOLLA-W22222                                    
153700           END-IF                                                         
153800        ELSE                                                              
153900           PERFORM S04-NOLLA-W22222                                       
154000        END-IF                                                            
154100                                                                          
154200        CALL W2214030 USING LINK-AREA LNK2-AREA WDP6-PCB WDF1-PCB         
154300                                                                          
154400        MOVE SPAR-AREA TO LNK2-AREA                                       
154500     ELSE                                                                 
154600        CALL W2214030 USING LINK-AREA LNK2-AREA WDP6-PCB WDF1-PCB         
154700     END-IF                                                               
154800     .                                                                    
154900     EJECT                                                                
155000 E-BESTALLNINGS-KONTROLL SECTION.                                         
155100     MOVE 'E-BESTALLNINGS-KONTROLL    ' TO CURRENT-SECTION                
155200                                                                          
155300******************************************************************        
155400*                                                                *        
155500*    BESTÄLLNINGS KONTROLL                                       *        
155600*                                                                *        
155700******************************************************************        
155800     MOVE LINK-IDLEVNR     TO OLIKA-LEV                                   
155900     IF NOT SATS-LEVNR                                                    
156000     MOVE BEARBETA  TO LINK-KDCALL                                        
156100     CALL W2214040 USING LINK-AREA LNK2-AREA                              
156200     END-IF                                                               
156300     .                                                                    
156400     EJECT                                                                
156500 F-DIVERSE-KONTROLLER-DEL2 SECTION.                                       
156600     MOVE 'F-DIVERSE-KONTROLLER-DEL2  ' TO CURRENT-SECTION                
156700                                                                          
156800******************************************************************        
156900*                                                                *        
157000*    DIVERSE KONTROLLER  DEL 2                                   *        
157100*                                                                *        
157200******************************************************************        
157300     SKIP1                                                                
157400     IF  LINK-KVBR-TOT > ZERO                                             
157500     AND W-TIFINLV-AAVV = W-DATUM-GRAENS                                  
157600         MOVE 16          TO W-KDLPORS                                    
157700         PERFORM S03-ADD-TILL-ORSAKSTABELL                                
157800     END-IF                                                               
157900     SKIP1                                                                
158000     COMPUTE W-ANTAL = LINK-KVLS (1)                                      
158100                    -  LINK-KVRESS (1)                                    
158200                    -  LINK-KVSLAGER (1)                                  
158300                    -  LINK-KVOKS-BULK (1)                                
158400                    -  LINK-KVOKS-DAG (1)                                 
158500                    -  LINK-KVOKS-VOR (1)                                 
158600     IF  LINK-KDVVKL = 3                                                  
158700     AND W-ANTAL < ZERO                                                   
158800         MOVE 74          TO W-KDLPORS                                    
158900         PERFORM S03-ADD-TILL-ORSAKSTABELL                                
159000     END-IF                                                               
159100     .                                                                    
159200     EJECT                                                                
159300 G-EXTRALEVERANS-KONTROLL SECTION.                                        
159400     MOVE 'G-EXTRALEVERANS-KONTROLL   ' TO CURRENT-SECTION                
159500                                                                          
159600******************************************************************        
159700*                                                                *        
159800*    EXTRALEVERANS OCH OMSPEC KONTROLL                           *        
159900*                                                                *        
160000******************************************************************        
160100     SKIP1                                                                
160200     SKIP1                                                                
160300     MOVE -6 TO W-ANTAL-VECKOR                                            
160400     DIVIDE LINK-TIFINLV BY 10 GIVING W-DATUM-GRAENS                      
160500     CALL W009VADD USING W-DATUM-GRAENS W-ANTAL-VECKOR                    
160600     SKIP1                                                                
160700      MOVE LNK2-TIAAVV-AKTUELL  TO TMP1-YYWW                              
160800      MOVE W-TIFINLV-AAVV       TO TMP2-YYWW                              
160900      MOVE W-DATUM-GRAENS       TO TMP3-YYWW                              
161000      PERFORM WY2000Q3                                                    
161100     MOVE LINK-IDLEVNR     TO OLIKA-LEV                                   
161200     MOVE LINK-KDPRODSL          TO TEST-KDPRODSL                         
161300     IF  KDPRODSL-BIMA-LOCAL                                              
161400     OR  LINK-KDLPSP = 3                                                  
161500     OR  LINK-REDIRLEV(1) = 1                                             
161600     OR (LINK-KVBR = ZERO AND LINK-KVBEST-PL = ZERO)                      
161700     OR  LINK-IDLEVNR = SPACE                                             
161800     OR  LINK-IDLEVNR = '1000'                                            
161900     OR  SATS-LEVNR                                                       
162000     OR  GEMEN-LEVNR                                                      
162100     OR  EGET-LEVNR                                                       
162200     OR  LINK-IDLEVNR = '9998'                                            
162300     OR  LINK-KDERS (1) > ZERO                                            
162400     OR (LINK-KDHF > ZERO AND LINK-FLAVRART = JA)                         
162500     OR  LINK-KDUART = 'S' OR 'M'                                         
162600     OR  LINK-KDOPPLAN = JA                                               
162700     OR  TMP1-YYWW           < TMP2-YYWW                                  
162800     OR  TMP1-YYWW           < TMP3-YYWW                                  
162900        CONTINUE                                                          
163000     ELSE                                                                 
163100        IF LINK-KDVVKL < 3                                                
163200           COMPUTE W-ANTAL = LINK-KVLS (1)                                
163300                           - LINK-KVRESS (1)                              
163400                           - LINK-KVROS (1)                               
163500                           - LINK-KVOKS-BULK (1)                          
163600                           - LINK-KVOKS-DAG (1)                           
163700                           - LINK-KVOKS-VOR (1)                           
163800                           + LINK-KVAKS (1)                               
163900                           + LINK-KVLAAN                                  
164000                           + LINK-KVLS-SDC-OVER                           
164100        END-IF                                                            
164200     END-IF                                                               
164300     .                                                                    
164400     EJECT                                                                
164500 H-KONTROLL-KORR-LEVPLAN SECTION.                                         
164600     MOVE 'H-KONTROLL-KORR-LEVPLAN    ' TO CURRENT-SECTION                
164700                                                                          
164800******************************************************************        
164900*                                                                *        
165000*    KONTROLL MOT KORRIDORGRÄNS                                  *        
165100*                                                                *        
165200******************************************************************        
165300     SKIP1                                                                
165400     MOVE NEJ TO SW-W2214060                                              
165500     IF  (LINK-KDLPSP NOT = 3 AND NOT = 5)                                
165600     AND (LINK-REDIRLEV(1) NOT = 1)                                       
165700       IF LINK-KDLPORS-TAB (1) > 50                                       
165800       MOVE LINK-IDLEVNR     TO OLIKA-LEV                                 
165900         IF  LINK-IDLEVNR = SPACE OR '9998'                               
166000         OR   (LINK-KDHF > ZERO AND LINK-FLAVRART = JA)                   
166100         OR  LINK-KDUART = 'S' OR 'M'                                     
166200         OR  EGET-LEVNR                                                   
166300         OR (LINK-KVBR = ZERO AND NOT SATS-LEVNR)                         
166400           CONTINUE                                                       
166500         ELSE                                                             
166600           MOVE JA TO SW-W2214060                                         
166700         END-IF                                                           
166800*                                   **** DO-REG SKALL KOLLAS              
166900       ELSE                                                               
167000         IF LINK-KDLPORS-TAB (1) = 50                                     
167100           OR LINK-KDLPORS-TAB (2) = 50                                   
167200           OR LINK-KDLPORS-TAB (3) = 50                                   
167300           MOVE JA TO SW-W2214060                                         
167400         ELSE                                                             
167500           IF LINK-KDLPSP = 6                                             
167600             CONTINUE                                                     
167700           ELSE                                                           
167800             PERFORM HA-KONTROLL-OM-KORRKNTL                              
167900           END-IF                                                         
168000         END-IF                                                           
168100       END-IF                                                             
168200     SKIP1                                                                
168300       IF SW-W2214060 = JA                                                
168400          CALL W2214060 USING LINK-AREA LNK2-AREA                         
168500                                WDK6-PCB WDD9-PCB ARTM-PCB                
168600     SKIP1                                                                
168700         IF  SW-W2214060 = JA                                             
168800           MOVE ZERO TO W-ANTAL-VECKOR                                    
168900           EVALUATE LINK-KDVVKL                                           
169000             WHEN 1                                                       
169100               MOVE 3 TO W-ANTAL-VECKOR                                   
169200             WHEN 2                                                       
169300               MOVE 3 TO W-ANTAL-VECKOR                                   
169400             WHEN 3                                                       
169500               MOVE 3 TO W-ANTAL-VECKOR                                   
169600             WHEN 4                                                       
169700               MOVE 3 TO W-ANTAL-VECKOR                                   
169800             WHEN 5                                                       
169900               MOVE 2 TO W-ANTAL-VECKOR                                   
170000             WHEN 6                                                       
170100             IF LINK-KDLPORS-TAB (1) NOT < 50                             
170200             AND LINK-KDLPORS-TAB (1) < 70                                
170300               MOVE ZERO TO LINK-KDLPSP                                   
170400             END-IF                                                       
170500           END-EVALUATE                                                   
170600           IF  LINK-FLTPO1 = JA                                           
170700             MOVE 1  TO W-ANTAL-VECKOR                                    
170800           END-IF                                                         
170900           IF  W-ANTAL-VECKOR > ZERO                                      
171000             MOVE LINK-TIAAVV-AKT TO LINK-TILPSP                          
171100             CALL W009VADD USING LINK-TILPSP W-ANTAL-VECKOR               
171200           END-IF                                                         
171300         END-IF                                                           
171400       END-IF                                                             
171500     END-IF                                                               
171600     .                                                                    
171700     EJECT                                                                
171800 HA-KONTROLL-OM-KORRKNTL SECTION.                                         
171900     MOVE 'HA-KONTROLL-OM-KORRKNTL    ' TO CURRENT-SECTION                
172000                                                                          
172100     MOVE LINK-IDLEVNR     TO OLIKA-LEV                                   
172200     IF  LINK-IDLEVNR = SPACE OR '9998'                                   
172300     OR   (LINK-KDHF > ZERO AND LINK-FLAVRART = JA)                       
172400     OR  LINK-KDUART = 'S' OR 'M'                                         
172500     OR  EGET-LEVNR                                                       
172600     OR (LINK-KVBR = ZERO AND NOT SATS-LEVNR)                             
172700     OR (LINK-KDLPORS-TAB (1) > ZERO                                      
172800      AND LINK-KDLPORS-TAB (1) < 50)                                      
172900       CONTINUE                                                           
173000     ELSE                                                                 
173100       IF  LINK-KDERS (1) = ZERO                                          
173200       AND LINK-KDLPSP = ZERO                                             
173300     SKIP1                                                                
173400         PERFORM HAA-SOEK-I-LEVTAB                                        
173500     SKIP1                                                                
173600         IF  SW-W2214060 = JA                                             
173700             IF W-IDLPKOLL NOT =  LEVTAB-IDLPKOLL (LEVTAB-IX)             
173800                MOVE NEJ TO SW-W2214060                                   
173900             END-IF                                                       
174000         ELSE                                                             
174100             MOVE LINK-TILPSP       TO TMP1-YYWW                          
174200             MOVE LINK-TIAAVV-AKT   TO TMP2-YYWW                          
174300             PERFORM WY2000P3                                             
174400             IF  TMP1-YYWW <= TMP2-YYWW                                   
174500                 MOVE JA TO SW-W2214060                                   
174600             END-IF                                                       
174700         END-IF                                                           
174800       END-IF                                                             
174900     END-IF                                                               
175000     SKIP3                                                                
175100     .                                                                    
175200 HAA-SOEK-I-LEVTAB SECTION.                                               
175300     MOVE 'HAA-SOEK-I-LEVTAB          ' TO CURRENT-SECTION                
175400     SKIP1                                                                
175500     SEARCH ALL LEVTAB-INGANG                                             
175600         WHEN                                                             
175700            LEVTAB-IDLEVNR (LEVTAB-IX) = LINK-IDLEVNR                     
175800                MOVE JA TO SW-W2214060                                    
175900     END-SEARCH                                                           
176000     .                                                                    
176100     EJECT                                                                
176200 I-KONTROLL-MOT-KOEPPKT SECTION.                                          
176300     MOVE 'I-KONTROLL-MOT-KOEPPKT     ' TO CURRENT-SECTION                
176400                                                                          
176500******************************************************************        
176600*                                                                *        
176700*    KONTROLL MOT KÖPPUNKT                                       *        
176800*                                                                *        
176900******************************************************************        
177000     SKIP1                                                                
177100     IF  LINK-KDAVT > ZERO                                                
177200     AND LINK-KDERS (1) = ZERO                                            
177300     AND LINK-KDLPORS-TAB (1) > 6                                         
177400     AND LINK-KDLPORS-TAB (1) < 50                                        
177500     AND LINK-KDKSP = ZERO                                                
177600         MOVE BEARBETA TO LINK-KDCALL                                     
177700         CALL W2214040 USING LINK-AREA LNK2-AREA                          
177800     END-IF                                                               
177900     .                                                                    
178000     EJECT                                                                
178100 J-SKAPA-UTFILER SECTION.                                                 
178200     MOVE 'J-SKAPA-UTFILER            ' TO CURRENT-SECTION                
178300                                                                          
178400******************************************************************        
178500*                                                                *        
178600*    SKAPA UTFILER: W22142 BEGÄRAN AV OMSPEC                     *        
178700*                                                                *        
178800******************************************************************        
178900     IF FLANSK-TEST = NEJ                                                 
179000**** OMSPEC VARJE VECKA MED ORSAK BEGÄRD FÖR JIT-ARTIKLAR                 
179100       IF LINK-FLJIT = JA   AND  LINK-KDERS (1) < 11 AND                  
179200         (LINK-KDLPORS-TAB (1) = 0 OR > 49)                               
179300          MOVE 23          TO W-KDLPORS                                   
179400          PERFORM S03-ADD-TILL-ORSAKSTABELL                               
179500       END-IF                                                             
179600     END-IF                                                               
179700     SKIP2                                                                
179800     IF  LINK-KDLPORS-TAB (1) > ZERO                                      
179900     AND LINK-KDLPORS-TAB (1) < 50                                        
180000         MOVE  LINK-IDARTNR TO U42BEG-IDARTNR                             
180100         MOVE LINK-KDLPORS-TAB (1) TO U42BEG-KDLPORS-TAB (1)              
180200         MOVE LINK-KDLPORS-TAB (2) TO U42BEG-KDLPORS-TAB (2)              
180300         MOVE LINK-KDLPORS-TAB (3) TO U42BEG-KDLPORS-TAB (3)              
180400         MOVE LINK-KVBEST-PL TO U42BEG-KVBEST-PL                          
180500         IF LINK-KVBEST-PL > ZERO                                         
180600           MOVE 1 TO U42BEG-KDPLKOEP                                      
180700         ELSE                                                             
180800           MOVE ZERO TO U42BEG-KDPLKOEP                                   
180900         END-IF                                                           
181000                                                                          
181100         PERFORM JA-SKRIV-W22142                                          
181200     END-IF                                                               
181300                                                                          
181400     PERFORM JD-SAMLA-DATA-TILL-W22149                                    
181500     .                                                                    
181600     EJECT                                                                
181700 JA-SKRIV-W22142 SECTION.                                                 
181800     MOVE 'JA-SKRIV-W22142            ' TO CURRENT-SECTION                
181900                                                                          
182000     WRITE U42BEG-POST FROM U42BEG-AREA                                   
182100     SKIP1                                                                
182200     MOVE 'W22142' TO POSTSUM-FDNAMN                                      
182300     MOVE 'W22140D4' TO POSTSUM-DDNAMN2                                   
182400     MOVE SPACE TO POSTSUM-TRANSTYP                                       
182500     CALL POSTSUM USING POSTSUM-PARM                                      
182600     SKIP3                                                                
182700     .                                                                    
182800 JD-SAMLA-DATA-TILL-W22149 SECTION.                                       
182900     MOVE 'JD-SAMLA-DATA-TILL-W22149  ' TO CURRENT-SECTION                
183000                                                                          
183100     MOVE WLINK-IDARTNR          TO W22149-IDARTNR                        
183200     MOVE WLINK-KDGK             TO W22149-KDGK                           
183300     MOVE WLINK-KVPB-VESL(1)     TO W22149-KVPB-VESL-GAMMAL(1)            
183400     MOVE WLINK-KVPB-TPO(1)      TO W22149-KVPB-TPO-GAMMAL(1)             
183500     MOVE LINK-KVPB-VESL(1)      TO W22149-KVPB-VESL-NY(1)                
183600     MOVE LINK-KVPB-TPO(1)       TO W22149-KVPB-TPO-NY(1)                 
183700                                                                          
183800     PERFORM S02-SKRIV-W22149                                             
183900     .                                                                    
184000     EJECT                                                                
184100                                                                          
184200 K-KONTROLL-UPPDATERING SECTION.                                          
184300     MOVE 'K-KONTROLL-UPPDATERING     ' TO CURRENT-SECTION                
184400                                                                          
184500******************************************************************        
184600*                                                                *        
184700*    UPPDATERING AV ÄNDRAD INFORMATION                           *        
184800*                                                                *        
184900******************************************************************        
185000     SKIP1                                                                
185100     IF WLINK-REGISTER-DATA-UPPD NOT = LINK-REGISTER-DATA-UPPD            
185200         MOVE LAES-ARTIKEL TO LINK-KDCALL                                 
185300         CALL W2214010 USING LINK-AREA WDK6-PCB WDD9-PCB                  
185400                              WDK7-PCB ARTM-PCB WDL2-PCB                  
185500                              WDF1-PCB WDB6-PCB                           
185600                                                                          
185700     SKIP3                                                                
185800       IF WLINK-FLMANQ        NOT = LINK-FLMANQ                           
185900       OR WLINK-KDAVT         NOT = LINK-KDAVT                            
186000       OR WLINK-KDKSP         NOT = LINK-KDKSP                            
186100       OR WLINK-KDLPSP        NOT = LINK-KDLPSP                           
186200       OR WLINK-KDLTK         NOT = LINK-KDLTK                            
186300       OR WLINK-KDVVKL        NOT = LINK-KDVVKL                           
186400       OR WLINK-KDOPPLAN      NOT = LINK-KDOPPLAN                         
186500       OR WLINK-KVAP          NOT = LINK-KVAP                             
186600       OR WLINK-KVBK          NOT = LINK-KVBK                             
186700       OR WLINK-KVKP          NOT = LINK-KVKP                             
186800       OR WLINK-KVOVERF       NOT = LINK-KVOVERF                          
186900       OR WLINK-KVQ           NOT = LINK-KVQ                              
187000       OR WLINK-KVQ-JUST      NOT = LINK-KVQ-JUST                         
187100       OR WLINK-KVSLUTKP      NOT = LINK-KVSLUTKP                         
187200       OR WLINK-TIBESRPT      NOT = LINK-TIBESRPT                         
187300       OR WLINK-TIBESRPT-PAAM NOT = LINK-TIBESRPT-PAAM                    
187400       OR WLINK-TILPSP        NOT = LINK-TILPSP                           
187500       OR WLINK-TIQJUST       NOT = LINK-TIQJUST                          
187600       OR WLINK-KVVECKOR-FT   NOT = LINK-KVVECKOR-FT                      
187700       OR WLINK-KVVECKOR-LT   NOT = LINK-KVVECKOR-LT                      
187800       OR WLINK-KVVECKOR-BT   NOT = LINK-KVVECKOR-BT                      
187900       OR WLINK-KVVECKOR-AT   NOT = LINK-KVVECKOR-AT                      
188000       OR WLINK-KVDAGAR-INLEV NOT = LINK-KVDAGAR-INLEV                    
188100       OR WLINK-KVDAGAR-FFH   NOT = LINK-KVDAGAR-FFH                      
188200       OR WLINK-FLJIT         NOT = LINK-FLJIT                            
188300       OR WLINK-KDLEVPLF      NOT = LINK-KDLEVPLF                         
188400       OR WLINK-KDFREKKL      NOT = LINK-KDFREKKL                         
188500       OR WLINK-KDPRISKL      NOT = LINK-KDPRISKL                         
188600       OR WLINK-DAPBPLAN      NOT = LINK-DAPBPLAN                         
188700       OR  WLINK-FLMPB (1)     NOT = LINK-FLMPB (1)                       
188800       OR  WLINK-KVMAD-SEP (1) NOT =  LINK-KVMAD-SEP (1)                  
188900       OR  WLINK-KVMAD-TOT (1) NOT =  LINK-KVMAD-TOT (1)                  
189000       OR  WLINK-KVMP      (1) NOT =  LINK-KVMP      (1)                  
189100       OR  WLINK-KVPB-VESL (1) NOT =  LINK-KVPB-VESL (1)                  
189200       OR  WLINK-RESLJUST  (1) NOT =  LINK-RESLJUST  (1)                  
189300       OR  WLINK-TISLJUST  (1) NOT =  LINK-TISLJUST  (1)                  
189400       OR  WLINK-KVPB-SEP  (1) NOT =  LINK-KVPB-SEP  (1)                  
189500       OR  WLINK-RVPROFEL  (1) NOT =  LINK-RVPROFEL  (1)                  
189600       OR  WLINK-RVPROURS  (1) NOT =  LINK-RVPROURS  (1)                  
189700       OR  WLINK-FLMANPB   (1) NOT =  LINK-FLMANPB   (1)                  
189800       OR  WLINK-KVPB-TPO  (1) NOT =  LINK-KVPB-TPO  (1)                  
189900       OR WLINK-KVSLAGER   (1) NOT =  LINK-KVSLAGER  (1)                  
190000                                                                          
190100          PERFORM KA-UPPDAT-CLAG                                          
190200       END-IF                                                             
190300     END-IF                                                               
190400     .                                                                    
190500     EJECT                                                                
190600 KA-UPPDAT-CLAG SECTION.                                                  
190700     MOVE 'KA-UPPDAT-CLAG '  TO CURRENT-SECTION                           
190800                                                                          
190900*--- E-UPPDAT-CLAG SECTION. I W2214010                                    
191000                                                                          
191100     PERFORM S100-NOLLA-W2214A-AREA                                       
191200                                                                          
191300     MOVE UPPDAT-CLAG-4A     TO U4A-UPD-IDPTYP                            
191400     MOVE WC-CDC-SE          TO U4A-UPD-IDDC                              
191500     MOVE SPACE              TO U4A-UPD-IDLEVNR                           
191600     MOVE LINK-IDARTNR       TO U4A-UPD-IDARTNR                           
191700                                                                          
191800     MOVE LINK-FLMANQ        TO U4A-UPD-FLMANQ                            
191900     MOVE LINK-KDAVT         TO U4A-UPD-KDAVT                             
192000     MOVE LINK-KDKSP         TO U4A-UPD-KDKSP                             
192100     MOVE LINK-KDLPSP        TO U4A-UPD-KDLPSP                            
192200     MOVE LINK-KDVVKL        TO U4A-UPD-KDVVKL                            
192300     MOVE LINK-KDOPPLAN      TO U4A-UPD-KDOPPLAN                          
192400     MOVE LINK-KVAP          TO U4A-UPD-KVAP                              
192500     MOVE LINK-KVBK          TO U4A-UPD-KVBK                              
192600     MOVE LINK-KVKP          TO U4A-UPD-KVKP                              
192700     MOVE LINK-KVOVERF       TO U4A-UPD-KVOVERF                           
192800     MOVE LINK-KVQ           TO U4A-UPD-KVQ                               
192900     MOVE LINK-KVQ-JUST      TO U4A-UPD-KVQ-JUST                          
193000     MOVE LINK-KVSLUTKP      TO U4A-UPD-KVSLUTKP                          
193100     MOVE LINK-TIBESRPT      TO U4A-UPD-TIBESRPT                          
193200     MOVE LINK-TIBESRPT-PAAM TO U4A-UPD-TIBESRPT-PAAM                     
193300     MOVE LINK-TILPSP        TO U4A-UPD-TILPSP                            
193400     MOVE LINK-TIQJUST       TO U4A-UPD-TIQJUST                           
193500     MOVE LINK-KVDAGAR-INLEV TO U4A-UPD-KVDAGAR-INLEV                     
193600     MOVE LINK-KVDAGAR-FFH   TO U4A-UPD-KVDAGAR-FFH                       
193700     MOVE LINK-KVVECKOR-LT   TO U4A-UPD-KVVECKOR-LT                       
193800     MOVE LINK-KVVECKOR-FT   TO U4A-UPD-KVVECKOR-FT                       
193900     MOVE LINK-KVVECKOR-BT   TO U4A-UPD-KVVECKOR-BT                       
194000     MOVE LINK-KVVECKOR-AT   TO U4A-UPD-KVVECKOR-AT                       
194100     MOVE LINK-FLJIT         TO U4A-UPD-FLJIT                             
194200     MOVE LINK-KDFREKKL      TO U4A-UPD-KDFREKKL                          
194300     MOVE LINK-KDPRISKL      TO U4A-UPD-KDPRISKL                          
194400                                                                          
194500     IF LINK-KDLEVPLF      =  SPACE                                       
194600        MOVE 'J'             TO U4A-UPD-KDLEVPLF                          
194700     ELSE                                                                 
194800        MOVE LINK-KDLEVPLF   TO U4A-UPD-KDLEVPLF                          
194900     END-IF                                                               
195000                                                                          
195100*--- KOD FÖR CLAG-KVSLUTKP + CLAG-IDPLANGR-LEV                            
195200*--- FLYTTAD TILL BMP W2214A00                                            
195300                                                                          
195400     MOVE 1                   TO IX                                       
195500     MOVE LINK-FLMPB     (IX) TO U4A-UPD-FLMPB                            
195600     MOVE LINK-KVMAD-SEP (IX) TO U4A-UPD-KVMAD-SEP                        
195700     MOVE LINK-KVMAD-TOT (IX) TO U4A-UPD-KVMAD-TOT                        
195800     MOVE LINK-KVMP (IX)      TO U4A-UPD-KVMP                             
195900     MOVE LINK-KVPB-VESL (IX) TO U4A-UPD-KVPB-VESL                        
196000     MOVE LINK-RESLJUST (IX)  TO U4A-UPD-RESLJUST                         
196100     MOVE LINK-TISLJUST (IX)  TO U4A-UPD-TISLJUST                         
196200     MOVE LINK-KVPB-SEP (IX)  TO U4A-UPD-KVPB-SEP                         
196300     MOVE LINK-RVPROURS (IX)  TO U4A-UPD-RVPROURS                         
196400     MOVE LINK-RVPROFEL (IX)  TO U4A-UPD-RVPROFEL                         
196500     MOVE LINK-FLMANPB  (IX)  TO U4A-UPD-FLMANPB                          
196600     MOVE LINK-KVPB-TPO (IX)  TO U4A-UPD-KVPB-TPO                         
196700     MOVE LINK-TILTK          TO U4A-UPD-TILTK                            
196800     MOVE LINK-KDLTK          TO U4A-UPD-KDLTK                            
196900     MOVE LINK-KVSLAGER (IX)  TO U4A-UPD-KVSLAGER                         
197000     MOVE LINK-KVULOAD        TO U4A-UPD-KVULOAD                          
197100     MOVE LINK-KVEOQ          TO U4A-UPD-KVEOQ                            
197200     MOVE LINK-KVSLAGER-OPT   TO U4A-UPD-KVSLAGER-OPT                     
197300     MOVE LINK-DAPBPLAN       TO U4A-UPD-DAPBPLAN                         
197400     MOVE LINK-DASEASON       TO U4A-UPD-DASEASON                         
197500                                                                          
197600     MOVE LINK-RESEASON-PLAN (1) TO U4A-UPD-RESEASON-PLAN (1)             
197700     MOVE LINK-RESEASON-PLAN (2) TO U4A-UPD-RESEASON-PLAN (2)             
197800     MOVE LINK-RESEASON-PLAN (3) TO U4A-UPD-RESEASON-PLAN (3)             
197900     MOVE LINK-RESEASON-PLAN (4) TO U4A-UPD-RESEASON-PLAN (4)             
198000     MOVE LINK-RESEASON-PLAN (5) TO U4A-UPD-RESEASON-PLAN (5)             
198100     MOVE LINK-RESEASON-PLAN (6) TO U4A-UPD-RESEASON-PLAN (6)             
198200     MOVE LINK-RESEASON-PLAN (7) TO U4A-UPD-RESEASON-PLAN (7)             
198300     MOVE LINK-RESEASON-PLAN (8) TO U4A-UPD-RESEASON-PLAN (8)             
198400     MOVE LINK-RESEASON-PLAN (9) TO U4A-UPD-RESEASON-PLAN (9)             
198500     MOVE LINK-RESEASON-PLAN (10) TO U4A-UPD-RESEASON-PLAN (10)           
198600     MOVE LINK-RESEASON-PLAN (11) TO U4A-UPD-RESEASON-PLAN (11)           
198700     MOVE LINK-RESEASON-PLAN (12) TO U4A-UPD-RESEASON-PLAN (12)           
198800                                                                          
198900     PERFORM S05-SKRIV-W2214A                                             
199000                                                                          
199100     .                                                                    
199200     EJECT                                                                
199300 M-ANSK-KONTROLL SECTION.                                                 
199400     MOVE 'M-ANSK-KONTROLL            ' TO CURRENT-SECTION                
199500                                                                          
199600     MOVE NEJ      TO FLANSK-TEST                                         
199700                                                                          
199800     SEARCH ALL TABANSK-INGANG WHEN                                       
199900       TABANSK-IDANSK (TABANSK-IX) = LINK-IDANSK                          
200000        MOVE JA       TO FLANSK-TEST                                      
200100     END-SEARCH                                                           
200200     SKIP2                                                                
200300        COMPUTE UDDA-DATUM = LINK-TIAAVV-AKT * 0.5                        
200400     .                                                                    
200500     EJECT                                                                
200600                                                                          
200700 N-KONTROLL-DISPDAT SECTION.                                              
200800     MOVE 'N-KONTROLL-DISPDAT         ' TO CURRENT-SECTION                
200900     SKIP3                                                                
201000     MOVE NEJ         TO FL22148POST-C1                                   
201100     MOVE LINK-TIDISPIN(1) TO WS-TIDISPIN-C1                              
201200     PERFORM NA-SAKNAS-LEVERANSPLAN                                       
201300     MOVE WS-TIDISPIN-C1   TO TMP1-YYMMDD                                 
201400     MOVE W-DATUM-AAMMDD   TO TMP2-YYMMDD                                 
201500     PERFORM WY2000P1                                                     
201600     IF  TMP1-YYMMDD <= TMP2-YYMMDD                                       
201700     AND WS-TIDISPIN-C1 >  ZERO                                           
201800        MOVE JA TO FL22148POST-C1                                         
201900     END-IF                                                               
202000                                                                          
202100     MOVE LINK-TIFINLV       TO TMP1-YYWWD                                
202200     MOVE LINK-TIAAVVD-AKT   TO TMP2-YYWWD                                
202300     PERFORM WY2000P2                                                     
202400     IF  LINK-PRARTSTD = 0                                                
202500     AND LINK-TIFINLV NOT = 99991                                         
202600     AND TMP1-YYWWD   NOT > TMP2-YYWWD                                    
202700            MOVE JA TO FL22148POST-C1                                     
202800     END-IF                                                               
202900                                                                          
203000     IF LINK-PRARTSTD > 0 AND FL-LEVPLAN-SAKNAS = JA                      
203100            MOVE JA TO FL22148POST-C1                                     
203200     END-IF                                                               
203300                                                                          
203400     IF WLINK-KVPB-TPO(1) NOT = LINK-KVPB-TPO(1)                          
203500        MOVE JA TO FL22148POST-C1                                         
203600     END-IF                                                               
203700                                                                          
203800     MOVE LINK-IDARTNR TO U22148-IDARTNR                                  
203900     IF FL22148POST-C1 = JA                                               
204000        PERFORM S01-SKRIV-W22148                                          
204100     END-IF                                                               
204200     .                                                                    
204300     EJECT                                                                
204400                                                                          
204500 NA-SAKNAS-LEVERANSPLAN SECTION.                                          
204600     MOVE 'NA-SAKNAS-LEVERANSPLAN     ' TO CURRENT-SECTION                
204700                                                                          
204800     MOVE NEJ            TO FL-LEVPLAN-SAKNAS                             
204900     MOVE LINK-IDARTNR   TO LINK3-IDARTNR                                 
205000     MOVE SPACE          TO LINK3-IDLEVNR                                 
205100     MOVE 2              TO LINK3-KDAVROP                                 
205200     MOVE LAES-AVROP-FIRST TO LINK3-KDCALL                                
205300     CALL W2214010 USING LINK3-AREA                                       
205400                         WDK6-PCB                                         
205500                         WDD9-PCB                                         
205600                         WDK7-PCB                                         
205700                         ARTM-PCB                                         
205800                         WDL2-PCB                                         
205900                         WDF1-PCB                                         
206000                         WDB6-PCB                                         
206100     IF LINK3-ANROP-FEL                                                   
206200        MOVE LAES-AVROP-NEXT TO LINK3-KDCALL                              
206300        CALL W2214010 USING LINK3-AREA                                    
206400                            WDK6-PCB                                      
206500                            WDD9-PCB                                      
206600                            WDK7-PCB                                      
206700                            ARTM-PCB                                      
206800                            WDL2-PCB                                      
206900                            WDF1-PCB                                      
207000                            WDB6-PCB                                      
207100        IF LINK3-ANROP-FEL                                                
207200           MOVE JA TO FL-LEVPLAN-SAKNAS                                   
207300        END-IF                                                            
207400     END-IF                                                               
207500     .                                                                    
207600     EJECT                                                                
207700 O-DIVERSE-DATA SECTION.                                                  
207800     MOVE 'O-DIVERSE-DATA             ' TO CURRENT-SECTION                
207900                                                                          
208000     DIVIDE LINK-TIFINLV BY 10 GIVING W-TIFINLV-AAVV                      
208100     MOVE LINK-KVVECKOR-FT TO W-ANTAL-VECKOR                              
208200     MOVE LINK-TIAAVV-AKT TO W-DATUM-GRAENS                               
208300     CALL W009VADD USING W-DATUM-GRAENS W-ANTAL-VECKOR                    
208400     .                                                                    
208500     EJECT                                                                
208600 P-KONTROLL-EJ-AUT-GODK-FORSLAG SECTION.                                  
208700     MOVE 'P-KONTROLL-EJ-AUT-GODK-FORSLAG ' TO CURRENT-SECTION            
208800                                                                          
208900*    FÖR VISSA LEVERANTÖRER OCH VISSA KONTON VILL MAN EJ                  
209000*    ATT LEVPLAN-FÖRSLAGEN SKALL KUNNA GODKÄNNAS AUTOMATISKT,             
209100*    DÅ SÄTTS KDLEVPLF = G  (I WDK611)                                    
209200                                                                          
209300     IF LINK-KDLEVPLF = 'P'                                               
209400        MOVE LINK-TIFINLV TO W-TIFINLV                                    
209500        MOVE AKT-DATUM-AA   TO TMP1-YY                                    
209600        MOVE W-TIFINLV-AA   TO TMP2-YY                                    
209700        PERFORM WY2000P9                                                  
209800        COMPUTE ANTAL-VV = (TMP1-YY      - TMP2-YY     ) * 52 +           
209900                           (AKT-DATUM-VV - W-TIFINLV-VV)                  
210000        IF ANTAL-VV > 26                                                  
210100          MOVE 'J' TO LINK-KDLEVPLF                                       
210200        END-IF                                                            
210300     END-IF                                                               
210400                                                                          
210500     IF LINK-KDLEVPLF = 'G' OR SPACE OR LOW-VALUE                         
210600        MOVE 'J' TO LINK-KDLEVPLF                                         
210700     END-IF                                                               
210800                                                                          
210900     IF LINK-KDLEVPLF = 'P' OR 'S'                                        
211000         CONTINUE                                                         
211100     ELSE                                                                 
211200     MOVE +1 TO IX-LN                                                     
211300     PERFORM UNTIL IX-LN > LN-TAB-MAX OR                                  
211400                   LINK-KDLEVPLF = 'G'                                    
211500        IF LINK-IDLEVNR = LN-TAB-IDLEVNR (IX-LN) AND                      
211600          (LINK-IDANSK NOT < LN-TAB-IDANSK-START (IX-LN) AND              
211700           LINK-IDANSK NOT > LN-TAB-IDANSK-SLUT  (IX-LN) )                
211800           MOVE 'G' TO LINK-KDLEVPLF                                      
211900        END-IF                                                            
212000        ADD +1 TO IX-LN                                                   
212100     END-PERFORM                                                          
212200                                                                          
212300     MOVE LINK-KDPRODSL          TO TEST-KDPRODSL                         
212400     IF KDPRODSL-VCBV-EMB                                                 
212500        MOVE 'G' TO LINK-KDLEVPLF                                         
212600     END-IF                                                               
212700                                                                          
212800     IF LINK-IDANSK NOT < 721 AND                                         
212900        LINK-IDANSK NOT > 724 AND                                         
213000        KDPRODSL-TOOLS                                                    
213100        MOVE 'G' TO LINK-KDLEVPLF                                         
213200     END-IF                                                               
213300                                                                          
213400     IF LINK-REDIRLEV (1) = 1                                             
213500        MOVE 'G' TO LINK-KDLEVPLF                                         
213600     END-IF                                                               
213700                                                                          
213800     IF FLANSK-TRAEFF AND LINK-KDLEVPLF = 'J'                             
213900        MOVE 'G' TO LINK-KDLEVPLF                                         
214000     END-IF                                                               
214100                                                                          
214200**** STOPPAR VISSA ANSKAFFARE GENERELLT    *************                  
214300     IF LINK-IDANSK = 705                                                 
214400        MOVE 'G' TO LINK-KDLEVPLF                                         
214500     END-IF                                                               
214600****                                       *************                  
214700     END-IF                                                               
214800                                                                          
214900                                                                          
215000     IF LINK-KDLEVPLF = 'G'                                               
215100        MOVE 'W221LN' TO POSTSUM-FDNAMN                                   
215200        MOVE 'ANTAL   ' TO POSTSUM-DDNAMN2                                
215300        MOVE 'G'       TO POSTSUM-TRANSTYP                                
215400        CALL POSTSUM USING POSTSUM-PARM                                   
215500     END-IF                                                               
215600                                                                          
215700     IF LINK-KDLEVPLF = 'P'                                               
215800        MOVE 'W221LN' TO POSTSUM-FDNAMN                                   
215900        MOVE 'ANTAL   ' TO POSTSUM-DDNAMN2                                
216000        MOVE 'P'       TO POSTSUM-TRANSTYP                                
216100        CALL POSTSUM USING POSTSUM-PARM                                   
216200     END-IF                                                               
216300     .                                                                    
216400     EJECT                                                                
216500 Z-AVSLUTA SECTION.                                                       
216600     MOVE 'Z-AVSLUTA                ' TO CURRENT-SECTION                  
216700                                                                          
216800     MOVE AVSLUTA TO LINK-KDCALL                                          
216900     CALL W2214040 USING LINK-AREA LNK2-AREA                              
217000     SKIP1                                                                
217100     CLOSE                                                                
217200           W22131                                                         
217300           W22133                                                         
217400           W22135                                                         
217500           W22142                                                         
217600           W2214A                                                         
217700           W22199                                                         
217800           W22148                                                         
217900           W22149                                                         
218000           W221LN                                                         
218100     SKIP1                                                                
218200     MOVE 'S' TO POSTSUM-OPKOD                                            
218300     CALL POSTSUM USING POSTSUM-PARM                                      
218400     .                                                                    
218500                                                                          
218600 S01-SKRIV-W22148 SECTION.                                                
218700     MOVE 'S01-SKRIV-W22148         ' TO CURRENT-SECTION                  
218800                                                                          
218900     WRITE U22148-POST FROM U22148-AREA                                   
219000     MOVE 'W22148' TO POSTSUM-FDNAMN                                      
219100     MOVE 'W22148DD' TO POSTSUM-DDNAMN2                                   
219200     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
219300     CALL POSTSUM USING POSTSUM-PARM                                      
219400     .                                                                    
219500     EJECT                                                                
219600                                                                          
219700 S02-SKRIV-W22149 SECTION.                                                
219800     MOVE 'S02-SKRIV-W22149         ' TO CURRENT-SECTION                  
219900                                                                          
220000     WRITE W22149-POST FROM W22149-AREA                                   
220100     MOVE 'W22149' TO POSTSUM-FDNAMN                                      
220200     MOVE 'W22149DE' TO POSTSUM-DDNAMN2                                   
220300     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
220400     CALL POSTSUM USING POSTSUM-PARM                                      
220500     .                                                                    
220600     EJECT                                                                
220700                                                                          
220800 S03-ADD-TILL-ORSAKSTABELL SECTION.                                       
220900     MOVE 'S03-ADD-TILL-ORSAKSTABELL' TO CURRENT-SECTION                  
221000                                                                          
221100     MOVE LINK-KDLPORS-TAB(01)   TO W-KDLPORS-TAB(01)                     
221200     MOVE LINK-KDLPORS-TAB(02)   TO W-KDLPORS-TAB(02)                     
221300     MOVE LINK-KDLPORS-TAB(03)   TO W-KDLPORS-TAB(03)                     
221400     MOVE W-KDLPORS              TO W-KDLPORS-TAB(04)                     
221500                                                                          
221600     CALL W221LPAD USING W-W221LP-CTX W-KDLPORS-GRP                       
221700                                                                          
221800     MOVE W-KDLPORS-TAB(01)      TO LINK-KDLPORS-TAB(01)                  
221900     MOVE W-KDLPORS-TAB(02)      TO LINK-KDLPORS-TAB(02)                  
222000     MOVE W-KDLPORS-TAB(03)      TO LINK-KDLPORS-TAB(03)                  
222100     .                                                                    
222200     EJECT                                                                
222300 S04-NOLLA-W22222 SECTION.                                                
222400                                                                          
222500     MOVE +0                        TO LNK2-KVBEHOV-SUMMA                 
222600                                       LNK2-KVBEHOV-DESSUTOM              
222700                                       LNK2-TIBEHOV-FIRST                 
222800                                                                          
222900     MOVE +1                        TO INDX                               
223000     PERFORM UNTIL INDX > MAX-INDX                                        
223100       MOVE +0                      TO LNK2-KVBEHOV-VECKA(INDX)           
223200       ADD +1                       TO INDX                               
223300     END-PERFORM                                                          
223400     .                                                                    
223500     EJECT                                                                
223600 S05-SKRIV-W2214A SECTION.                                                
223700     MOVE 'S05-SKRIV-W2214A         ' TO CURRENT-SECTION                  
223800                                                                          
223900     WRITE U4A-POST FROM U4A-AREA                                         
224000     MOVE 'W2214A' TO POSTSUM-FDNAMN                                      
224100     MOVE 'W22140D5' TO POSTSUM-DDNAMN2                                   
224200     MOVE U4A-UPD-IDPTYP TO POSTSUM-TRANSTYP                              
224300     CALL POSTSUM USING POSTSUM-PARM                                      
224400     .                                                                    
224500     EJECT                                                                
224600 S100-NOLLA-W2214A-AREA SECTION.                                          
224700     MOVE 'S100-NOLLA-W2214A-AREA '  TO CURRENT-SECTION                   
224800                                                                          
224900*-- UPPDATERA WDK611                                                      
225000                                                                          
225100      MOVE SPACE   TO U4A-UPD-IDPTYP                                      
225200                      U4A-UPD-IDDC                                        
225300                      U4A-UPD-IDLEVNR                                     
225400                      U4A-UPD-FLMANQ                                      
225500                      U4A-UPD-KDOPPLAN                                    
225600                      U4A-UPD-FLJIT                                       
225700                      U4A-UPD-KDFREKKL                                    
225800                      U4A-UPD-KDPRISKL                                    
225900                      U4A-UPD-KDLEVPLF                                    
226000                      U4A-UPD-FLMPB                                       
226100                      U4A-UPD-FLMANPB                                     
226200                                                                          
226300      MOVE ZERO    TO U4A-UPD-IDARTNR                                     
226400                      U4A-UPD-KDAVT                                       
226500                      U4A-UPD-KDKSP                                       
226600                      U4A-UPD-KDLPSP                                      
226700                      U4A-UPD-KDVVKL                                      
226800                      U4A-UPD-KVAP                                        
226900                      U4A-UPD-KVBK                                        
227000                      U4A-UPD-KVKP                                        
227100                      U4A-UPD-KVOVERF                                     
227200                      U4A-UPD-KVQ                                         
227300                      U4A-UPD-KVQ-JUST                                    
227400                      U4A-UPD-KVSLUTKP                                    
227500                      U4A-UPD-TIBESRPT                                    
227600                      U4A-UPD-TIBESRPT-PAAM                               
227700                      U4A-UPD-TILPSP                                      
227800                      U4A-UPD-TIQJUST                                     
227900                      U4A-UPD-KVDAGAR-INLEV                               
228000                      U4A-UPD-KVDAGAR-FFH                                 
228100                      U4A-UPD-KVVECKOR-LT                                 
228200                      U4A-UPD-KVVECKOR-FT                                 
228300                      U4A-UPD-KVVECKOR-BT                                 
228400                      U4A-UPD-KVVECKOR-AT                                 
228500                      U4A-UPD-IDPLANGR-LEV                                
228600                      U4A-UPD-KVMAD-SEP                                   
228700                      U4A-UPD-KVMAD-TOT                                   
228800                      U4A-UPD-KVMP                                        
228900                      U4A-UPD-KVPB-VESL                                   
229000                      U4A-UPD-RESLJUST                                    
229100                      U4A-UPD-TISLJUST                                    
229200                      U4A-UPD-KVPB-SEP                                    
229300                      U4A-UPD-RVPROFEL                                    
229400                      U4A-UPD-KVPB-TPO                                    
229500                      U4A-UPD-TILTK                                       
229600                      U4A-UPD-KDLTK                                       
229700                      U4A-UPD-KVSLAGER                                    
229800                      U4A-UPD-KVULOAD                                     
229900                      U4A-UPD-KVEOQ                                       
230000                      U4A-UPD-KVSLAGER-OPT                                
230100                      U4A-UPD-DAPBPLAN                                    
230200                      U4A-UPD-DASEASON                                    
230300                                                                          
230400     MOVE 1 TO IX                                                         
230500     PERFORM UNTIL IX > 12                                                
230600        MOVE ZERO TO U4A-UPD-RESEASON-PLAN(IX)                            
230700        ADD 1     TO IX                                                   
230800     END-PERFORM                                                          
230900                                                                          
231000*-- UPPDATERA WDD9                                                        
231100     MOVE ZERO     TO U4A-UPD-KDAVROP                                     
231200     .                                                                    
231300     EJECT                                                                
231400                                                                          
231500*    -COPY WY2000P1                                                       
231600     EJECT                                                                
231700*    -COPY WY2000P2                                                       
231800     EJECT                                                                
231900*    -COPY WY2000P3                                                       
232000     EJECT                                                                
232100*    -COPY WY2000P9                                                       
232200     EJECT                                                                
232300*    -COPY WY2000Q3                                                       
