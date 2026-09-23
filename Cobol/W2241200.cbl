000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2241200.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   13/02/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LEVERANSPLANEBERÄKNING LOKAL ANSKAFFNING                         
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDK7                                       
001100*        PROGRAMMET LÄSER      WDD9                                       
001200*        PROGRAMMET LÄSER      WDB6                                       
001300*        PROGRAMMET LÄSER      WDD6                                       
001400*        PROGRAMMET LÄSER      WDL6                                       
001500*        PROGRAMMET LÄSER      WDF1                                       
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100* CHANGE LOG:                                                             
002200* 2015-04-22   E'TRACKER 10130993                                         
002300*              REDUCE NUMBER OF DELIVERY SCHEDULES                        
002400*                                                                         
002500* 2015-09-10   ETRACKER 10209749    (WDD903)                              
002600*              ÄNDRA 2103/2403 ORSAK EXTRALEVERANSER OCH MERA.            
002700*                                                                         
002800* 2015-11-12   E'TRACKER 10243132  KINA EXPORT 2015                       
002900*                                                                         
003000* 2016-11-24   E'TRACKER 10292047  RULES FOR 2447 SAME AS 2147.           
003100*                                  (3 V INNAN NY OMSPEC)                  
003200*                                                                         
003300* 2017-09-11   E'TRACKER 10299286  LOKAL ANSKAFFNING USA                  
003400*                                                                         
003500*                                                                         
003600                                                                          
003700                                                                          
003800 ENVIRONMENT DIVISION.                                                    
003900 INPUT-OUTPUT SECTION.                                                    
004000                                                                          
004100 FILE-CONTROL.                                                            
004200                                                                          
004300*          --- OMSPEC. ARTIKLAR                                           
004400     SELECT W22410                     ASSIGN TO W22412D1.                
004500                                                                          
004600*          --- NDC-CN / NDC-US ARTIKLAR                                   
004700     SELECT W22418                     ASSIGN TO W22412D2.                
004800                                                                          
004900*          --- OMSPEC LEV.PLAN                                            
005000     SELECT W22412                     ASSIGN TO W22412D3.                
005100                                                                          
005200*          --- UPPDATERINGAR LEVERANSPLAN                                 
005300     SELECT W22413                     ASSIGN TO W22412D4.                
005400                                                                          
005500                                                                          
005600 DATA DIVISION.                                                           
005700 FILE SECTION.                                                            
005800                                                                          
005900 FD  W22410                                                               
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200                                                                          
006300*01  -COPY W2242204      -L.                                              
006400                                                                          
006500                                                                          
006600 FD  W22418                                                               
006700     RECORDING       F                                                    
006800     BLOCK CONTAINS  0.                                                   
006900                                                                          
007000*01  -COPY W22418      -L.                                                
007100                                                                          
007200                                                                          
007300 FD  W22412                                                               
007400     RECORDING       F                                                    
007500     BLOCK CONTAINS  0.                                                   
007600                                                                          
007700*01  POST -COPY W224LI12 -PRE  LI12-  -L.                                 
007800                                                                          
007900                                                                          
008000 FD  W22413                                                               
008100     RECORDING       V                                                    
008200     BLOCK CONTAINS  0.                                                   
008300                                                                          
008400*01  UPD-POST -COPY W22413    -L.                                         
008500                                                                          
008600                                                                          
008700 WORKING-STORAGE SECTION.                                                 
008800     SKIP2                                                                
008900*    -COPY WY2000W2                                                       
009000     SKIP3                                                                
009100                                                                          
009200 77  IDPGM                       PIC X(8)    VALUE 'W2241200'.            
009300 77  JA                          PIC X       VALUE 'J'.                   
009400 77  NEJ                         PIC X       VALUE 'N'.                   
009500                                                                          
009600 77  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
009700 77  CURRENT-IMS-SECTION         PIC X(32)   VALUE SPACE.                 
009800                                                                          
009900 77  W22418-EOF-SW               PIC X       VALUE 'N'.                   
010000     88  END-OF-W22418                       VALUE 'J'.                   
010100                                                                          
010200 77  W22410-EOF-SW               PIC X       VALUE 'N'.                   
010300     88  END-OF-W22410                       VALUE 'J'.                   
010400     88  NOT-END-OF-W22410                   VALUE 'N'.                   
010500                                                                          
010600 01  UPD-FAELT.                                                           
010700*    FRÅN PUNK                                                            
010800     03  WUPD-KVEOQ              PIC S9(7)   COMP-3 VALUE ZERO.           
010900     03  WUPD-KVREFBER           PIC S9(7)   COMP-3 VALUE ZERO.           
011000     03  WUPD-KVREFPKT           PIC S9(7)   COMP-3 VALUE ZERO.           
011100     03  WUPD-KVREFOVL           PIC S9(7)   COMP-3 VALUE ZERO.           
011200     03  WUPD-KVSLAGER           PIC S9(7)   COMP-3 VALUE ZERO.           
011300     03  WUPD-TIMANSEC           PIC S9(7)   COMP-3 VALUE ZERO.           
011400     03  WUPD-TIREFPAF           PIC S9(7)   COMP-3 VALUE ZERO.           
011500     03  WUPD-TIREFPKT           PIC S9(7)   COMP-3 VALUE ZERO.           
011600                                                                          
011700     03  WUPD-DAAVROP-AVS        PIC  9(6)   VALUE ZERO.                  
011800     03  WUPD-DAPBPLAN           PIC  9(8)   VALUE ZERO.                  
011900     03  WUPD-DASEASON           PIC 9(8)    VALUE ZERO.                  
012000     03  WUPD-KDLEVPLF           PIC X(1)    VALUE SPACE.                 
012100     03  WUPD-KDLPSP             PIC S9      COMP-3 VALUE ZERO.           
012200     03  WUPD-KVPB-PLAN          PIC S9(6)V9 COMP-3 VALUE ZERO.           
012300     03  WUPD-KVSLUTKP           PIC S9(7)   COMP-3 VALUE ZERO.           
012400     03  WUPD-RESEASON.                                                   
012500         05  WUPD-RESEASON-PLAN  OCCURS 12                                
012600                                 PIC S9V9(2) COMP-3 VALUE ZERO.           
012700     03  WUPD-TILEVDAG           PIC S9(1)   COMP-3 VALUE ZERO.           
012800     03  WUPD-TILPSP             PIC S9(5)   COMP-3 VALUE ZERO.           
012900                                                                          
013000 01  WC-IDPTYP.                                                           
013100     03  UPDATE-WDK711           PIC X(3)    VALUE '001'.                 
013200     03  UPDATE-WDK722           PIC X(3)    VALUE '002'.                 
013300     03  INSERT-WDD901           PIC X(3)    VALUE '003'.                 
013400     03  INSERT-WDD902           PIC X(3)    VALUE '004'.                 
013500     03  DELETE-WDD901           PIC X(3)    VALUE '008'.                 
013600     03  DELETE-WDD904           PIC X(3)    VALUE '009'.                 
013700     03  DELETE-WDD601           PIC X(3)    VALUE '010'.                 
013800     03  DELETE-WDD905           PIC X(3)    VALUE '011'.                 
013900     03  REPLACE-WDD905          PIC X(3)    VALUE '012'.                 
014000     03  INSERT-WDD905           PIC X(3)    VALUE '013'.                 
014100                                                                          
014200 01  IX                          PIC 9(9)    VALUE ZERO.                  
014300 01  IX-DAG                      PIC 9       VALUE ZERO.                  
014400 01  IX-DAG-MAX                  PIC 9       VALUE 5.                     
014500 01  IX-PUNK                     PIC 9(9)    VALUE ZERO.                  
014600 01  IX-PUNK-MAX                 PIC 9(9)    VALUE 12.                    
014700 01  IX-BHDC                     PIC 9(03)   VALUE ZERO.                  
014800 01  IX-BHDC-MAX                 PIC 9(03)   VALUE 156.                   
014900                                                                          
015000                                                                          
015100 01  IDDC-INDEX-WS.                                                       
015200     03 DC-MAX           PIC 9(3)    VALUE 100.                           
015300                                                                          
015400     03 WDCIX            PIC 9(3)    VALUE ZERO.                          
015500     03 DCS-TRAEFF       PIC X       VALUE 'J'.                           
015600                                                                          
015700 01  IDDC-TABELL.                                                         
015800     03 DC-TAB  OCCURS 1 TO 100 DEPENDING ON DC-MAX                       
015900                INDEXED BY DCIX.                                          
016000        05 T-DCS.                                                         
016100          07 T-DCS-IDDC           PIC X(2).                               
016200          07 T-DCS-KDDC           PIC X(2).                               
016300          07 T-DCS-FLOVRLAGBER    PIC X.                                  
016400                                                                          
016500 01  TAB2204-TABELL.                                                      
016600     03  TAB2204-MAX              PIC S9(9)  COMP-3 VALUE 25000.          
016700                                                                          
016800     03  TAB2204-INGANG   OCCURS 25000 DEPENDING ON TAB2204-MAX           
016900                 ASCENDING KEY IS TAB2204-IDDC TAB2204-IDARTNR            
017000                             INDEXED BY TAB2204-IX.                       
017100         05  TAB2204-IDARTNR      PIC S9(9)  COMP-3.                      
017200         05  TAB2204-IDDC         PIC X(2).                               
017300         05  TAB2204-KDLPORS-1    PIC 9(2).                               
017400         05  TAB2204-KDLPORS-2    PIC 9(2).                               
017500         05  TAB2204-KDLPORS-3    PIC 9(2).                               
017600                                                                          
017700 01  TILLGANGSTABELL.                                                     
017800     03  TILLGTAB-MAX        PIC S9(9)   VALUE +156  COMP SYNC.           
017900     03  TILLGTAB-IX         PIC S9(9)   VALUE +0    COMP SYNC.           
018000     SKIP1                                                                
018100     03  TILLGTAB.                                                        
018200         05  TILLGTAB-INGANG OCCURS 156.                                  
018300             10  TILLGTAB-ANTAL                                           
018400                             PIC S9(7)V99            COMP-3.              
018500                                                                          
018600 01  W-HELA-DATUMET               PIC 9(8).                               
018700 01  FILLER              REDEFINES W-HELA-DATUMET.                        
018800     03  W-HELA-DATUMET-SEKEL     PIC 9(2).                               
018900     03  W-HELA-DATUMET-AAMMDD    PIC 9(6).                               
019000 01  W-DATUM-AAVVD                PIC 9(5).                               
019100 01  W-DATUM-AAVV                 PIC 9(4).                               
019200 01  W-DAT-AAVV    REDEFINES W-DATUM-AAVV.                                
019300     03  W-DATUM-AA               PIC 9(2).                               
019400     03  W-DATUM-VV               PIC 9(2).                               
019500 01  W-DATUM-AAMMDD               PIC 9(6).                               
019600 01  W-DAT2-AAMMDD REDEFINES W-DATUM-AAMMDD.                              
019700     03  W-DATUM2-AA              PIC 9(2).                               
019800     03  W-DATUM2-MM              PIC 9(2).                               
019900     03  W-DATUM2-DD              PIC 9(2).                               
020000 01  AKT-DATUM-AAVV               PIC 9(4).                               
020100 01  FILLER REDEFINES AKT-DATUM-AAVV.                                     
020200     03  AKT-DATUM-AA             PIC 9(2).                               
020300     03  AKT-DATUM-VV             PIC 9(2).                               
020400 01  W-DAPUBL                     PIC 9(8).                               
020500 01  FILLER REDEFINES W-DAPUBL.                                           
020600     03  W-DAPUBL-SS              PIC 9(2).                               
020700     03  W-DAPUBL-AAVVDD          PIC 9(6).                               
020800 01  W-TIFINLV-AAVV               PIC 9(4).                               
020900 01  W-TIFINLV                    PIC 9(5).                               
021000 01  FILLER REDEFINES W-TIFINLV.                                          
021100     03  W-TIFINLV-AA             PIC 9(2).                               
021200     03  W-TIFINLV-VVD            PIC 9(3).                               
021300     03  FILLER REDEFINES W-TIFINLV-VVD.                                  
021400         05  W-TIFINLV-VV         PIC 9(2).                               
021500         05  W-TIFINLV-D          PIC 9(1).                               
021600 01  W-DATUM-FOM                  PIC 9(4).                               
021700 01  FILLER REDEFINES W-DATUM-FOM.                                        
021800     03  W-DATUM-FOM-AA           PIC 9(2).                               
021900     03  W-DATUM-FOM-VV           PIC 9(2).                               
022000 01  W-DATUM-TOM                  PIC 9(4).                               
022100 01  FILLER REDEFINES W-DATUM-TOM.                                        
022200     03  W-DATUM-TOM-AA           PIC 9(2).                               
022300     03  W-DATUM-TOM-VV           PIC 9(2).                               
022400 01  W-HELP-DATUM-SSAAVVD.                                                
022500     03  W-HELP-DATUM-SSAA        PIC 9(4).                               
022600     03  FILLER REDEFINES W-HELP-DATUM-SSAA.                              
022700         05  W-HELP-DATUM-SS      PIC 9(2).                               
022800         05  W-HELP-DATUM-AA      PIC 9(2).                               
022900     03  W-HELP-DATUM-VV          PIC 9(2).                               
023000     03  W-HELP-DATUM-D           PIC 9(1).                               
023100 01  W-HELP-TIFINLV-SSAAVVD.                                              
023200     03  W-HELP-TIFINLV-SSAA        PIC 9(4).                             
023300     03  FILLER REDEFINES W-HELP-TIFINLV-SSAA.                            
023400         05  W-HELP-TIFINLV-SS    PIC 9(2).                               
023500         05  W-HELP-TIFINLV-AA    PIC 9(2).                               
023600     03  W-HELP-TIFINLV-VV        PIC 9(2).                               
023700     03  W-HELP-TIFINLV-D         PIC 9(1).                               
023800 01  W-VECKO-DIFF                 PIC 9(5)  VALUE ZERO.                   
023900 01  W-ANTAL-VECKOR               PIC S9(3)          COMP-3.              
024000 01  W-DATUM-GRAENS               PIC S9(5)          COMP-3.              
024100 01  W-TILLGANG-NDC               PIC 9(7)  VALUE ZERO  COMP-3.           
024200 01  W-KVSLUTKP-DC                PIC 9(7)  VALUE ZERO  COMP-3.           
024300 01  W-KVPB-TREND-VECKA           PIC S9(6)V9(1)     COMP-3.              
024400 01  W-TILLG-SDC                  PIC S9(7)          COMP-3.              
024500 01  W-OVERLAGER-SDC              PIC S9(7)          COMP-3.              
024600 01  W-KVOKS                      PIC S9(7) VALUE ZERO  COMP-3.           
024700 01  W-FRYSTIDP                   PIC S9(5)          COMP-3.              
024800 01  W-ANT-BRIST-VECKOR           PIC S9(3)          COMP-3.              
024900 01  W-ANT-OVERSK-VECKOR          PIC S9(3)          COMP-3.              
025000 01  W-TIAAVV                     PIC 9(4)   VALUE ZERO.                  
025100 01  W-TIAAVV-FT                  PIC S9(5)          COMP-3.              
025200 01  SW-INLEV-UNDER-PERIODEN      PIC X      VALUE 'N'.                   
025300 01  SW-OMSPEC-MINSKNING          PIC X      VALUE 'N'.                   
025400 01  W-TILLG                      PIC S9(7)V99       COMP-3.              
025500 01  W-OVRE-GRANS                 PIC S9(7)V99       COMP-3.              
025600 01  W-UDDATEST                   PIC 9(4)V9.                             
025700 01  FILLER REDEFINES W-UDDATEST.                                         
025800     03  FILLER                   PIC 9(4).                               
025900     03  RESTEN                   PIC 9.                                  
026000 01  W-FRAMFORHALLNING            PIC S9(3)          COMP-3.              
026100 01  W-DIFF                       PIC S9(7)V99       COMP-3.              
026200 01  W-REST                       PIC S9V99          COMP-3.              
026300 01  W-ARS-OMS                    PIC S9(9)V9(2)     COMP-3.              
026400 01  W-TIAVRDAT-INL               PIC S9(5)   COMP-3 VALUE ZERO.          
026500 01  W-TIAVRDAT-DISP              PIC S9(5)   COMP-3 VALUE ZERO.          
026600                                                                          
026700*01  -COPY WWPRODSL                                                       
026800                                                                          
026900*01  -COPY WZ20DAYS                                                       
027000 01  W-TIAAVV-AKTUELL             PIC 9(4)   VALUE ZERO.                  
027100 01  W-DATUM-GRAENS-AAVV          PIC 9(4)   VALUE ZERO.                  
027200 01  W-TIAAAAVV-AKTUELL           PIC 9(6)   VALUE ZERO.                  
027300 01  W-DATUM-GRAENS-AAAAVV        PIC 9(6)   VALUE ZERO.                  
027400                                                                          
027500                                                                          
027600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
027700 01  FILLER REDEFINES DAGENS-DATUM.                                       
027800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
027900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
028000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
028100                                                                          
028200 01  DYNAMISKA-SUBPROGRAM.                                                
028300*                                                                         
028400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
028500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
028600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
028700     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
028800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
028900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
029000     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
029100     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
029200     03  W221LPAD                PIC X(8)    VALUE 'W221LPAD'.            
029300     03  W222BHDC                PIC X(8)    VALUE 'W222BHDC'.            
029400     03  W224PUNK                PIC X(8)    VALUE 'W224PUNK'.            
029500     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
029600                                                                          
029700*    --- VARIABLER TILL SUBPROGRAM W221LPAD                               
029800 01  W-W221LP-CTX                PIC X(08) VALUE 'W221LP02'.              
029900 01  W-KDLPORS-GRP.                                                       
030000     03 W-KDLPORS-TAB OCCURS 4   PIC 9(3).                                
030100                                                                          
030200*    --- PARAMETRAR TILL ABEND                                            
030300                                                                          
030400 77  RKOD                        PIC S9(4)   COMP VALUE +0.               
030500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
030600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
030700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
030800                                                                          
030900 01  FELTEXT.                                                             
031000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
031100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
031200                                                                          
031300*                            *** PARAMETRAR TILL DATUMKORT                
031400 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W22140'.                  
031500 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
031600*    -COPY WDATKORT.                                                      
031700                                                                          
031800*                        ****    PARAMETRAR TILL WDATKONV                 
031900 01  FILLER                      PIC X(16) VALUE 'WDATAREA'.              
032000*01      -COPY WDATAREA                                                   
032100                                                                          
032200*    --- PARAMETRAR TILL POSTSUM                                          
032300*                                                                         
032400*01  -COPY W0005   -PRE  POSTSUM-                                         
032500                                                                          
032600                                                                          
032700*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
032800 01  TABENTRY-PARM.                                                       
032900     03  STEGLANGD               PIC S9(9) COMP.                          
033000     03  ANTAL                   PIC S9(9) COMP.                          
033100     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 9.                 
033200                                                                          
033300*    --- LÄNKAREA TILL W222BHDC                                           
033400*01  -COPY W222BHDC -PRE BHDC-                                            
033500*    --- SPARAD LÄNKAREA TILL W222BHDC                                    
033600*01  -COPY W222BHDC -PRE SPAR-BHDC-                                       
033700 01  PB-TOTAL-SEP-LEV-XDC        PIC X(2)   VALUE '03'.                   
033800 01  XDC-CDC-BEHOV               PIC X(2)   VALUE '05'.                   
033900                                                                          
034000*    --- LÄNKAREA TILL SUBPROGRAM W224PUNK                                
034100*01 -COPY W224PUNK                                                        
034200                                                                          
034300                                                                          
034400 01  NDCCN-AREA-START            PIC X(24)   VALUE                        
034500                                 'NDCCN-AREA-START '.                     
034600                                                                          
034700*01  NDCCN-AREA -COPY W22418                                              
034800                                                                          
034900                                                                          
035000 01  OMSP-AREA-START             PIC X(24)   VALUE                        
035100                                 'OMSP-AREA-START  '.                     
035200                                                                          
035300*01  AREA -COPY W2242204     -PRE OMSP-                                   
035400                                                                          
035500                                                                          
035600 01  LI12-AREA-START             PIC X(24)   VALUE                        
035700                                 'LI12-AREA-START  '.                     
035800                                                                          
035900*01  AREA -COPY W224LI12     -PRE LI12-                                   
036000                                                                          
036100                                                                          
036200 01  UPD-AREA-START              PIC X(24)   VALUE                        
036300                                 'UPD-AREA-START  '.                      
036400 01  UPD-AREA.                                                            
036500*    03  FILLER -COPY W22413                                              
036600                                                                          
036700                                                                          
036800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
036900*                                                                         
037000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
037100                                                                          
037200 01  NYCKLAR-TILL-DLI.                                                    
037300     03  W-IDARTNR-X.                                                     
037400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
037500     03  W-IDDC-X.                                                        
037600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
037700     03  W-IDDC-K7-MIN-X.                                                 
037800         05  W-IDDC-K7-MIN       PIC X(2)    VALUE LOW-VALUE.             
037900     03  W-IDDC-K7-MAX-X.                                                 
038000         05  W-IDDC-K7-MAX       PIC X(2)    VALUE HIGH-VALUE.            
038100     03  W-IDDC-REF-X.                                                    
038200         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
038300     03  W-WDD901KY-X.                                                    
038400         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
038500         05  W-IDDC-D9           PIC  X(2)   VALUE SPACE.                 
038600     03  W-IDLEVNR-X.                                                     
038700         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
038800     03  W-DAAVROP-X.                                                     
038900         05  W-DAAVROP           PIC  9(6)    VALUE ZERO.                 
039000                                                                          
039100     03  W-KDAVROP-X.                                                     
039200         05  W-KDAVROP           PIC S9(1) VALUE ZERO COMP-3.             
039300                                                                          
039400*    --- STATUS-KOD FRÅN IMS                                              
039500 01  STATUS-WS                   PIC XX.                                  
039600     88  SEGMENT-FINNS                       VALUE '  '.                  
039700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
039800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
039900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
040000                                                                          
040100 01  GODK-STATUSKODER.                                                    
040200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
040300                                                                          
040400 01  ALL-SSA.                                                             
040500     03 SSA1                     PIC X(64).                               
040600     03 SSA2                     PIC X(64).                               
040700     03 SSA3                     PIC X(64).                               
040800                                                                          
040900*    --- IMS FUNKTIONSKODER                                               
041000*01  -COPY W0003                                                          
041100                                                                          
041200                                                                          
041300*    ---  DLI INPUT-OUTPUT AREA                                           
041400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
041500 01  DLI-IO-WDK701.                                                       
041600*    03  -COPY WDK701                                                     
041700                                                                          
041800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
041900 01  DLI-IO-WDK711.                                                       
042000*    03  -COPY WDK711                                                     
042100                                                                          
042200                                                                          
042300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
042400 01  DLI-IO-WDD901.                                                       
042500*    03  -COPY WDD901 -PRE D901-                                          
042600                                                                          
042700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
042800 01  DLI-IO-WDD902.                                                       
042900*    03  -COPY WDD902 -PRE D902-                                          
043000                                                                          
043100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD904'.                      
043200 01  DLI-IO-WDD904.                                                       
043300*    03  -COPY WDD904 -PRE D904-                                          
043400                                                                          
043500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
043600 01  DLI-IO-WDD905.                                                       
043700*    03  -COPY WDD905 -PRE D905-                                          
043800                                                                          
043900                                                                          
044000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
044100 01  DLI-IO-WDB601.                                                       
044200*    03  -COPY WDB601                                                     
044300                                                                          
044400                                                                          
044500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD601'.                      
044600 01  DLI-IO-WDD601.                                                       
044700*    03  -COPY WDD601                                                     
044800                                                                          
044900                                                                          
045000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL601'.                      
045100 01  DLI-IO-WDL601.                                                       
045200*    03  -COPY WDL601                                                     
045300                                                                          
045400                                                                          
045500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
045600 01  DLI-IO-WDF101.                                                       
045700*    03  -COPY WDF101                                                     
045800                                                                          
045900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF116'.                      
046000 01  DLI-IO-WDF116.                                                       
046100*    03  -COPY WDF116                                                     
046200                                                                          
046300                                                                          
046400                                                                          
046500 LINKAGE SECTION.                                                         
046600                                                                          
046700*01  -COPY W0008  -PRE WDK7-                                              
046800     05  FILLER                   PIC X.                                  
046900                                                                          
047000*01  -COPY W0008  -PRE WDD9-                                              
047100     05  FILLER                   PIC X(7).                               
047200     05  WDD9-KEY-02-IDLEVNR      PIC X(5).                               
047300                                                                          
047400*01  -COPY W0008  -PRE WDB6-                                              
047500     05  FILLER                   PIC X.                                  
047600                                                                          
047700*01  -COPY W0008  -PRE WDD6-                                              
047800     05  FILLER                   PIC X.                                  
047900                                                                          
048000*01  -COPY W0008  -PRE WDL6-                                              
048100     05  FILLER                   PIC X.                                  
048200                                                                          
048300*01  -COPY W0008  -PRE WDF1-                                              
048400     05  FILLER                   PIC X.                                  
048500                                                                          
048600*    PROGRAM W222BHDC                                                     
048700 01  BHDC-WDK6-PCB                PIC X.                                  
048800 01  BHDC-WDK7-PCB                PIC X.                                  
048900 01  BHDC-WDB6-PCB                PIC X.                                  
049000 01  BHDC-WDR2-PCB                PIC X.                                  
049100 01  BHDC-WDD7-PCB                PIC X.                                  
049200 01  BHDC-WDK7E-PCB               PIC X.                                  
049300 01  BHDC-WDD7-2-PCB              PIC X.                                  
049400 01  BHDC-WDK9-PCB                PIC X.                                  
049500 01  BHDC-REFL1-2501-PCB          PIC X.                                  
049600 01  BHDC-REFL1-WDB6-PCB          PIC X.                                  
049700 01  BHDC-REFL1-WDK7-PCB          PIC X.                                  
049800 01  BHDC-REFL1-UTIL-WDK6-PCB     PIC X.                                  
049900 01  BHDC-REFL1-UTIL-WDK7-PCB     PIC X.                                  
050000 01  BHDC-REFL1-UTIL-WDB6-PCB     PIC X.                                  
050100     EJECT                                                                
050200 01  BHDC-REFL2-2501-PCB          PIC X.                                  
050300 01  BHDC-REFL2-WDB6-PCB          PIC X.                                  
050400 01  BHDC-REFL2-UTIL-WDK6-PCB     PIC X.                                  
050500 01  BHDC-REFL2-UTIL-WDK7-PCB     PIC X.                                  
050600 01  BHDC-REFL2-UTIL-WDB6-PCB     PIC X.                                  
050700     EJECT                                                                
050800 01  BHDC-UTIL-WDK6-PCB           PIC X.                                  
050900 01  BHDC-UTIL-WDK7-PCB           PIC X.                                  
051000 01  BHDC-UTIL-WDB6-PCB           PIC X.                                  
051100     EJECT                                                                
051200 01  BHDC-W222-WDK6-PCB           PIC X.                                  
051300 01  BHDC-W222-WDK7-PCB           PIC X.                                  
051400 01  BHDC-W222-ARTM-PCB           PIC X.                                  
051500 01  BHDC-W222-2501-PCB           PIC X.                                  
051600 01  BHDC-W222-WDB6R-PCB          PIC X.                                  
051700 01  BHDC-W222-WDK7R-PCB          PIC X.                                  
051800 01  BHDC-W222-WDB6-PCB           PIC X.                                  
051900 01  BHDC-W222-WDD7-PCB           PIC X.                                  
052000 01  BHDC-W222-WDK7E-PCB          PIC X.                                  
052100 01  BHDC-W222-UTIL-WDK6-PCB      PIC X.                                  
052200 01  BHDC-W222-UTIL-WDK7-PCB      PIC X.                                  
052300 01  BHDC-W222-UTIL-WDB6-PCB      PIC X.                                  
052400 01  BHDC-W222-UTUP-WDK7-PCB      PIC X.                                  
052500 01  BHDC-W222-UTUP-WDB6-PCB      PIC X.                                  
052600 01  BHDC-W222-UTUP-UTIL-WDK6-PCB PIC X.                                  
052700 01  BHDC-W222-UTUP-UTIL-WDK7-PCB PIC X.                                  
052800 01  BHDC-W222-UTUP-UTIL-WDB6-PCB PIC X.                                  
052900     EJECT                                                                
053000 01  BHDC-UTUP-WDK7-PCB           PIC X.                                  
053100 01  BHDC-UTUP-WDB6-PCB           PIC X.                                  
053200 01  BHDC-UTUP-UTIL-WDK6-PCB      PIC X.                                  
053300 01  BHDC-UTUP-UTIL-WDK7-PCB      PIC X.                                  
053400 01  BHDC-UTUP-UTIL-WDB6-PCB      PIC X.                                  
053500     EJECT                                                                
053600                                                                          
053700*    PROGRAM W224PUNK                                                     
053800 01  PUNK-REFL1-2501-PCB          PIC X.                                  
053900 01  PUNK-REFL1-WDB6-PCB          PIC X.                                  
054000 01  PUNK-REFL1-WDK7-PCB          PIC X.                                  
054100 01  PUNK-REFL1-UTIL-WDK6-PCB     PIC X.                                  
054200 01  PUNK-REFL1-UTIL-WDK7-PCB     PIC X.                                  
054300 01  PUNK-REFL1-UTIL-WDB6-PCB     PIC X.                                  
054400 01  PUNK-UTUP1-WDK7-PCB          PIC X.                                  
054500 01  PUNK-UTUP1-WDB6-PCB          PIC X.                                  
054600 01  PUNK-UTUP1-UTIL-WDK6-PCB     PIC X.                                  
054700 01  PUNK-UTUP1-UTIL-WDK7-PCB     PIC X.                                  
054800 01  PUNK-UTUP1-UTIL-WDB6-PCB     PIC X.                                  
054900                                                                          
055000                                                                          
055100 PROCEDURE DIVISION  USING  WDK7-PCB WDD9-PCB WDB6-PCB                    
055200                            WDD6-PCB WDL6-PCB WDF1-PCB                    
055300                                                                          
055400                            BHDC-WDK6-PCB   BHDC-WDK7-PCB                 
055500                            BHDC-WDB6-PCB   BHDC-WDR2-PCB                 
055600                            BHDC-WDD7-PCB   BHDC-WDK7E-PCB                
055700                            BHDC-WDD7-2-PCB BHDC-WDK9-PCB                 
055800                            BHDC-REFL1-2501-PCB                           
055900                            BHDC-REFL1-WDB6-PCB                           
056000                            BHDC-REFL1-WDK7-PCB                           
056100                            BHDC-REFL1-UTIL-WDK6-PCB                      
056200                            BHDC-REFL1-UTIL-WDK7-PCB                      
056300                            BHDC-REFL1-UTIL-WDB6-PCB                      
056400                            BHDC-REFL2-2501-PCB                           
056500                            BHDC-REFL2-WDB6-PCB                           
056600                            BHDC-REFL2-UTIL-WDK6-PCB                      
056700                            BHDC-REFL2-UTIL-WDK7-PCB                      
056800                            BHDC-REFL2-UTIL-WDB6-PCB                      
056900                            BHDC-UTIL-WDK6-PCB                            
057000                            BHDC-UTIL-WDK7-PCB                            
057100                            BHDC-UTIL-WDB6-PCB                            
057200                            BHDC-W222-WDK6-PCB                            
057300                            BHDC-W222-WDK7-PCB                            
057400                            BHDC-W222-ARTM-PCB                            
057500                            BHDC-W222-2501-PCB                            
057600                            BHDC-W222-WDB6R-PCB                           
057700                            BHDC-W222-WDK7R-PCB                           
057800                            BHDC-W222-WDB6-PCB                            
057900                            BHDC-W222-WDD7-PCB                            
058000                            BHDC-W222-WDK7E-PCB                           
058100                            BHDC-W222-UTIL-WDK6-PCB                       
058200                            BHDC-W222-UTIL-WDK7-PCB                       
058300                            BHDC-W222-UTIL-WDB6-PCB                       
058400                            BHDC-W222-UTUP-WDK7-PCB                       
058500                            BHDC-W222-UTUP-WDB6-PCB                       
058600                            BHDC-W222-UTUP-UTIL-WDK6-PCB                  
058700                            BHDC-W222-UTUP-UTIL-WDK7-PCB                  
058800                            BHDC-W222-UTUP-UTIL-WDB6-PCB                  
058900                            BHDC-UTUP-WDK7-PCB                            
059000                            BHDC-UTUP-WDB6-PCB                            
059100                            BHDC-UTUP-UTIL-WDK6-PCB                       
059200                            BHDC-UTUP-UTIL-WDK7-PCB                       
059300                            BHDC-UTUP-UTIL-WDB6-PCB                       
059400                                                                          
059500                            PUNK-REFL1-2501-PCB                           
059600                            PUNK-REFL1-WDB6-PCB                           
059700                            PUNK-REFL1-WDK7-PCB                           
059800                            PUNK-REFL1-UTIL-WDK6-PCB                      
059900                            PUNK-REFL1-UTIL-WDK7-PCB                      
060000                            PUNK-REFL1-UTIL-WDB6-PCB                      
060100                            PUNK-UTUP1-WDK7-PCB                           
060200                            PUNK-UTUP1-WDB6-PCB                           
060300                            PUNK-UTUP1-UTIL-WDK6-PCB                      
060400                            PUNK-UTUP1-UTIL-WDK7-PCB                      
060500                            PUNK-UTUP1-UTIL-WDB6-PCB                      
060600                            .                                             
060700 MAIN SECTION.                                                            
060800     ENTRY 'DLITCBL' USING  WDK7-PCB WDD9-PCB WDB6-PCB                    
060900                            WDD6-PCB WDL6-PCB WDF1-PCB                    
061000                                                                          
061100                            BHDC-WDK6-PCB   BHDC-WDK7-PCB                 
061200                            BHDC-WDB6-PCB   BHDC-WDR2-PCB                 
061300                            BHDC-WDD7-PCB   BHDC-WDK7E-PCB                
061400                            BHDC-WDD7-2-PCB BHDC-WDK9-PCB                 
061500                            BHDC-REFL1-2501-PCB                           
061600                            BHDC-REFL1-WDB6-PCB                           
061700                            BHDC-REFL1-WDK7-PCB                           
061800                            BHDC-REFL1-UTIL-WDK6-PCB                      
061900                            BHDC-REFL1-UTIL-WDK7-PCB                      
062000                            BHDC-REFL1-UTIL-WDB6-PCB                      
062100                            BHDC-REFL2-2501-PCB                           
062200                            BHDC-REFL2-WDB6-PCB                           
062300                            BHDC-REFL2-UTIL-WDK6-PCB                      
062400                            BHDC-REFL2-UTIL-WDK7-PCB                      
062500                            BHDC-REFL2-UTIL-WDB6-PCB                      
062600                            BHDC-UTIL-WDK6-PCB                            
062700                            BHDC-UTIL-WDK7-PCB                            
062800                            BHDC-UTIL-WDB6-PCB                            
062900                            BHDC-W222-WDK6-PCB                            
063000                            BHDC-W222-WDK7-PCB                            
063100                            BHDC-W222-ARTM-PCB                            
063200                            BHDC-W222-2501-PCB                            
063300                            BHDC-W222-WDB6R-PCB                           
063400                            BHDC-W222-WDK7R-PCB                           
063500                            BHDC-W222-WDB6-PCB                            
063600                            BHDC-W222-WDD7-PCB                            
063700                            BHDC-W222-WDK7E-PCB                           
063800                            BHDC-W222-UTIL-WDK6-PCB                       
063900                            BHDC-W222-UTIL-WDK7-PCB                       
064000                            BHDC-W222-UTIL-WDB6-PCB                       
064100                            BHDC-W222-UTUP-WDK7-PCB                       
064200                            BHDC-W222-UTUP-WDB6-PCB                       
064300                            BHDC-W222-UTUP-UTIL-WDK6-PCB                  
064400                            BHDC-W222-UTUP-UTIL-WDK7-PCB                  
064500                            BHDC-W222-UTUP-UTIL-WDB6-PCB                  
064600                            BHDC-UTUP-WDK7-PCB                            
064700                            BHDC-UTUP-WDB6-PCB                            
064800                            BHDC-UTUP-UTIL-WDK6-PCB                       
064900                            BHDC-UTUP-UTIL-WDK7-PCB                       
065000                            BHDC-UTUP-UTIL-WDB6-PCB                       
065100                                                                          
065200                            PUNK-REFL1-2501-PCB                           
065300                            PUNK-REFL1-WDB6-PCB                           
065400                            PUNK-REFL1-WDK7-PCB                           
065500                            PUNK-REFL1-UTIL-WDK6-PCB                      
065600                            PUNK-REFL1-UTIL-WDK7-PCB                      
065700                            PUNK-REFL1-UTIL-WDB6-PCB                      
065800                            PUNK-UTUP1-WDK7-PCB                           
065900                            PUNK-UTUP1-WDB6-PCB                           
066000                            PUNK-UTUP1-UTIL-WDK6-PCB                      
066100                            PUNK-UTUP1-UTIL-WDK7-PCB                      
066200                            PUNK-UTUP1-UTIL-WDB6-PCB                      
066300                            .                                             
066400                                                                          
066500     PERFORM A-INIT                                                       
066600                                                                          
066700     PERFORM S01-LAES-W22418                                              
066800     PERFORM UNTIL END-OF-W22418                                          
066900                                                                          
067000       MOVE NDCCN-IDARTNR  TO W-IDARTNR                                   
067100       PERFORM IMS-GU-WDK701                                              
067200                                                                          
067300       IF SEGMENT-FINNS                                                   
067400           PERFORM B-SPARA-INDATA                                         
067500           PERFORM C-BERAKNA-TILLGANGAR                                   
067600           PERFORM D-LAES-DATA                                            
067700           PERFORM E-DIVERSE-KONTROLLER                                   
067800           PERFORM F-PUNKTBERAKNING                                       
067900           IF W-KDLPORS-TAB(1) = +18                                      
068000           OR W-KDLPORS-TAB(2) = +18                                      
068100           OR W-KDLPORS-TAB(3) = +18                                      
068200              CONTINUE                                                    
068300           ELSE                                                           
068400              PERFORM H-EXTRALEVERANS-KONTROLL                            
068500           END-IF                                                         
068600           PERFORM I-KONTROLL-KORR-LEVPLAN                                
068700           PERFORM J-SKAPA-OMSPEC-BEGARAN                                 
068800           PERFORM K-KONTROLL-EJ-AUT-GODK-FORSLAG                         
068900                                                                          
069000           PERFORM L-KOLLA-SKRIV-WDK7-UPPDATERING                         
069100       END-IF                                                             
069200                                                                          
069300       PERFORM S01-LAES-W22418                                            
069400     END-PERFORM                                                          
069500                                                                          
069600                                                                          
069700     PERFORM Z-FINIT                                                      
069800                                                                          
069900     MOVE ZERO TO RETURN-CODE                                             
070000     GOBACK                                                               
070100     .                                                                    
070200                                                                          
070300                                                                          
070400 A-INIT SECTION.                                                          
070500     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
070600                                                                          
070700     OPEN INPUT  W22418                                                   
070800                 W22410                                                   
070900                                                                          
071000     OPEN OUTPUT W22412                                                   
071100                 W22413                                                   
071200                                                                          
071300     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
071400     MOVE D-AAR    TO W-DATUM-AA W-DATUM2-AA AKT-DATUM-AA                 
071500     MOVE D-MAANAD TO W-DATUM2-MM                                         
071600     MOVE D-DAG    TO W-DATUM2-DD                                         
071700     MOVE D-VECKA  TO W-DATUM-VV           AKT-DATUM-VV                   
071800     COMPUTE W-DATUM-AAVVD = W-DATUM-AAVV * 10                            
071900                                                                          
072000     MOVE W-DATUM-AAMMDD TO DAT-I-TIDATUM                                 
072100     MOVE 'AAMMDD'       TO DAT-KDDATFORM                                 
072200     CALL WDATKONV USING DAT-KDDATFORM,                                   
072300                         DAT-I-TIDATUM,                                   
072400                         DAT-O-TIDATUM,                                   
072500                         DAT-KDSVAR                                       
072600     MOVE DAT-TISEKEL    TO W-HELA-DATUMET-SEKEL                          
072700     MOVE W-DATUM-AAMMDD TO W-HELA-DATUMET-AAMMDD                         
072800                                                                          
072900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
073000                                                                          
073100     PERFORM AA-FYLL-DC-TABELL                                            
073200     PERFORM AB-LAES-HAENDELSER                                           
073300     .                                                                    
073400                                                                          
073500                                                                          
073600 AA-FYLL-DC-TABELL SECTION.                                               
073700     MOVE 'AA-FYLL-DC-TAB  ' TO CURRENT-SECTION                           
073800                                                                          
073900* FYLLER DC-TABELLEN MED ALLA DATABAS-RECORD FRÅN WDB6                    
074000* FÖR ATT SLIPPA BAS-LÄSNING FÖR VARJE ART I INFILEN.                     
074100                                                                          
074200     SET DCIX TO +1                                                       
074300     PERFORM IMS-GN-WDB601                                                
074400     PERFORM UNTIL SEGMENT-SLUT                                           
074500                                                                          
074600        IF DCIX <= DC-MAX                                                 
074700           MOVE DCS-IDDC TO T-DCS-IDDC(DCIX)                              
074800           MOVE DCS-KDDC TO T-DCS-KDDC(DCIX)                              
074900           MOVE DCS-FLOVRLAGBER TO T-DCS-FLOVRLAGBER(DCIX)                
075000           SET DCIX UP BY +1                                              
075100           PERFORM IMS-GN-WDB601                                          
075200        ELSE                                                              
075300                                                                          
075400           MOVE 35 TO RKOD                                                
075500           MOVE 'DC-TABELL SLUT. ÖKA DC-MAX' TO FELTEXT-STR               
075600           DISPLAY FELTEXT                                                
075700           CALL ABEND USING RKOD                                          
075800        END-IF                                                            
075900     END-PERFORM                                                          
076000                                                                          
076100*****  --- SÄTTER TAKET PÅ TABELLEN                                       
076200     SET DCIX  DOWN BY +1                                                 
076300     SET DC-MAX TO DCIX                                                   
076400                                                                          
076500*    --- SORTERA TABELLEN PÅ IDDC, FÖR ATT SEARCH SKA FUNKA               
076600     MOVE DC-MAX                  TO ANTAL                                
076700     MOVE LENGTH OF T-DCS(1)      TO STEGLANGD                            
076800     MOVE LENGTH OF T-DCS-IDDC(1) TO NYCKELLANGD                          
076900                                                                          
077000     CALL WINTSOR USING IDDC-TABELL  STEGLANGD  ANTAL                     
077100                  T-DCS-IDDC(1) NYCKELLANGD                               
077200                                                                          
077300     DISPLAY 'Alla IDDC på WDB6 laddade i DC-TAB'                         
077400     DISPLAY 'Antal = ' DC-MAX                                            
077500     DISPLAY '----TOP-----'                                               
077600     SET DCIX TO +1                                                       
077700     PERFORM UNTIL DCIX > DC-MAX                                          
077800       SET WDCIX TO DCIX                                                  
077900       DISPLAY  '  TAB-RAD ' WDCIX ' IDDC '  T-DCS-IDDC(DCIX)             
078000       SET DCIX UP BY +1                                                  
078100     END-PERFORM                                                          
078200     DISPLAY '----END-----'                                               
078300     .                                                                    
078400                                                                          
078500                                                                          
078600 AB-LAES-HAENDELSER SECTION.                                              
078700     MOVE 'AB-LAES-HANDELSE' TO CURRENT-SECTION                           
078800                                                                          
078900     PERFORM S02-LAES-W22410                                              
079000     SET TAB2204-IX TO 1                                                  
079100     MOVE ZERO      TO IX                                                 
079200     IF NOT-END-OF-W22410                                                 
079300        MOVE OMSP-IDARTNR TO   TAB2204-IDARTNR (TAB2204-IX)               
079400        MOVE OMSP-IDDC    TO   TAB2204-IDDC    (TAB2204-IX)               
079500     END-IF                                                               
079600                                                                          
079700     PERFORM UNTIL NOT(NOT-END-OF-W22410                                  
079800         AND    (TAB2204-IX < TAB2204-MAX                                 
079900           OR    (TAB2204-IX = TAB2204-MAX AND                            
080000                 (OMSP-IDARTNR = TAB2204-IDARTNR (TAB2204-IX)             
080100              AND OMSP-IDDC    = TAB2204-IDDC    (TAB2204-IX)))))         
080200                                                                          
080300        IF OMSP-IDARTNR = TAB2204-IDARTNR (TAB2204-IX)                    
080400       AND OMSP-IDDC    = TAB2204-IDDC    (TAB2204-IX)                    
080500           ADD 1 TO IX                                                    
080600        ELSE                                                              
080700           IF IX < 2                                                      
080800              MOVE 99 TO TAB2204-KDLPORS-2 (TAB2204-IX)                   
080900           END-IF                                                         
081000           IF  IX < 3                                                     
081100               MOVE 99 TO TAB2204-KDLPORS-3 (TAB2204-IX)                  
081200           END-IF                                                         
081300              MOVE 1 TO IX                                                
081400              SET TAB2204-IX UP BY 1                                      
081500        END-IF                                                            
081600                                                                          
081700        MOVE OMSP-IDARTNR TO TAB2204-IDARTNR (TAB2204-IX)                 
081800        MOVE OMSP-IDDC    TO TAB2204-IDDC    (TAB2204-IX)                 
081900        EVALUATE IX                                                       
082000           WHEN 1                                                         
082100              MOVE OMSP-KDLPORS                                           
082200                TO TAB2204-KDLPORS-1 (TAB2204-IX)                         
082300           WHEN 2                                                         
082400              MOVE OMSP-KDLPORS                                           
082500                TO TAB2204-KDLPORS-2 (TAB2204-IX)                         
082600           WHEN 3                                                         
082700              MOVE OMSP-KDLPORS                                           
082800                TO TAB2204-KDLPORS-3 (TAB2204-IX)                         
082900        END-EVALUATE                                                      
083000                                                                          
083100        PERFORM S02-LAES-W22410                                           
083200     END-PERFORM                                                          
083300                                                                          
083400     IF IX < 2                                                            
083500        MOVE 99 TO TAB2204-KDLPORS-2 (TAB2204-IX)                         
083600     END-IF                                                               
083700     IF IX < 3                                                            
083800        MOVE 99 TO TAB2204-KDLPORS-3 (TAB2204-IX)                         
083900     END-IF                                                               
084000                                                                          
084100     IF  TAB2204-IX = TAB2204-MAX                                         
084200     AND NOT-END-OF-W22410                                                
084300         DISPLAY 'HÄNDELSE-TABELL FULL'                                   
084400         MOVE 25 TO RETURN-CODE                                           
084500         PERFORM S02-LAES-W22410                                          
084600     ELSE                                                                 
084700         SET TAB2204-MAX   TO TAB2204-IX                                  
084800     END-IF                                                               
084900                                                                          
085000     PERFORM UNTIL NOT(TAB2204-IX < TAB2204-MAX)                          
085100                                                                          
085200        SET TAB2204-IX UP BY 1                                            
085300        MOVE 999999999  TO TAB2204-IDARTNR (TAB2204-IX)                   
085400        MOVE HIGH-VALUE TO TAB2204-IDDC    (TAB2204-IX)                   
085500        MOVE 99         TO TAB2204-KDLPORS-1 (TAB2204-IX)                 
085600                           TAB2204-KDLPORS-2 (TAB2204-IX)                 
085700                           TAB2204-KDLPORS-3 (TAB2204-IX)                 
085800     END-PERFORM                                                          
085900     .                                                                    
086000                                                                          
086100                                                                          
086200 B-SPARA-INDATA  SECTION.                                                 
086300     MOVE 'B-SPARA-INDATA  ' TO CURRENT-SECTION                           
086400                                                                          
086500     MOVE NDCCN-KVSLAGER          TO WUPD-KVSLAGER                        
086600     MOVE NDCCN-KVEOQ             TO WUPD-KVEOQ                           
086700     MOVE NDCCN-TIMANSEC          TO WUPD-TIMANSEC                        
086800     MOVE NDCCN-KDLPSP            TO WUPD-KDLPSP                          
086900     MOVE NDCCN-KVSLUTKP          TO WUPD-KVSLUTKP                        
087000     MOVE NDCCN-TILPSP            TO WUPD-TILPSP                          
087100     MOVE NDCCN-KDLEVPLF          TO WUPD-KDLEVPLF                        
087200     MOVE NDCCN-DAPBPLAN          TO WUPD-DAPBPLAN                        
087300     MOVE NDCCN-KVPB-PLAN         TO WUPD-KVPB-PLAN                       
087400     MOVE NDCCN-DASEASON          TO WUPD-DASEASON                        
087500     MOVE NDCCN-RESEASON-PLAN(01) TO WUPD-RESEASON-PLAN(01)               
087600     MOVE NDCCN-RESEASON-PLAN(02) TO WUPD-RESEASON-PLAN(02)               
087700     MOVE NDCCN-RESEASON-PLAN(03) TO WUPD-RESEASON-PLAN(03)               
087800     MOVE NDCCN-RESEASON-PLAN(04) TO WUPD-RESEASON-PLAN(04)               
087900     MOVE NDCCN-RESEASON-PLAN(05) TO WUPD-RESEASON-PLAN(05)               
088000     MOVE NDCCN-RESEASON-PLAN(06) TO WUPD-RESEASON-PLAN(06)               
088100     MOVE NDCCN-RESEASON-PLAN(07) TO WUPD-RESEASON-PLAN(07)               
088200     MOVE NDCCN-RESEASON-PLAN(08) TO WUPD-RESEASON-PLAN(08)               
088300     MOVE NDCCN-RESEASON-PLAN(09) TO WUPD-RESEASON-PLAN(09)               
088400     MOVE NDCCN-RESEASON-PLAN(10) TO WUPD-RESEASON-PLAN(10)               
088500     MOVE NDCCN-RESEASON-PLAN(11) TO WUPD-RESEASON-PLAN(11)               
088600     MOVE NDCCN-RESEASON-PLAN(12) TO WUPD-RESEASON-PLAN(12)               
088700     .                                                                    
088800                                                                          
088900                                                                          
089000 C-BERAKNA-TILLGANGAR SECTION.                                            
089100     MOVE 'C-BER-TILLGANGAR' TO CURRENT-SECTION                           
089200                                                                          
089300     MOVE ZERO              TO   W-TILLG-SDC                              
089400*                                W-KVPB-SDC-TOT                           
089500                                 W-OVERLAGER-SDC                          
089600                                                                          
089700     MOVE NDCCN-IDDC     TO W-IDDC-K7-MIN                                 
089800                            W-IDDC-K7-MAX                                 
089900     MOVE 'A'            TO W-IDDC-K7-MIN(2:1)                            
090000     MOVE '9'            TO W-IDDC-K7-MAX(2:1)                            
090100     MOVE NDCCN-IDDC     TO W-IDDC-REF                                    
090200     PERFORM IMS-GNP-WDK711-REF                                           
090300                                                                          
090400     PERFORM UNTIL SEGMENT-SAKNAS                                         
090500                                                                          
090600*       ADD SLAG-KVPB-REF   TO   W-KVPB-SDC-TOT                           
090700                                                                          
090800        MOVE ZERO           TO   W-TILLG-SDC                              
090900        ADD SLAG-KVLS       TO   W-TILLG-SDC                              
091000        ADD SLAG-KVBEART    TO   W-TILLG-SDC                              
091100        ADD SLAG-KVAKS-SDC  TO   W-TILLG-SDC                              
091200        ADD SLAG-KVAKS-PAV  TO   W-TILLG-SDC                              
091300        COMPUTE W-KVOKS = SLAG-KVOKS-BULK                                 
091400                           + SLAG-KVOKS-DAG                               
091500*****-- FIX FÖR NEGATIVA KVOKS                                            
091600        IF W-KVOKS > 0                                                    
091700           SUBTRACT W-KVOKS FROM W-TILLG-SDC                              
091800        END-IF                                                            
091900                                                                          
092000        SUBTRACT SLAG-KVRESS FROM W-TILLG-SDC                             
092100                                                                          
092200        SET DCIX TO +1                                                    
092300        SEARCH DC-TAB                                                     
092400            AT END                                                        
092500               MOVE NEJ TO DCS-TRAEFF                                     
092600            WHEN T-DCS-IDDC (DCIX) = SLAG-IDDC                            
092700               MOVE JA  TO DCS-TRAEFF                                     
092800        END-SEARCH                                                        
092900                                                                          
093000        IF DCS-TRAEFF = JA                                                
093100*          - KOLLA OM ÖVERLAGERBERÄKNING PÅ SDC SKA GÖRAS                 
093200           IF T-DCS-FLOVRLAGBER(DCIX) = NEJ                               
093300              CONTINUE                                                    
093400           ELSE                                                           
093500              IF SLAG-KVREFOVL < W-TILLG-SDC                              
093600                 COMPUTE W-OVERLAGER-SDC = W-OVERLAGER-SDC                
093700                                         + W-TILLG-SDC                    
093800                                         - SLAG-KVREFOVL                  
093900              END-IF                                                      
094000           END-IF                                                         
094100        ELSE                                                              
094200*          -- SDC ÄR EJ REGISTRERAT PÅ WDB6. HOPPA !                      
094300*          -- Detta borde egentligen aldrig inträffa'                     
094400           DISPLAY 'IDDC ' SLAG-IDDC ' EJ REG PÅ WDB6'                    
094500        END-IF                                                            
094600                                                                          
094700        IF NDCCN-DAPUBL > ZERO                                            
094800           MOVE NDCCN-DAPUBL(3:6) TO DAT-I-TIDATUM                        
094900           MOVE 'AAMMDD'          TO DAT-KDDATFORM                        
095000           CALL WDATKONV USING  DAT-KDDATFORM                             
095100                                DAT-I-TIDATUM                             
095200                                DAT-O-TIDATUM                             
095300                                DAT-KDSVAR                                
095400                                                                          
095500           IF DAT-KDSVAR-FEL                                              
095600             MOVE 32          TO RKOD                                     
095700             DISPLAY '*** W2241200, FELAKTIG DATUMKONV 1'                 
095800             CALL ABEND USING RKOD                                        
095900           END-IF                                                         
096000        ELSE                                                              
096100           MOVE NDCCN-TIFINLV     TO DAT-I-TIDATUM                        
096200           MOVE 'AAVVD'           TO DAT-KDDATFORM                        
096300           CALL WDATKONV USING  DAT-KDDATFORM                             
096400                                DAT-I-TIDATUM                             
096500                                DAT-O-TIDATUM                             
096600                                DAT-KDSVAR                                
096700           IF DAT-KDSVAR-FEL                                              
096800             MOVE 32          TO RKOD                                     
096900             DISPLAY '*** W2241200, FELAKTIG DATUMKONV 2'                 
097000             CALL ABEND USING RKOD                                        
097100           END-IF                                                         
097200        END-IF                                                            
097300        MOVE DAT-TISEKEL          TO W-HELP-TIFINLV-SS                    
097400        MOVE DAT-TIAA             TO W-HELP-TIFINLV-AA                    
097500        MOVE DAT-TIVV             TO W-HELP-TIFINLV-VV                    
097600                                                                          
097700        MOVE 20                   TO W-HELP-DATUM-SS                      
097800        MOVE W-DATUM-AA           TO W-HELP-DATUM-AA                      
097900        MOVE W-DATUM-VV           TO W-HELP-DATUM-VV                      
098000        COMPUTE W-VECKO-DIFF =                                            
098100            (W-HELP-DATUM-SSAA - W-HELP-TIFINLV-SSAA) * 52 +              
098200            (W-HELP-DATUM-VV   - W-HELP-TIFINLV-VV)                       
098300        IF W-VECKO-DIFF < 52                                              
098400           MOVE ZERO TO W-OVERLAGER-SDC                                   
098500***        DISPLAY '          ÖVERLAG NOLLAS. VECKODIFF < 52'             
098600        END-IF                                                            
098700        PERFORM IMS-GNP-WDK711-REF                                        
098800     END-PERFORM                                                          
098900                                                                          
099000*    COMPUTE W-KVPB-SDC-TOT ROUNDED = W-KVPB-SDC-TOT                      
099100     .                                                                    
099200                                                                          
099300                                                                          
099400 D-LAES-DATA     SECTION.                                                 
099500     MOVE 'D-LAES-DATA     ' TO CURRENT-SECTION                           
099600                                                                          
099700     PERFORM DA-LAES-BEHOVS-TABELL                                        
099800     PERFORM DB-LAES-LEVERANTOER                                          
099900     PERFORM DC-SOEK-I-ORSAKSTABELL                                       
100000     .                                                                    
100100                                                                          
100200                                                                          
100300 DA-LAES-BEHOVS-TABELL  SECTION.                                          
100400     MOVE 'DA-LAES-BEHOVTAB' TO CURRENT-SECTION                           
100500                                                                          
100600     MOVE NDCCN-IDARTNR        TO BHDC-IDARTNR                            
100700     MOVE NDCCN-IDDC           TO BHDC-IDDC                               
100800     MOVE PB-TOTAL-SEP-LEV-XDC TO BHDC-KDBEHOV                            
100900     MOVE 52                   TO BHDC-KVVECKOR-BEHOV                     
101000     MOVE ZERO                 TO BHDC-KVTILLG-TOT-CDC                    
101100                                                                          
101200     MOVE W-DATUM-AAVV         TO BHDC-TIAAVV-AKTUELL                     
101300     MOVE 1                    TO W-ANTAL-VECKOR                          
101400                                                                          
101500     CALL W009VADD USING BHDC-TIAAVV-AKTUELL W-ANTAL-VECKOR               
101600     MOVE BHDC-TIAAVV-AKTUELL  TO BHDC-TIBEHOV-START                      
101700     MOVE +6                   TO BHDC-TID-AKTUELL                        
101800                                                                          
101900     IF NDCCN-KDERS-UTG = 0                                               
102000       CALL W222BHDC USING BHDC-W222BHDC                                  
102100                           BHDC-WDK6-PCB   BHDC-WDK7-PCB                  
102200                           BHDC-WDB6-PCB   BHDC-WDR2-PCB                  
102300                           BHDC-WDD7-PCB   BHDC-WDK7E-PCB                 
102400                           BHDC-WDD7-2-PCB BHDC-WDK9-PCB                  
102500                           BHDC-REFL1-2501-PCB                            
102600                           BHDC-REFL1-WDB6-PCB                            
102700                           BHDC-REFL1-WDK7-PCB                            
102800                           BHDC-REFL1-UTIL-WDK6-PCB                       
102900                           BHDC-REFL1-UTIL-WDK7-PCB                       
103000                           BHDC-REFL1-UTIL-WDB6-PCB                       
103100                           BHDC-REFL2-2501-PCB                            
103200                           BHDC-REFL2-WDB6-PCB                            
103300                           BHDC-REFL2-UTIL-WDK6-PCB                       
103400                           BHDC-REFL2-UTIL-WDK7-PCB                       
103500                           BHDC-REFL2-UTIL-WDB6-PCB                       
103600                           BHDC-UTIL-WDK6-PCB                             
103700                           BHDC-UTIL-WDK7-PCB                             
103800                           BHDC-UTIL-WDB6-PCB                             
103900                           BHDC-W222-WDK6-PCB                             
104000                           BHDC-W222-WDK7-PCB                             
104100                           BHDC-W222-ARTM-PCB                             
104200                           BHDC-W222-2501-PCB                             
104300                           BHDC-W222-WDB6R-PCB                            
104400                           BHDC-W222-WDK7R-PCB                            
104500                           BHDC-W222-WDB6-PCB                             
104600                           BHDC-W222-WDD7-PCB                             
104700                           BHDC-W222-WDK7E-PCB                            
104800                           BHDC-W222-UTIL-WDK6-PCB                        
104900                           BHDC-W222-UTIL-WDK7-PCB                        
105000                           BHDC-W222-UTIL-WDB6-PCB                        
105100                           BHDC-W222-UTUP-WDK7-PCB                        
105200                           BHDC-W222-UTUP-WDB6-PCB                        
105300                           BHDC-W222-UTUP-UTIL-WDK6-PCB                   
105400                           BHDC-W222-UTUP-UTIL-WDK7-PCB                   
105500                           BHDC-W222-UTUP-UTIL-WDB6-PCB                   
105600                           BHDC-UTUP-WDK7-PCB                             
105700                           BHDC-UTUP-WDB6-PCB                             
105800                           BHDC-UTUP-UTIL-WDK6-PCB                        
105900                           BHDC-UTUP-UTIL-WDK7-PCB                        
106000                           BHDC-UTUP-UTIL-WDB6-PCB                        
106100                                                                          
106200       IF BHDC-FLJANEJ-ANROP = 'N'                                        
106300         PERFORM S10-NOLLA-BHDC-RESULTATFLT                               
106400       END-IF                                                             
106500     ELSE                                                                 
106600       PERFORM S10-NOLLA-BHDC-RESULTATFLT                                 
106700     END-IF                                                               
106800     .                                                                    
106900                                                                          
107000                                                                          
107100 DB-LAES-LEVERANTOER    SECTION.                                          
107200     MOVE 'DB-LAES-LEV     ' TO CURRENT-SECTION                           
107300                                                                          
107400     MOVE ZERO          TO W-KDLPORS-TAB (1)                              
107500                           W-KDLPORS-TAB (2)                              
107600                           W-KDLPORS-TAB (3)                              
107700                                                                          
107800     MOVE ZERO          TO D904-KDLPORS-TAB (1)                           
107900                           D904-KDLPORS-TAB (2)                           
108000                           D904-KDLPORS-TAB (3)                           
108100                                                                          
108200     MOVE NDCCN-IDARTNR TO W-IDARTNR-D9                                   
108300     MOVE NDCCN-IDDC    TO W-IDDC-D9                                      
108400     MOVE NDCCN-IDLEVNR TO W-IDLEVNR                                      
108500                                                                          
108600     PERFORM IMS-GU-WDD901                                                
108700     IF SEGMENT-FINNS                                                     
108800        PERFORM IMS-GU-WDD902-O                                           
108900        IF SEGMENT-SAKNAS                                                 
109000* ---   Rötter utan underliggande segment tas bort'                       
109100           PERFORM S100-NOLLA-W22413-AREA                                 
109200           MOVE DELETE-WDD901    TO UPD-IDPTYP                            
109300           MOVE NDCCN-IDDC       TO UPD-IDDC                              
109400           MOVE NDCCN-IDARTNR    TO UPD-IDARTNR                           
109500                                                                          
109600           PERFORM S12-SKRIV-W22413                                       
109700        ELSE                                                              
109800           PERFORM IMS-GU-WDD902                                          
109900           IF SEGMENT-FINNS                                               
110000              MOVE D902-IDLEVNR TO W-IDLEVNR                              
110100              PERFORM IMS-GU-WDD904                                       
110200           END-IF                                                         
110300        END-IF                                                            
110400     END-IF                                                               
110500     .                                                                    
110600                                                                          
110700                                                                          
110800 DC-SOEK-I-ORSAKSTABELL SECTION.                                          
110900     MOVE 'DC-SOEK-I-ORSTAB' TO CURRENT-SECTION                           
111000                                                                          
111100     SEARCH ALL TAB2204-INGANG                                            
111200     WHEN                                                                 
111300       TAB2204-IDARTNR (TAB2204-IX) = NDCCN-IDARTNR  AND                  
111400       TAB2204-IDDC    (TAB2204-IX) = NDCCN-IDDC                          
111500         MOVE TAB2204-KDLPORS-1 (TAB2204-IX)                              
111600                  TO W-KDLPORS-TAB (1)                                    
111700         MOVE TAB2204-KDLPORS-2 (TAB2204-IX)                              
111800                  TO W-KDLPORS-TAB (2)                                    
111900         MOVE TAB2204-KDLPORS-3 (TAB2204-IX)                              
112000                  TO W-KDLPORS-TAB (3)                                    
112100     END-SEARCH                                                           
112200                                                                          
112300     IF  W-KDLPORS-TAB (1) = 99                                           
112400         MOVE ZERO TO W-KDLPORS-TAB (1)                                   
112500     END-IF                                                               
112600     IF  W-KDLPORS-TAB (2) = 99                                           
112700         MOVE ZERO TO W-KDLPORS-TAB (2)                                   
112800     END-IF                                                               
112900     IF  W-KDLPORS-TAB (3) = 99                                           
113000         MOVE ZERO TO W-KDLPORS-TAB (3)                                   
113100     END-IF                                                               
113200     .                                                                    
113300                                                                          
113400                                                                          
113500 E-DIVERSE-KONTROLLER SECTION.                                            
113600     MOVE 'E-DIVERSE-KONTR ' TO CURRENT-SECTION                           
113700                                                                          
113800     MOVE NDCCN-KVVECKOR-FT       TO W-ANTAL-VECKOR                       
113900     MOVE W-DATUM-AAVV            TO W-DATUM-GRAENS                       
114000     CALL W009VADD USING W-DATUM-GRAENS W-ANTAL-VECKOR                    
114100                                                                          
114200     IF NDCCN-KDAVT > ZERO                                                
114300        IF NDCCN-DAPUBL > ZERO                                            
114400           MOVE NDCCN-DAPUBL(3:6) TO DAT-I-TIDATUM                        
114500           MOVE 'AAMMDD'        TO DAT-KDDATFORM                          
114600           CALL WDATKONV USING DAT-KDDATFORM                              
114700                               DAT-I-TIDATUM                              
114800                               DAT-O-TIDATUM                              
114900                               DAT-KDSVAR                                 
115000           IF DAT-KDSVAR-OK                                               
115100             MOVE DAT-TIAAVVD     TO W-TIFINLV                            
115200           ELSE                                                           
115300             MOVE 32       TO RKOD                                        
115400             DISPLAY '*** W2241200, FELAKTIG DATUMKONV 3'                 
115500             CALL ABEND USING RKOD                                        
115600           END-IF                                                         
115700        ELSE                                                              
115800           MOVE NDCCN-TIFINLV   TO W-TIFINLV                              
115900        END-IF                                                            
116000                                                                          
116100        DIVIDE W-TIFINLV BY 10 GIVING W-TIFINLV-AAVV                      
116200        IF W-TIFINLV-AAVV = W-DATUM-GRAENS                                
116300          MOVE 16               TO W-KDLPORS-TAB(4)                       
116400          CALL W221LPAD USING W-W221LP-CTX W-KDLPORS-GRP                  
116500        END-IF                                                            
116600     END-IF                                                               
116700                                                                          
116800     MOVE -3 TO W-ANTAL-VECKOR                                            
116900     MOVE W-DATUM-AAVV          TO W-DATUM-GRAENS                         
117000     CALL W009VADD USING W-DATUM-GRAENS W-ANTAL-VECKOR                    
117100                                                                          
117200     IF (NDCCN-KDLPSP = 5                                                 
117300         AND NDCCN-TIOMSPEC NOT > W-DATUM-GRAENS)                         
117400     OR (NDCCN-KDLPSP = 5                                                 
117500         AND NDCCN-KDERS > 20)                                            
117600         MOVE 71                TO W-KDLPORS-TAB(4)                       
117700         CALL W221LPAD USING W-W221LP-CTX W-KDLPORS-GRP                   
117800                                                                          
117900         MOVE +0 TO D904-KDLPORS-TAB (1)                                  
118000                    D904-KDLPORS-TAB (2)                                  
118100                    D904-KDLPORS-TAB (3)                                  
118200                                                                          
118300         MOVE ZERO TO WUPD-KDLPSP                                         
118400                                                                          
118500         PERFORM EB-BORTTAG-OMSPEC                                        
118600         PERFORM EC-BORTTAG-AVROP                                         
118700                                                                          
118800     END-IF                                                               
118900                                                                          
119000     IF  (NDCCN-KDLPSP = 3                                                
119100     OR   NDCCN-KDLPSP = 6)                                               
119200     AND NDCCN-TILPSP <= W-DATUM-AAVV                                     
119300         MOVE ZERO TO WUPD-KDLPSP                                         
119400     END-IF                                                               
119500                                                                          
119600     IF NDCCN-DAPBPLAN < W-HELA-DATUMET                                   
119700        MOVE ZERO TO WUPD-DAPBPLAN                                        
119800        MOVE ZERO TO WUPD-KVPB-PLAN                                       
119900*       NOLLINGEN SKALL UPPDATERA WDK722                                  
120000*       VILKET BORDE KOMMA ATT GÖRAS I                                    
120100*       L-KOLLA-SKRIV-WDK7-UPPDATER                                       
120110     ELSE                                                                 
120120        IF NDCCN-KVPB-TREND NOT = ZERO                                    
120121          COMPUTE W-KVPB-TREND-VECKA =                                    
120122                  NDCCN-KVPB-TREND / 4.33                                 
120123          COMPUTE WUPD-KVPB-PLAN ROUNDED = WUPD-KVPB-PLAN +               
120124                                   W-KVPB-TREND-VECKA                     
120125          IF WUPD-KVPB-PLAN < ZERO                                        
120126            MOVE ZERO     TO WUPD-KVPB-PLAN                               
120127          END-IF                                                          
120130        END-IF                                                            
120200     END-IF                                                               
120300                                                                          
120400     IF NDCCN-DASEASON < W-HELA-DATUMET                                   
120500        MOVE ZERO TO WUPD-DASEASON                                        
120600        MOVE ZERO TO WUPD-RESEASON-PLAN (1)                               
120700        MOVE ZERO TO WUPD-RESEASON-PLAN (2)                               
120800        MOVE ZERO TO WUPD-RESEASON-PLAN (3)                               
120900        MOVE ZERO TO WUPD-RESEASON-PLAN (4)                               
121000        MOVE ZERO TO WUPD-RESEASON-PLAN (5)                               
121100        MOVE ZERO TO WUPD-RESEASON-PLAN (6)                               
121200        MOVE ZERO TO WUPD-RESEASON-PLAN (7)                               
121300        MOVE ZERO TO WUPD-RESEASON-PLAN (8)                               
121400        MOVE ZERO TO WUPD-RESEASON-PLAN (9)                               
121500        MOVE ZERO TO WUPD-RESEASON-PLAN (10)                              
121600        MOVE ZERO TO WUPD-RESEASON-PLAN (11)                              
121700        MOVE ZERO TO WUPD-RESEASON-PLAN (12)                              
121800*       NOLLINGEN SKALL UPPDATERA WDK722                                  
121900*       VILKET BORDE KOMMA ATT GÖRAS I                                    
122000*       L-KOLLA-SKRIV-WDK7-UPPDATER                                       
122100     END-IF                                                               
122200     .                                                                    
122300                                                                          
122400                                                                          
122500 EB-BORTTAG-OMSPEC   SECTION.                                             
122600     MOVE 'EB-BORTTAG-OMSP ' TO CURRENT-SECTION                           
122700                                                                          
122800     MOVE NDCCN-IDLEVNR    TO W-IDLEVNR                                   
122900     PERFORM IMS-GU-WDD902                                                
123000     IF SEGMENT-FINNS                                                     
123100                                                                          
123200        PERFORM IMS-GNP-WDD904                                            
123300        PERFORM UNTIL SEGMENT-SAKNAS                                      
123400           PERFORM S100-NOLLA-W22413-AREA                                 
123500           MOVE DELETE-WDD904   TO UPD-IDPTYP                             
123600           MOVE NDCCN-IDDC      TO UPD-IDDC                               
123700           MOVE NDCCN-IDARTNR   TO UPD-IDARTNR                            
123800           MOVE NDCCN-IDLEVNR   TO UPD-IDLEVNR                            
123900           PERFORM S12-SKRIV-W22413                                       
124000           PERFORM IMS-GNP-WDD904                                         
124100        END-PERFORM                                                       
124200     END-IF                                                               
124300                                                                          
124400*    -- TAG ÄVEN BORT ARTIKELN FRÅN FÖRSLAGS-KÖN OM DEN FINNS             
124500     PERFORM S100-NOLLA-W22413-AREA                                       
124600     MOVE DELETE-WDD601    TO UPD-IDPTYP                                  
124700     MOVE NDCCN-IDDC       TO UPD-IDDC                                    
124800     MOVE NDCCN-IDARTNR    TO UPD-IDARTNR                                 
124900     PERFORM S12-SKRIV-W22413                                             
125000     .                                                                    
125100                                                                          
125200                                                                          
125300 EC-BORTTAG-AVROP    SECTION.                                             
125400     MOVE 'EC-BORTTAG-AVROP' TO CURRENT-SECTION                           
125500                                                                          
125600     MOVE NDCCN-IDLEVNR    TO W-IDLEVNR                                   
125700     PERFORM IMS-GU-WDD902                                                
125800     IF SEGMENT-FINNS                                                     
125900        MOVE 1             TO W-KDAVROP                                   
126000        PERFORM IMS-GNP-WDD905                                            
126100                                                                          
126200        PERFORM UNTIL SEGMENT-SAKNAS                                      
126300           PERFORM S100-NOLLA-W22413-AREA                                 
126400           MOVE DELETE-WDD905    TO UPD-IDPTYP                            
126500           MOVE NDCCN-IDDC       TO UPD-IDDC                              
126600           MOVE NDCCN-IDARTNR    TO UPD-IDARTNR                           
126700           MOVE NDCCN-IDLEVNR    TO UPD-IDLEVNR                           
126800           MOVE D905-KDAVROP     TO UPD-KDAVROP                           
126900           MOVE D905-DAAVROP-AVS TO UPD-DAAVROP-AVS                       
127000           MOVE D905-TILEVDAG    TO UPD-TILEVDAG                          
127100           PERFORM S12-SKRIV-W22413                                       
127200                                                                          
127300           PERFORM IMS-GNP-WDD905                                         
127400        END-PERFORM                                                       
127500     END-IF                                                               
127600     .                                                                    
127700                                                                          
127800                                                                          
127900 F-PUNKTBERAKNING SECTION.                                                
128000     MOVE 'F-PUNKTBERAKNING' TO CURRENT-SECTION                           
128100                                                                          
128200     IF NDCCN-DAPUBL > ZERO                                               
128300       MOVE NDCCN-DAPUBL(3:6) TO DAT-I-TIDATUM                            
128400       MOVE 'AAMMDD'          TO DAT-KDDATFORM                            
128500       CALL WDATKONV USING  DAT-KDDATFORM                                 
128600                            DAT-I-TIDATUM                                 
128700                            DAT-O-TIDATUM                                 
128800                            DAT-KDSVAR                                    
128900                                                                          
129000       IF DAT-KDSVAR-OK                                                   
129100         MOVE DAT-TIAAVVD     TO TMP1-YYWWD                               
129200         MOVE W-DATUM-AAVVD   TO TMP2-YYWWD                               
129300       ELSE                                                               
129400         MOVE 32          TO RKOD                                         
129500         DISPLAY '*** W2241200, FELAKTIG DATUMKONV 4'                     
129600         CALL ABEND USING RKOD                                            
129700       END-IF                                                             
129800     ELSE                                                                 
129900       MOVE NDCCN-TIFINLV   TO TMP1-YYWWD                                 
130000       MOVE W-DATUM-AAVVD   TO TMP2-YYWWD                                 
130100     END-IF                                                               
130200                                                                          
130300     PERFORM WY2000P2                                                     
130400     IF TMP1-YYWWD > TMP2-YYWWD                                           
130500        MOVE BHDC-W222BHDC     TO SPAR-BHDC-W222BHDC                      
130600                                                                          
130700        MOVE NDCCN-IDARTNR     TO BHDC-IDARTNR                            
130800        MOVE NDCCN-IDDC        TO BHDC-IDDC                               
130900        MOVE XDC-CDC-BEHOV     TO BHDC-KDBEHOV                            
131000        MOVE +6                TO BHDC-TID-AKTUELL                        
131100        MOVE ZERO              TO BHDC-KVTILLG-TOT-CDC                    
131200                                                                          
131300        IF NDCCN-KDERS-UTG = 0                                            
131400          CALL W222BHDC USING BHDC-W222BHDC                               
131500                              BHDC-WDK6-PCB   BHDC-WDK7-PCB               
131600                              BHDC-WDB6-PCB   BHDC-WDR2-PCB               
131700                              BHDC-WDD7-PCB   BHDC-WDK7E-PCB              
131800                              BHDC-WDD7-2-PCB BHDC-WDK9-PCB               
131900                              BHDC-REFL1-2501-PCB                         
132000                              BHDC-REFL1-WDB6-PCB                         
132100                              BHDC-REFL1-WDK7-PCB                         
132200                              BHDC-REFL1-UTIL-WDK6-PCB                    
132300                              BHDC-REFL1-UTIL-WDK7-PCB                    
132400                              BHDC-REFL1-UTIL-WDB6-PCB                    
132500                              BHDC-REFL2-2501-PCB                         
132600                              BHDC-REFL2-WDB6-PCB                         
132700                              BHDC-REFL2-UTIL-WDK6-PCB                    
132800                              BHDC-REFL2-UTIL-WDK7-PCB                    
132900                              BHDC-REFL2-UTIL-WDB6-PCB                    
133000                              BHDC-UTIL-WDK6-PCB                          
133100                              BHDC-UTIL-WDK7-PCB                          
133200                              BHDC-UTIL-WDB6-PCB                          
133300                              BHDC-W222-WDK6-PCB                          
133400                              BHDC-W222-WDK7-PCB                          
133500                              BHDC-W222-ARTM-PCB                          
133600                              BHDC-W222-2501-PCB                          
133700                              BHDC-W222-WDB6R-PCB                         
133800                              BHDC-W222-WDK7R-PCB                         
133900                              BHDC-W222-WDB6-PCB                          
134000                              BHDC-W222-WDD7-PCB                          
134100                              BHDC-W222-WDK7E-PCB                         
134200                              BHDC-W222-UTIL-WDK6-PCB                     
134300                              BHDC-W222-UTIL-WDK7-PCB                     
134400                              BHDC-W222-UTIL-WDB6-PCB                     
134500                              BHDC-W222-UTUP-WDK7-PCB                     
134600                              BHDC-W222-UTUP-WDB6-PCB                     
134700                              BHDC-W222-UTUP-UTIL-WDK6-PCB                
134800                              BHDC-W222-UTUP-UTIL-WDK7-PCB                
134900                              BHDC-W222-UTUP-UTIL-WDB6-PCB                
135000                              BHDC-UTUP-WDK7-PCB                          
135100                              BHDC-UTUP-WDB6-PCB                          
135200                              BHDC-UTUP-UTIL-WDK6-PCB                     
135300                              BHDC-UTUP-UTIL-WDK7-PCB                     
135400                              BHDC-UTUP-UTIL-WDB6-PCB                     
135500                                                                          
135600          IF BHDC-FLJANEJ-ANROP = 'N'                                     
135700            PERFORM S10-NOLLA-BHDC-RESULTATFLT                            
135800          END-IF                                                          
135900        ELSE                                                              
136000          PERFORM S10-NOLLA-BHDC-RESULTATFLT                              
136100        END-IF                                                            
136200                                                                          
136300        PERFORM FA-BERAKNA-PUNKTER                                        
136400                                                                          
136500        MOVE SPAR-BHDC-W222BHDC TO BHDC-W222BHDC                          
136600     ELSE                                                                 
136700        PERFORM FA-BERAKNA-PUNKTER                                        
136800     END-IF                                                               
136900     .                                                                    
137000     EJECT                                                                
137100                                                                          
137200 FA-BERAKNA-PUNKTER SECTION.                                              
137300     MOVE 'EA-BER-PUNKTER  ' TO CURRENT-SECTION                           
137400                                                                          
137500     PERFORM FAA-INITIERA-PUNK-PARM                                       
137600                                                                          
137700     CALL W224PUNK USING PUNK-W224PUNK                                    
137800                         PUNK-REFL1-2501-PCB                              
137900                         PUNK-REFL1-WDB6-PCB                              
138000                         PUNK-REFL1-WDK7-PCB                              
138100                         PUNK-REFL1-UTIL-WDK6-PCB                         
138200                         PUNK-REFL1-UTIL-WDK7-PCB                         
138300                         PUNK-REFL1-UTIL-WDB6-PCB                         
138400                         PUNK-UTUP1-WDK7-PCB                              
138500                         PUNK-UTUP1-WDB6-PCB                              
138600                         PUNK-UTUP1-UTIL-WDK6-PCB                         
138700                         PUNK-UTUP1-UTIL-WDK7-PCB                         
138800                         PUNK-UTUP1-UTIL-WDB6-PCB                         
138900                                                                          
139000     MOVE PUNK-KVREFBER    TO WUPD-KVREFBER                               
139100     MOVE PUNK-KVREFPKT    TO WUPD-KVREFPKT                               
139200     MOVE PUNK-KVREFOVL    TO WUPD-KVREFOVL                               
139300     MOVE PUNK-KVSLAGER    TO WUPD-KVSLAGER                               
139400     MOVE PUNK-KVEOQ       TO WUPD-KVEOQ                                  
139500     MOVE PUNK-TIREFPAF    TO WUPD-TIREFPAF                               
139600     MOVE PUNK-TIMANSEC    TO WUPD-TIMANSEC                               
139700     MOVE PUNK-TIREFPKT    TO WUPD-TIREFPKT                               
139800                                                                          
139900     PERFORM FAB-BERAKNA-KVSLUTKP                                         
140000     .                                                                    
140100                                                                          
140200                                                                          
140300 FAA-INITIERA-PUNK-PARM SECTION.                                          
140400     MOVE 'FAA-INIT-PUNK   ' TO CURRENT-SECTION                           
140500                                                                          
140600     MOVE NDCCN-IDARTNR        TO PUNK-IDARTNR                            
140700     MOVE NDCCN-IDDC           TO PUNK-IDDC                               
140800     MOVE NDCCN-IDDC-REF       TO PUNK-IDDC-REF                           
140900     MOVE W-DATUM-AAVV         TO PUNK-TIAAVV-AKT                         
141000     MOVE W-DATUM-AAVVD        TO PUNK-TIAAVVD-AKT                        
141100     MOVE NDCCN-IDREFTAB       TO PUNK-IDREFTAB                           
141200     MOVE NDCCN-FLREFBEO       TO PUNK-FLREFBEO                           
141300     MOVE NDCCN-FLWILSON       TO PUNK-FLWILSON                           
141400     MOVE NDCCN-KVREFPKT       TO PUNK-KVREFPKT-IN                        
141500     MOVE NDCCN-TIREFPKT       TO PUNK-TIREFPKT-IN                        
141600     MOVE NDCCN-KVREFBER       TO PUNK-KVREFBER-IN                        
141700     MOVE NDCCN-TIREFPAF       TO PUNK-TIREFPAF-IN                        
141800     MOVE NDCCN-KVSLAGER       TO PUNK-KVSLAGER-IN                        
141900     MOVE NDCCN-TIMANSEC       TO PUNK-TIMANSEC-IN                        
142000     MOVE 1 TO IX-PUNK                                                    
142100     PERFORM UNTIL IX-PUNK > IX-PUNK-MAX                                  
142200        MOVE NDCCN-RESEASON(IX-PUNK)                                      
142300                               TO PUNK-RESEASON(IX-PUNK)                  
142400        ADD 1 TO IX-PUNK                                                  
142500     END-PERFORM                                                          
142600     MOVE NDCCN-FLFLYG         TO PUNK-FLFLYG                             
142700     MOVE NDCCN-IDLEVNR        TO PUNK-IDLEVNR                            
142800     MOVE NDCCN-KVPALL         TO PUNK-KVPALL                             
142900     MOVE NDCCN-KVULOAD        TO PUNK-KVULOAD                            
143000     MOVE NDCCN-KVPB-REF       TO PUNK-KVPB-REF                           
143100     MOVE NDCCN-KVPBREOI       TO PUNK-KVPBREOI                           
143200     MOVE NDCCN-PRMATRL        TO PUNK-PRMATRL                            
143300     MOVE ZERO                 TO PUNK-PRARTBES                           
143400     MOVE NDCCN-ADLAGOMR       TO PUNK-ADLAGOMR                           
143500                                                                          
143600*    UTDATA                                                               
143700     MOVE ZERO                 TO PUNK-KVREFBER                           
143800                                  PUNK-KVREFPKT                           
143900                                  PUNK-KVREFOVL                           
144000                                  PUNK-KVSLAGER                           
144100                                  PUNK-KVEOQ                              
144200                                  PUNK-TIREFPAF                           
144300                                  PUNK-TIMANSEC                           
144400                                  PUNK-TIREFPKT                           
144500     .                                                                    
144600                                                                          
144700                                                                          
144800 FAB-BERAKNA-KVSLUTKP SECTION.                                            
144900     MOVE 'FAB-BER-KVSLUTKP' TO CURRENT-SECTION                           
145000                                                                          
145100     IF NDCCN-KVSLUTKP > ZERO                                             
145200        MOVE W-DATUM-AAVVD  TO DAT-I-TIDATUM                              
145300        ADD +5              TO DAT-I-TIDATUM                              
145400        MOVE 'AAVVD'        TO DAT-KDDATFORM                              
145500        CALL WDATKONV USING DAT-KDDATFORM                                 
145600                            DAT-I-TIDATUM                                 
145700                            DAT-O-TIDATUM                                 
145800                            DAT-KDSVAR                                    
145900        IF DAT-KDSVAR-OK                                                  
146000           IF NDCCN-TISLUTKP > DAT-TIAAMMDD                               
146100              MOVE ZERO     TO WUPD-KVSLUTKP                              
146200           ELSE                                                           
146300              MOVE NDCCN-KVSLUTKP                                         
146400                            TO WUPD-KVSLUTKP                              
146500           END-IF                                                         
146600        ELSE                                                              
146700           MOVE 32          TO RKOD                                       
146800           DISPLAY '*** W2241200, FELAKTIG DATUMKONV 5'                   
146900           CALL ABEND USING RKOD                                          
147000        END-IF                                                            
147100     ELSE                                                                 
147200        MOVE NDCCN-KVSLUTKP TO WUPD-KVSLUTKP                              
147300     END-IF                                                               
147400                                                                          
147500     COMPUTE W-TILLGANG-NDC = NDCCN-KVLS                                  
147600                            + NDCCN-KVAKS-SDC                             
147700                            + NDCCN-KVAKS-PAV                             
147800                            - NDCCN-KVRESS                                
147900                            - NDCCN-KVROS-BULK                            
148000                            - NDCCN-KVROS-DAG                             
148100                            - NDCCN-KVOKS-BULK                            
148200                            - NDCCN-KVOKS-DAG                             
148300     IF WUPD-KVSLUTKP > ZERO                                              
148400        COMPUTE W-KVSLUTKP-DC = W-TILLGANG-NDC                            
148500                              - PUNK-KVSLAGER                             
148600                              + PUNK-KVREFBER                             
148700        IF W-KVSLUTKP-DC < WUPD-KVSLUTKP                                  
148800           MOVE W-KVSLUTKP-DC TO WUPD-KVSLUTKP                            
148900        END-IF                                                            
149000        IF W-KVSLUTKP-DC < ZERO                                           
149100           MOVE ZERO          TO WUPD-KVSLUTKP                            
149200        END-IF                                                            
149300     END-IF                                                               
149400     .                                                                    
149500                                                                          
149600                                                                          
149700 H-EXTRALEVERANS-KONTROLL SECTION.                                        
149800     MOVE 'H-EXTR-LEV-KOLL ' TO CURRENT-SECTION                           
149900                                                                          
150000     MOVE -6 TO W-ANTAL-VECKOR                                            
150100     IF NDCCN-DAPUBL > ZERO                                               
150200        MOVE NDCCN-DAPUBL(3:6)  TO DAT-I-TIDATUM                          
150300        MOVE 'AAMMDD'           TO DAT-KDDATFORM                          
150400        CALL WDATKONV USING DAT-KDDATFORM                                 
150500                            DAT-I-TIDATUM                                 
150600                            DAT-O-TIDATUM                                 
150700                            DAT-KDSVAR                                    
150800        IF DAT-KDSVAR-OK                                                  
150900          MOVE DAT-TIAAVVD        TO W-TIFINLV                            
151000        ELSE                                                              
151100          MOVE 32          TO RKOD                                        
151200          DISPLAY '*** W2241200, FELAKTIG DATUMKONV 7'                    
151300          CALL ABEND USING RKOD                                           
151400        END-IF                                                            
151500     ELSE                                                                 
151600        MOVE NDCCN-TIFINLV      TO W-TIFINLV                              
151700     END-IF                                                               
151800     DIVIDE W-TIFINLV BY 10 GIVING W-DATUM-GRAENS                         
151900     CALL W009VADD USING W-DATUM-GRAENS W-ANTAL-VECKOR                    
152000                                                                          
152100     MOVE 'YYWW'               TO DAYS-KDDATFMT1                          
152200     MOVE 'YYYYWW'             TO DAYS-KDDATFMT2                          
152300     MOVE SPACE                TO DAYS-TIDATE2                            
152400                                  DAYS-IDCALEND                           
152500     MOVE ZERO                 TO DAYS-KVDAYS                             
152600                                                                          
152700     MOVE BHDC-TIAAVV-AKTUELL  TO W-TIAAVV-AKTUELL                        
152800     MOVE W-TIAAVV-AKTUELL     TO DAYS-TIDATE1                            
152900     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
153000     IF DAYS-KDRC = ZERO                                                  
153100        MOVE DAYS-TIDATE2(1:6) TO W-TIAAAAVV-AKTUELL                      
153200     ELSE                                                                 
153300        MOVE 50 TO RKOD                                                   
153400        MOVE 'WZ20DAYS BHDC-TIAAVV-AKTUELL FEL' TO FELTEXT-STR            
153500        DISPLAY FELTEXT                                                   
153600        CALL ABEND USING RKOD                                             
153700     END-IF                                                               
153800                                                                          
153900     MOVE SPACE                TO DAYS-TIDATE2                            
154000     MOVE W-DATUM-GRAENS       TO W-DATUM-GRAENS-AAVV                     
154100     MOVE W-DATUM-GRAENS-AAVV  TO DAYS-TIDATE1                            
154200     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
154300     IF DAYS-KDRC = ZERO                                                  
154400        MOVE DAYS-TIDATE2(1:6) TO W-DATUM-GRAENS-AAAAVV                   
154500     ELSE                                                                 
154600        MOVE 52 TO RKOD                                                   
154700        MOVE 'WZ20DAYS W-DATUM-GRAENS FEL' TO FELTEXT-STR                 
154800        DISPLAY FELTEXT                                                   
154900        CALL ABEND USING RKOD                                             
155000     END-IF                                                               
155100                                                                          
155200     IF  NDCCN-KDLPSP = 3                                                 
155300     OR  NDCCN-IDLEVNR = SPACE                                            
155400     OR  NDCCN-IDLEVNR = '9998'                                           
155500     OR  NDCCN-KDERS > ZERO                                               
155600     OR  NDCCN-KDOPPLAN = JA                                              
155700     OR  W-TIAAAAVV-AKTUELL < W-DATUM-GRAENS-AAAAVV                       
155800        CONTINUE                                                          
155900     ELSE                                                                 
156000                                                                          
156100        PERFORM HB-INITIERAR-VARIABLER                                    
156200        PERFORM HC-BERAKNA-AKTUELL-TILLGANG                               
156300        PERFORM HD-SKAPA-TILLGANGSTABELL                                  
156400        PERFORM HE-KONTROLL-EXTRALEVERANS                                 
156500                                                                          
156600     END-IF                                                               
156700     .                                                                    
156800                                                                          
156900                                                                          
157000 HB-INITIERAR-VARIABLER SECTION.                                          
157100     MOVE 'HB-INIT-VAR     ' TO CURRENT-SECTION                           
157200                                                                          
157300     MOVE BHDC-TIAAVV-AKTUELL TO W-FRYSTIDP                               
157400     MOVE NDCCN-KVVECKOR-FT   TO W-ANTAL-VECKOR                           
157500     ADD 1 TO W-ANTAL-VECKOR                                              
157600     CALL W009VADD USING W-FRYSTIDP W-ANTAL-VECKOR                        
157700     SKIP1                                                                
157800     MOVE ZERO TO W-ANT-BRIST-VECKOR                                      
157900                  W-TIAAVV-FT                                             
158000                                                                          
158100     MOVE 1 TO IX                                                         
158200     PERFORM UNTIL NOT(IX NOT > TILLGTAB-MAX)                             
158300         MOVE ZERO TO TILLGTAB-ANTAL (IX)                                 
158400         ADD 1 TO IX                                                      
158500     END-PERFORM                                                          
158600     .                                                                    
158700                                                                          
158800 HC-BERAKNA-AKTUELL-TILLGANG SECTION.                                     
158900     MOVE 'HC-BER-AKT-TILLG' TO CURRENT-SECTION                           
159000                                                                          
159100     COMPUTE W-TILLG = NDCCN-KVLS                                         
159200                     + NDCCN-KVAKS-SDC                                    
159300                     + NDCCN-KVAKS-PAV                                    
159400                     - NDCCN-KVRESS                                       
159500                     - NDCCN-KVROS-BULK                                   
159600                     - NDCCN-KVROS-DAG                                    
159700                     - NDCCN-KVOKS-BULK                                   
159800                     - NDCCN-KVOKS-DAG                                    
159900                     + W-OVERLAGER-SDC                                    
160000     SUBTRACT BHDC-KVBEHOV-DESSUTOM                                       
160100                  FROM W-TILLG                                            
160200     .                                                                    
160300                                                                          
160400                                                                          
160500 HD-SKAPA-TILLGANGSTABELL SECTION.                                        
160600     MOVE 'HD-SKAPA-TILLGTA' TO CURRENT-SECTION                           
160700******************************************************************        
160800*                                                                *        
160900*    TABELL MED INLEVERANSER PLACERADE I RESPEKTIVE VECKA        *        
161000*                                                                *        
161100******************************************************************        
161200                                                                          
161300     MOVE NEJ   TO SW-INLEV-UNDER-PERIODEN                                
161400                                                                          
161500     MOVE NDCCN-IDARTNR      TO W-IDARTNR                                 
161600     MOVE NDCCN-IDDC         TO W-IDDC                                    
161700     PERFORM IMS-GU-WDK711                                                
161800                                                                          
161900     PERFORM IMS-GU-WDD901                                                
162000     IF SEGMENT-FINNS                                                     
162100        MOVE 2  TO W-KDAVROP                                              
162200        PERFORM IMS-GNP-WDD902-05-FIRST                                   
162300        PERFORM UNTIL SEGMENT-SAKNAS                                      
162400                                                                          
162500           MOVE D905-TIAVRDAT-DISP TO DAT-I-TIDATUM                       
162600           MOVE 'AAMMDD'           TO DAT-KDDATFORM                       
162700           CALL WDATKONV USING DAT-KDDATFORM                              
162800                               DAT-I-TIDATUM                              
162900                               DAT-O-TIDATUM                              
163000                               DAT-KDSVAR                                 
163100           IF DAT-KDSVAR-FEL                                              
163200              MOVE 32 TO RKOD                                             
163300              DISPLAY 'FEL VID ANROP TILL WDATKONV 9'                     
163400              CALL ABEND USING RKOD                                       
163500           END-IF                                                         
163600                                                                          
163700           MOVE DAT-TIAAVV-GRP TO W-TIAAVV                                
163800           IF W-TIAAVV < W-FRYSTIDP                                       
163900                                                                          
164000              IF D905-DAAVROP-AVS(3:4) NOT > W-DATUM-AAVV                 
164100                 ADD D905-KVAVROP  TO W-TILLG                             
164200              ELSE                                                        
164300                 PERFORM HDA-BERAKNA-VECKODIFFERENS                       
164400                 MOVE W-VECKO-DIFF TO TILLGTAB-IX                         
164500                 IF TILLGTAB-IX > ZERO AND NOT > TILLGTAB-MAX             
164600                    ADD D905-KVAVROP                                      
164700                                   TO TILLGTAB-ANTAL (TILLGTAB-IX)        
164800                 END-IF                                                   
164900                 IF D902-IDLEVNR = SLAG-IDLEVNR                           
165000                    MOVE JA        TO SW-INLEV-UNDER-PERIODEN             
165100                 END-IF                                                   
165200              END-IF                                                      
165300           END-IF                                                         
165400           PERFORM IMS-GNP-WDD902-05-NEXT                                 
165500        END-PERFORM                                                       
165600     END-IF                                                               
165700     .                                                                    
165800                                                                          
165900                                                                          
166000 HDA-BERAKNA-VECKODIFFERENS SECTION.                                      
166100     MOVE 'HDA-BER-VECKODIF' TO CURRENT-SECTION                           
166200                                                                          
166300     MOVE W-TIAAVV             TO W-DATUM-TOM                             
166400     MOVE W-DATUM-AAVV         TO W-DATUM-FOM                             
166500                                                                          
166600     COMPUTE W-VECKO-DIFF = 52 * (W-DATUM-FOM-AA - W-DATUM-TOM-AA)        
166700     ADD W-DATUM-TOM-VV        TO W-VECKO-DIFF                            
166800     SUBTRACT W-DATUM-FOM-VV FROM W-VECKO-DIFF                            
166900                                                                          
167000*    JUSTERA FÖR SKOTTÅR (KAN JU VARA ATT PROGRAMMET LEVER LÄNGE!)        
167100     IF (W-DATUM-FOM-AA = 15 AND W-DATUM-TOM-AA > 15)                     
167200     OR (W-DATUM-FOM-AA = 20 AND W-DATUM-TOM-AA > 20)                     
167300     OR (W-DATUM-FOM-AA = 26 AND W-DATUM-TOM-AA > 26)                     
167400     OR (W-DATUM-FOM-AA = 32 AND W-DATUM-TOM-AA > 32)                     
167500     OR (W-DATUM-FOM-AA = 37 AND W-DATUM-TOM-AA > 37)                     
167600     OR (W-DATUM-FOM-AA = 43 AND W-DATUM-TOM-AA > 43)                     
167700     OR (W-DATUM-FOM-AA = 48 AND W-DATUM-TOM-AA > 48)                     
167800     OR (W-DATUM-FOM-AA = 54 AND W-DATUM-TOM-AA > 54)                     
167900     OR (W-DATUM-FOM-AA = 60 AND W-DATUM-TOM-AA > 60)                     
168000     OR (W-DATUM-FOM-AA = 65 AND W-DATUM-TOM-AA > 65)                     
168100     OR (W-DATUM-FOM-AA = 71 AND W-DATUM-TOM-AA > 71)                     
168200     OR (W-DATUM-FOM-AA = 76 AND W-DATUM-TOM-AA > 76)                     
168300     OR (W-DATUM-FOM-AA = 82 AND W-DATUM-TOM-AA > 82)                     
168400     OR (W-DATUM-FOM-AA = 88 AND W-DATUM-TOM-AA > 88)                     
168500     OR (W-DATUM-FOM-AA = 93 AND W-DATUM-TOM-AA > 93)                     
168600        ADD +1 TO W-VECKO-DIFF                                            
168700     END-IF                                                               
168800     .                                                                    
168900                                                                          
169000                                                                          
169100 HE-KONTROLL-EXTRALEVERANS SECTION.                                       
169200     MOVE 'HE-KOLL-EXTRALEV' TO CURRENT-SECTION                           
169300******************************************************************        
169400*                                                                *        
169500*    ÖVRE OCH UNDRE LAGERGRÄNSER BERÄKNAS                        *        
169600*    BERÄKNAD TILLGÅNG KONTROLLERAS MOT GRÄNSER VARJE VECKA      *        
169700*    TILLGÅNG UTANFÖR GRÄNSER TRE VECKOR I FÖLJD                 *        
169800*    MEDFÖR EXTRA LEVERANS ELLER MINSKNING AV LEVERANSER         *        
169900*    EXTRALEVERANS SKAPAS OCKSÅ DÅ DET FINNS TPO-BEHOV INOM FT   *        
170000*    OCH MAN HAR BRIST I NÅGON VECKA.                            *        
170100*                                                                *        
170200*--- 20150911 E'TR 10209749  TA BORT EXTRALEVERANSER.            *        
170300*    BARA FUNKTIONEN FÖR MINSKNING SKALL GÖRAS.                  *        
170400******************************************************************        
170500                                                                          
170600     COMPUTE W-OVRE-GRANS  = NDCCN-KVREFOVL                               
170700                                                                          
170800     MOVE ZERO         TO W-ANT-OVERSK-VECKOR                             
170900     MOVE NEJ          TO SW-OMSPEC-MINSKNING                             
171000                                                                          
171100     MOVE 1 TO IX                                                         
171200     PERFORM UNTIL IX > NDCCN-KVVECKOR-FT                                 
171300                                                                          
171400         ADD TILLGTAB-ANTAL (IX) TO W-TILLG                               
171500         SUBTRACT BHDC-KVBEHOV-VECKA (IX)                                 
171600                                 FROM W-TILLG                             
171700         PERFORM HEA-TESTA-GRANSER                                        
171800         ADD 1 TO IX                                                      
171900     END-PERFORM                                                          
172000                                                                          
172100     IF SW-OMSPEC-MINSKNING = JA                                          
172200        MOVE 08                TO W-KDLPORS-TAB(4)                        
172300        CALL W221LPAD USING W-W221LP-CTX W-KDLPORS-GRP                    
172400     END-IF                                                               
172500     .                                                                    
172600                                                                          
172700                                                                          
172800 HEA-TESTA-GRANSER SECTION.                                               
172900     MOVE 'HEA-TESTA-GRANS ' TO CURRENT-SECTION                           
173000******************************************************************        
173100*                                                                *        
173200*    TEST AV BERÄKNAD VECKOTILLGÅNG MOT ACCEPTABEL               *        
173300*    ÖVRE LAGERNIVÅ.KONTROLL AV FUNKTIONERNA FÖR MINSKNING.      *        
173400*                                                                *        
173500******************************************************************        
173600                                                                          
173700                                                                          
173800     IF W-TILLG > W-OVRE-GRANS                                            
173900        AND SW-INLEV-UNDER-PERIODEN = JA                                  
174000        ADD 1 TO W-ANT-OVERSK-VECKOR                                      
174100     ELSE                                                                 
174200        MOVE ZERO TO W-ANT-OVERSK-VECKOR                                  
174300     END-IF                                                               
174400     IF NDCCN-KDLPSP = +5                                                 
174500        CONTINUE                                                          
174600     ELSE                                                                 
174700        IF W-ANT-OVERSK-VECKOR > 2                                        
174800           COMPUTE W-UDDATEST = W-TIAAVV-AKTUELL / 2                      
174900           IF RESTEN NOT = ZERO                                           
175000              MOVE JA TO SW-OMSPEC-MINSKNING                              
175100           END-IF                                                         
175200        END-IF                                                            
175300     END-IF                                                               
175400     .                                                                    
175500                                                                          
175600                                                                          
175700 I-KONTROLL-KORR-LEVPLAN SECTION.                                         
175800     MOVE 'I-KOLL-KORR-LEVP' TO CURRENT-SECTION                           
175900                                                                          
176000******************************************************************        
176100*                                                                *        
176200*    KONTROLL MOT KORRIDORGRÄNS                                  *        
176300*                                                                *        
176400******************************************************************        
176500                                                                          
176600     IF (NDCCN-KDLPSP NOT = 3 AND NOT = 5)                                
176700        IF W-KDLPORS-TAB (1) > 50                                         
176800           IF NDCCN-IDLEVNR = SPACE OR '9998'                             
176900               CONTINUE                                                   
177000           ELSE                                                           
177100               PERFORM IA-BERAKNA-TILPSP                                  
177200           END-IF                                                         
177300        ELSE                                                              
177400           IF W-KDLPORS-TAB (1) = 50                                      
177500           OR W-KDLPORS-TAB (2) = 50                                      
177600           OR W-KDLPORS-TAB (3) = 50                                      
177700              PERFORM IA-BERAKNA-TILPSP                                   
177800           ELSE                                                           
177900              IF  NDCCN-IDLEVNR = SPACE OR '9998'                         
178000              OR  (W-KDLPORS-TAB (1) > ZERO                               
178100               AND W-KDLPORS-TAB (1) < 50)                                
178200                 CONTINUE                                                 
178300              ELSE                                                        
178400                 IF  NDCCN-KDERS  = ZERO                                  
178500                 AND NDCCN-KDLPSP = ZERO                                  
178600                 AND NDCCN-TILPSP <= W-DATUM-AAVV                         
178700                     PERFORM IA-BERAKNA-TILPSP                            
178800                 END-IF                                                   
178900              END-IF                                                      
179000           END-IF                                                         
179100        END-IF                                                            
179200     END-IF                                                               
179300     .                                                                    
179400                                                                          
179500                                                                          
179600 IA-BERAKNA-TILPSP SECTION.                                               
179700     MOVE 'IA-BER-TILPSP   ' TO CURRENT-SECTION                           
179800                                                                          
179900     MOVE 22          TO W-KDLPORS-TAB(4)                                 
180000     CALL W221LPAD USING W-W221LP-CTX W-KDLPORS-GRP                       
180100                                                                          
180200     MOVE ZERO TO W-ANTAL-VECKOR                                          
180300     COMPUTE W-ARS-OMS = 12 * (NDCCN-KVPB-REF + NDCCN-KVPBREOI)           
180400                            *     NDCCN-PRMATRL                           
180500                                                                          
180600     IF W-ARS-OMS > 100000                                                
180700        MOVE 1 TO W-ANTAL-VECKOR                                          
180800     ELSE                                                                 
180900        MOVE 3 TO W-ANTAL-VECKOR                                          
181000     END-IF                                                               
181100                                                                          
181200     IF W-ANTAL-VECKOR > ZERO                                             
181300         MOVE W-DATUM-AAVV TO WUPD-TILPSP                                 
181400         CALL W009VADD USING WUPD-TILPSP W-ANTAL-VECKOR                   
181500     END-IF                                                               
181600     .                                                                    
181700                                                                          
181800                                                                          
181900 J-SKAPA-OMSPEC-BEGARAN SECTION.                                          
182000     MOVE 'J-SKAPA-OMSP-BEG' TO CURRENT-SECTION                           
182100                                                                          
182200**** OMSPEC VARJE VECKA MED ORSAK BEGÄRD FÖR JIT-ARTIKLAR                 
182300     IF NDCCN-FLJIT = JA   AND  NDCCN-KDERS < 11 AND                      
182400       (W-KDLPORS-TAB (1) = 0 OR > 49)                                    
182500        MOVE 23                 TO W-KDLPORS-TAB(4)                       
182600        CALL W221LPAD USING W-W221LP-CTX W-KDLPORS-GRP                    
182700     END-IF                                                               
182800                                                                          
182900     IF  W-KDLPORS-TAB (1) > ZERO                                         
183000     AND W-KDLPORS-TAB (1) < 50                                           
183100         MOVE NDCCN-IDARTNR     TO LI12-IDARTNR                           
183200         MOVE NDCCN-IDDC        TO LI12-IDDC                              
183300         MOVE NDCCN-KDERS       TO LI12-KDERS                             
183400         MOVE NDCCN-TIFINLV     TO LI12-TIFINLV                           
183500         MOVE NDCCN-KDERS-UTG   TO LI12-KDERS-UTG                         
183600         MOVE W-KDLPORS-TAB (1) TO LI12-KDLPORS-TAB (1)                   
183700         MOVE W-KDLPORS-TAB (2) TO LI12-KDLPORS-TAB (2)                   
183800         MOVE W-KDLPORS-TAB (3) TO LI12-KDLPORS-TAB (3)                   
183900                                                                          
184000         PERFORM S11-SKRIV-W22412                                         
184100     END-IF                                                               
184200                                                                          
184300     .                                                                    
184400                                                                          
184500                                                                          
184600 K-KONTROLL-EJ-AUT-GODK-FORSLAG SECTION.                                  
184700     MOVE 'K-KONTR-EJ-GODK ' TO CURRENT-SECTION                           
184800                                                                          
184900*    FÖR VISSA LEVERANTÖRER OCH VISSA KONTON VILL MAN EJ                  
185000*    ATT LEVPLAN-FÖRSLAGEN SKALL KUNNA GODKÄNNAS AUTOMATISKT,             
185100*    DÅ SÄTTS KDLEVPLF = G  (I WDK722)                                    
185200                                                                          
185300     IF NDCCN-KDLEVPLF = 'P'                                              
185400        IF NDCCN-DAPUBL > ZERO                                            
185500           MOVE NDCCN-DAPUBL(3:6)                                         
185600                                TO DAT-I-TIDATUM                          
185700           MOVE 'AAMMDD'        TO DAT-KDDATFORM                          
185800           CALL WDATKONV USING DAT-KDDATFORM                              
185900                               DAT-I-TIDATUM                              
186000                               DAT-O-TIDATUM                              
186100                               DAT-KDSVAR                                 
186200                                                                          
186300           IF DAT-KDSVAR-FEL                                              
186400              MOVE 32 TO RKOD                                             
186500              DISPLAY 'FEL VID ANROP TILL WDATKONV 10'                    
186600              CALL ABEND USING RKOD                                       
186700           END-IF                                                         
186800        ELSE                                                              
186900           MOVE NDCCN-TIFINLV   TO DAT-I-TIDATUM                          
187000           MOVE 'AAVVD'         TO DAT-KDDATFORM                          
187100           CALL WDATKONV USING DAT-KDDATFORM                              
187200                               DAT-I-TIDATUM                              
187300                               DAT-O-TIDATUM                              
187400                               DAT-KDSVAR                                 
187500           IF DAT-KDSVAR-FEL                                              
187600              MOVE 32 TO RKOD                                             
187700              DISPLAY 'FEL VID ANROP TILL WDATKONV 11'                    
187800              CALL ABEND USING RKOD                                       
187900           END-IF                                                         
188000        END-IF                                                            
188100        MOVE DAT-TISEKEL        TO W-HELP-TIFINLV-SS                      
188200        MOVE DAT-TIAA           TO W-HELP-TIFINLV-AA                      
188300        MOVE DAT-TIVV           TO W-HELP-TIFINLV-VV                      
188400                                                                          
188500        MOVE 20                 TO W-HELP-DATUM-SS                        
188600        MOVE W-DATUM-AA         TO W-HELP-DATUM-AA                        
188700        MOVE W-DATUM-VV         TO W-HELP-DATUM-VV                        
188800        COMPUTE W-VECKO-DIFF =                                            
188900               (W-HELP-DATUM-SSAA - W-HELP-TIFINLV-SSAA) * 52 +           
189000               (W-HELP-DATUM-VV   - W-HELP-TIFINLV-VV)                    
189100        IF W-VECKO-DIFF > 26                                              
189200          MOVE 'J' TO WUPD-KDLEVPLF                                       
189300        END-IF                                                            
189400     END-IF                                                               
189500                                                                          
189600     IF WUPD-KDLEVPLF = 'G' OR SPACE OR LOW-VALUE                         
189700        MOVE 'J' TO WUPD-KDLEVPLF                                         
189800     END-IF                                                               
189900                                                                          
190000     IF WUPD-KDLEVPLF = 'P' OR 'S'                                        
190100         CONTINUE                                                         
190200     ELSE                                                                 
190300        MOVE NDCCN-KDPRODSL      TO TEST-KDPRODSL                         
190400        IF KDPRODSL-VCBV-EMB                                              
190500           MOVE 'G' TO WUPD-KDLEVPLF                                      
190600        END-IF                                                            
190700                                                                          
190800        IF NDCCN-IDANSK NOT < 721 AND                                     
190900           NDCCN-IDANSK NOT > 724 AND                                     
191000           KDPRODSL-TOOLS                                                 
191100           MOVE 'G' TO WUPD-KDLEVPLF                                      
191200        END-IF                                                            
191300     END-IF                                                               
191400                                                                          
191500     IF WUPD-KDLEVPLF = 'G'                                               
191600        MOVE 'W221LN'   TO POSTSUM-FDNAMN                                 
191700        MOVE 'ANTAL   ' TO POSTSUM-DDNAMN2                                
191800        MOVE 'G'        TO POSTSUM-TRANSTYP                               
191900        CALL POSTSUM USING POSTSUM-PARM                                   
192000     END-IF                                                               
192100                                                                          
192200     IF WUPD-KDLEVPLF = 'P'                                               
192300        MOVE 'W221LN'   TO POSTSUM-FDNAMN                                 
192400        MOVE 'ANTAL   ' TO POSTSUM-DDNAMN2                                
192500        MOVE 'P'        TO POSTSUM-TRANSTYP                               
192600        CALL POSTSUM USING POSTSUM-PARM                                   
192700     END-IF                                                               
192800     .                                                                    
192900                                                                          
193000                                                                          
193100 L-KOLLA-SKRIV-WDK7-UPPDATERING SECTION.                                  
193200     MOVE 'L-KOLLA-SKRIV-K7' TO CURRENT-SECTION                           
193300                                                                          
193400     MOVE NDCCN-IDARTNR      TO W-IDARTNR                                 
193500     MOVE NDCCN-IDDC         TO W-IDDC                                    
193600     PERFORM IMS-GU-WDK711                                                
193700                                                                          
193800     IF SLAG-KVREFBER NOT = WUPD-KVREFBER                                 
193900     OR SLAG-KVREFPKT NOT = WUPD-KVREFPKT                                 
194000     OR SLAG-KVREFOVL NOT = WUPD-KVREFOVL                                 
194100     OR SLAG-TIREFPAF NOT = WUPD-TIREFPAF                                 
194200     OR SLAG-TIREFPKT NOT = WUPD-TIREFPKT                                 
194300                                                                          
194400        PERFORM S100-NOLLA-W22413-AREA                                    
194500                                                                          
194600        MOVE UPDATE-WDK711  TO UPD-IDPTYP                                 
194700        MOVE NDCCN-IDARTNR  TO UPD-IDARTNR                                
194800        MOVE NDCCN-IDDC     TO UPD-IDDC                                   
194900        MOVE WUPD-KVREFBER  TO UPD-KVREFBER                               
195000        MOVE WUPD-KVREFPKT  TO UPD-KVREFPKT                               
195100        MOVE WUPD-KVREFOVL  TO UPD-KVREFOVL                               
195200        MOVE WUPD-TIREFPAF  TO UPD-TIREFPAF                               
195300        MOVE WUPD-TIREFPKT  TO UPD-TIREFPKT                               
195400                                                                          
195500        PERFORM S12-SKRIV-W22413                                          
195600     END-IF                                                               
195700                                                                          
195800     IF NDCCN-KVSLAGER          NOT = WUPD-KVSLAGER                       
195900     OR NDCCN-KVEOQ             NOT = WUPD-KVEOQ                          
196000     OR NDCCN-TIMANSEC          NOT = WUPD-TIMANSEC                       
196100     OR NDCCN-KDLPSP            NOT = WUPD-KDLPSP                         
196200     OR NDCCN-KVSLUTKP          NOT = WUPD-KVSLUTKP                       
196300     OR NDCCN-TILPSP            NOT = WUPD-TILPSP                         
196400     OR NDCCN-KDLEVPLF          NOT = WUPD-KDLEVPLF                       
196500     OR NDCCN-DAPBPLAN          NOT = WUPD-DAPBPLAN                       
196600     OR NDCCN-KVPB-PLAN         NOT = WUPD-KVPB-PLAN                      
196700     OR NDCCN-DASEASON          NOT = WUPD-DASEASON                       
196800     OR NDCCN-RESEASON-PLAN(01) NOT = WUPD-RESEASON-PLAN(01)              
196900     OR NDCCN-RESEASON-PLAN(02) NOT = WUPD-RESEASON-PLAN(02)              
197000     OR NDCCN-RESEASON-PLAN(03) NOT = WUPD-RESEASON-PLAN(03)              
197100     OR NDCCN-RESEASON-PLAN(04) NOT = WUPD-RESEASON-PLAN(04)              
197200     OR NDCCN-RESEASON-PLAN(05) NOT = WUPD-RESEASON-PLAN(05)              
197300     OR NDCCN-RESEASON-PLAN(06) NOT = WUPD-RESEASON-PLAN(06)              
197400     OR NDCCN-RESEASON-PLAN(07) NOT = WUPD-RESEASON-PLAN(07)              
197500     OR NDCCN-RESEASON-PLAN(08) NOT = WUPD-RESEASON-PLAN(08)              
197600     OR NDCCN-RESEASON-PLAN(09) NOT = WUPD-RESEASON-PLAN(09)              
197700     OR NDCCN-RESEASON-PLAN(10) NOT = WUPD-RESEASON-PLAN(10)              
197800     OR NDCCN-RESEASON-PLAN(11) NOT = WUPD-RESEASON-PLAN(11)              
197900     OR NDCCN-RESEASON-PLAN(12) NOT = WUPD-RESEASON-PLAN(12)              
198000                                                                          
198100        PERFORM S100-NOLLA-W22413-AREA                                    
198200                                                                          
198300        MOVE UPDATE-WDK722          TO UPD-IDPTYP                         
198400        MOVE NDCCN-IDARTNR          TO UPD-IDARTNR                        
198500        MOVE NDCCN-IDDC             TO UPD-IDDC                           
198600        MOVE WUPD-KVSLAGER          TO UPD-KVSLAGER                       
198700        MOVE WUPD-KVEOQ             TO UPD-KVEOQ                          
198800        MOVE WUPD-TIMANSEC          TO UPD-TIMANSEC                       
198900        MOVE WUPD-KDLPSP            TO UPD-KDLPSP                         
199000        MOVE WUPD-KVSLUTKP          TO UPD-KVSLUTKP                       
199100        MOVE WUPD-TILPSP            TO UPD-TILPSP                         
199200        IF WUPD-KDLEVPLF = SPACE                                          
199300           MOVE JA                  TO UPD-KDLEVPLF                       
199400        ELSE                                                              
199500           MOVE WUPD-KDLEVPLF       TO UPD-KDLEVPLF                       
199600        END-IF                                                            
199700        MOVE WUPD-DAPBPLAN          TO UPD-DAPBPLAN                       
199800        MOVE WUPD-KVPB-PLAN         TO UPD-KVPB-PLAN                      
199900        MOVE WUPD-KVSLAGER          TO UPD-KVSLAGER                       
200000        MOVE WUPD-DASEASON          TO UPD-DASEASON                       
200100        MOVE WUPD-RESEASON-PLAN(01) TO UPD-RESEASON-PLAN(01)              
200200        MOVE WUPD-RESEASON-PLAN(02) TO UPD-RESEASON-PLAN(02)              
200300        MOVE WUPD-RESEASON-PLAN(03) TO UPD-RESEASON-PLAN(03)              
200400        MOVE WUPD-RESEASON-PLAN(04) TO UPD-RESEASON-PLAN(04)              
200500        MOVE WUPD-RESEASON-PLAN(05) TO UPD-RESEASON-PLAN(05)              
200600        MOVE WUPD-RESEASON-PLAN(06) TO UPD-RESEASON-PLAN(06)              
200700        MOVE WUPD-RESEASON-PLAN(07) TO UPD-RESEASON-PLAN(07)              
200800        MOVE WUPD-RESEASON-PLAN(08) TO UPD-RESEASON-PLAN(08)              
200900        MOVE WUPD-RESEASON-PLAN(09) TO UPD-RESEASON-PLAN(09)              
201000        MOVE WUPD-RESEASON-PLAN(10) TO UPD-RESEASON-PLAN(10)              
201100        MOVE WUPD-RESEASON-PLAN(11) TO UPD-RESEASON-PLAN(11)              
201200        MOVE WUPD-RESEASON-PLAN(12) TO UPD-RESEASON-PLAN(12)              
201300                                                                          
201400        PERFORM S12-SKRIV-W22413                                          
201500     END-IF                                                               
201600     .                                                                    
201700                                                                          
201800                                                                          
201900 Z-FINIT SECTION.                                                         
202000     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
202100                                                                          
202200     CLOSE W22418                                                         
202300           W22410                                                         
202400           W22412                                                         
202500           W22413                                                         
202600                                                                          
202700     MOVE 'S' TO POSTSUM-OPKOD                                            
202800     CALL POSTSUM USING POSTSUM-PARM                                      
202900     .                                                                    
203000                                                                          
203100                                                                          
203200 S01-LAES-W22418  SECTION.                                                
203300                                                                          
203400     READ W22418 INTO NDCCN-AREA                                          
203500     AT END                                                               
203600        MOVE HIGH-VALUE TO NDCCN-AREA                                     
203700        SET END-OF-W22418 TO TRUE                                         
203800                                                                          
203900     NOT AT END                                                           
204000        MOVE 'W22418'   TO POSTSUM-FDNAMN                                 
204100        MOVE 'W22412D1' TO POSTSUM-DDNAMN2                                
204200        MOVE SPACE      TO POSTSUM-TRANSTYP                               
204300        CALL POSTSUM USING POSTSUM-PARM                                   
204400     END-READ                                                             
204500     .                                                                    
204600                                                                          
204700                                                                          
204800 S02-LAES-W22410  SECTION.                                                
204900                                                                          
205000     READ W22410 INTO OMSP-AREA                                           
205100     AT END                                                               
205200        MOVE HIGH-VALUE TO OMSP-AREA                                      
205300        SET END-OF-W22410 TO TRUE                                         
205400                                                                          
205500     NOT AT END                                                           
205600        MOVE 'W22410'   TO POSTSUM-FDNAMN                                 
205700        MOVE 'W22412D2' TO POSTSUM-DDNAMN2                                
205800        MOVE SPACE      TO POSTSUM-TRANSTYP                               
205900        CALL POSTSUM USING POSTSUM-PARM                                   
206000     END-READ                                                             
206100     .                                                                    
206200                                                                          
206300                                                                          
206400 S10-NOLLA-BHDC-RESULTATFLT  SECTION.                                     
206500     MOVE 'S10-NOLLA-BHDC-RESULTATFLT ' TO CURRENT-SECTION                
206600                                                                          
206700     MOVE ZERO                   TO BHDC-KVBEHOV-SUMMA                    
206800     MOVE ZERO                   TO BHDC-KVBEHOV-DESSUTOM                 
206900     MOVE ZERO                   TO BHDC-TIBEHOV-FIRST                    
207000                                                                          
207100     MOVE 1   TO IX-BHDC                                                  
207200     PERFORM UNTIL IX-BHDC > IX-BHDC-MAX                                  
207300       MOVE ZERO                 TO BHDC-KVBEHOV-VECKA(IX-BHDC)           
207400                                                                          
207500       ADD 1  TO IX-BHDC                                                  
207600     END-PERFORM                                                          
207700                                                                          
207800     .                                                                    
207900     EJECT                                                                
208000 S11-SKRIV-W22412 SECTION.                                                
208100                                                                          
208200     WRITE LI12-POST FROM LI12-AREA                                       
208300                                                                          
208400     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
208500     MOVE 'W22412'   TO POSTSUM-FDNAMN                                    
208600     MOVE 'W22412D3' TO POSTSUM-DDNAMN2                                   
208700     CALL POSTSUM USING POSTSUM-PARM                                      
208800     .                                                                    
208900                                                                          
209000                                                                          
209100 S12-SKRIV-W22413 SECTION.                                                
209200                                                                          
209300     WRITE UPD-POST FROM UPD-AREA                                         
209400                                                                          
209500     MOVE UPD-IDPTYP TO POSTSUM-TRANSTYP                                  
209600     MOVE 'W22413'   TO POSTSUM-FDNAMN                                    
209700     MOVE 'W22412D4' TO POSTSUM-DDNAMN2                                   
209800     CALL POSTSUM USING POSTSUM-PARM                                      
209900     .                                                                    
210000                                                                          
210100                                                                          
210200 S99-ABEND SECTION.                                                       
210300                                                                          
210400     SKIP2                                                                
210500     MOVE 'S' TO POSTSUM-OPKOD                                            
210600     CALL POSTSUM USING POSTSUM-PARM                                      
210700     CALL ABEND USING RKOD-ABEND                                          
210800     .                                                                    
210900                                                                          
211000                                                                          
211100 S100-NOLLA-W22413-AREA SECTION.                                          
211200                                                                          
211300     MOVE SPACE   TO UPD-IDPTYP                                           
211400                     UPD-IDDC                                             
211500                     UPD-IDLEVNR                                          
211600                     UPD-KDLEVPLF                                         
211700     MOVE ZERO    TO UPD-IDARTNR                                          
211800                     UPD-DAAVROP-AVS                                      
211900                     UPD-DAPBPLAN                                         
212000                     UPD-DASEASON                                         
212100                     UPD-IDANSK                                           
212200                     UPD-KDAVROP                                          
212300                     UPD-KDLPSP                                           
212400                     UPD-KVAVROP                                          
212500                     UPD-KVEOQ                                            
212600                     UPD-KVPB-PLAN                                        
212700                     UPD-KVREFBER                                         
212800                     UPD-KVREFOVL                                         
212900                     UPD-KVREFPKT                                         
213000                     UPD-KVSLAGER                                         
213100                     UPD-KVSLUTKP                                         
213200                     UPD-TIAVRDAT-INL                                     
213300                     UPD-TIAVRDAT-DISP                                    
213400                     UPD-TILEVDAG                                         
213500                     UPD-TILPSP                                           
213600                     UPD-TIMANSEC                                         
213700                     UPD-TIREFPAF                                         
213800                     UPD-TIREFPKT                                         
213900     MOVE 1 TO IX                                                         
214000     PERFORM UNTIL IX > 12                                                
214100        MOVE ZERO TO UPD-RESEASON-PLAN(IX)                                
214200        ADD 1     TO IX                                                   
214300     END-PERFORM                                                          
214400     .                                                                    
214500                                                                          
214600                                                                          
214700* --- IMS SEKTIONER ---                                                   
214800                                                                          
214900 IMS-GU-WDK701 SECTION.                                                   
215000     MOVE 'IMS-GU-WDK701   ' TO CURRENT-IMS-SECTION                       
215100                                                                          
215200     MOVE SPACE               TO ALL-SSA                                  
215300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
215400          DELIMITED BY SIZE INTO SSA1                                     
215500     MOVE '  GE'              TO GODK-STATUSKODER                         
215600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
215700     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
215800     PERFORM IMS-STATUSKONTROLL                                           
215900     .                                                                    
216000                                                                          
216100                                                                          
216200 IMS-GNP-WDK711-REF SECTION.                                              
216300     MOVE 'IMS-GNP-WDK701-R' TO CURRENT-IMS-SECTION                       
216400                                                                          
216500     MOVE SPACE               TO ALL-SSA                                  
216600     STRING 'WDK711  (IDDC    >=' W-IDDC-K7-MIN                           
216700                    '&IDDC    <=' W-IDDC-K7-MAX                           
216800                    '&IDDCREF  =' W-IDDC-REF-X ')'                        
216900          DELIMITED BY SIZE INTO SSA1                                     
217000     MOVE '  GE'              TO GODK-STATUSKODER                         
217100     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
217200     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
217300     PERFORM IMS-STATUSKONTROLL                                           
217400     .                                                                    
217500                                                                          
217600                                                                          
217700 IMS-GU-WDK711   SECTION.                                                 
217800     MOVE 'IMS-GU-WDK711   ' TO CURRENT-IMS-SECTION                       
217900                                                                          
218000     MOVE SPACE               TO ALL-SSA                                  
218100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
218200          DELIMITED BY SIZE INTO SSA1                                     
218300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
218400          DELIMITED BY SIZE INTO SSA2                                     
218500     MOVE '    '              TO GODK-STATUSKODER                         
218600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
218700     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
218800     PERFORM IMS-STATUSKONTROLL                                           
218900     .                                                                    
219000                                                                          
219100                                                                          
219200 IMS-GU-WDD901 SECTION.                                                   
219300     MOVE 'IMS-GU-WDD901   ' TO CURRENT-IMS-SECTION                       
219400                                                                          
219500     MOVE SPACE               TO ALL-SSA                                  
219600     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
219700          DELIMITED BY SIZE INTO SSA1                                     
219800     MOVE '  GE'              TO GODK-STATUSKODER                         
219900     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
220000     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
220100     PERFORM IMS-STATUSKONTROLL                                           
220200     .                                                                    
220300                                                                          
220400                                                                          
220500 IMS-GU-WDD902 SECTION.                                                   
220600     MOVE 'IMS-GU-WDD902   ' TO CURRENT-IMS-SECTION                       
220700                                                                          
220800     MOVE SPACE               TO ALL-SSA                                  
220900     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
221000          DELIMITED BY SIZE INTO SSA1                                     
221100     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
221200          DELIMITED BY SIZE INTO SSA2                                     
221300     MOVE '  GE'              TO GODK-STATUSKODER                         
221400     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
221500     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
221600     PERFORM IMS-STATUSKONTROLL                                           
221700     .                                                                    
221800                                                                          
221900                                                                          
222000 IMS-GU-WDD902-O SECTION.                                                 
222100     MOVE 'IMS-GU-WDD902-O ' TO CURRENT-IMS-SECTION                       
222200                                                                          
222300     MOVE SPACE               TO ALL-SSA                                  
222400     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
222500          DELIMITED BY SIZE INTO SSA1                                     
222600     MOVE   'WDD902 '         TO SSA2                                     
222700     MOVE '  GE'              TO GODK-STATUSKODER                         
222800     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD902 SSA1 SSA2               
222900     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
223000     PERFORM IMS-STATUSKONTROLL                                           
223100     .                                                                    
223200                                                                          
223300                                                                          
223400 IMS-GU-WDD904 SECTION.                                                   
223500     MOVE 'IMS-GU-WDD904   ' TO CURRENT-IMS-SECTION                       
223600                                                                          
223700     MOVE SPACE               TO ALL-SSA                                  
223800     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
223900          DELIMITED BY SIZE INTO SSA1                                     
224000     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
224100          DELIMITED BY SIZE INTO SSA2                                     
224200     MOVE 'WDD904 '           TO SSA3                                     
224300     MOVE '  GE'              TO GODK-STATUSKODER                         
224400     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD904 SSA1 SSA2 SSA3          
224500     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
224600     PERFORM IMS-STATUSKONTROLL                                           
224700     .                                                                    
224800                                                                          
224900                                                                          
225000 IMS-GNP-WDD904 SECTION.                                                  
225100     MOVE 'IMS-GNP-WDD904  ' TO CURRENT-IMS-SECTION                       
225200                                                                          
225300     MOVE SPACE               TO ALL-SSA                                  
225400     MOVE 'WDD904 '           TO SSA1                                     
225500     MOVE '  GE'              TO GODK-STATUSKODER                         
225600     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD904 SSA1                   
225700     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
225800     PERFORM IMS-STATUSKONTROLL                                           
225900     .                                                                    
226000                                                                          
226100                                                                          
226200 IMS-GU-WDD905   SECTION.                                                 
226300     MOVE 'IMS-GU-WDD905   ' TO CURRENT-IMS-SECTION                       
226400                                                                          
226500     STRING 'WDD901    (WDD901KY =' W-WDD901KY-X ')'                      
226600            DELIMITED BY SIZE INTO SSA1                                   
226700     STRING 'WDD902    (IDLEVNR  =' W-IDLEVNR-X ')'                       
226800            DELIMITED BY SIZE INTO SSA2                                   
226900     STRING 'WDD905    (DAAVROP  =' W-DAAVROP-X                           
227000                      '&KDAVROP  =' W-KDAVROP-X ')'                       
227100            DELIMITED BY SIZE INTO SSA3                                   
227200     MOVE '  GE' TO GODK-STATUSKODER                                      
227300     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3          
227400     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
227500     PERFORM IMS-STATUSKONTROLL                                           
227600     .                                                                    
227700     EJECT                                                                
227800                                                                          
227900 IMS-GNP-WDD902-05-FIRST SECTION.                                         
228000     MOVE 'IMS-GNP-WDD902-05-FIRST' TO CURRENT-IMS-SECTION                
228100                                                                          
228200     MOVE SPACE               TO ALL-SSA                                  
228300     MOVE   'WDD902  *F'      TO SSA1                                     
228400     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
228500          DELIMITED BY SIZE INTO SSA2                                     
228600     MOVE '  GE'              TO GODK-STATUSKODER                         
228700     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2              
228800     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
228900     PERFORM IMS-STATUSKONTROLL                                           
229000     .                                                                    
229100     EJECT                                                                
229200                                                                          
229300 IMS-GNP-WDD902-05-NEXT SECTION.                                          
229400     MOVE 'IMS-GNP-WDD902-05-NEXT' TO CURRENT-IMS-SECTION                 
229500                                                                          
229600     MOVE SPACE               TO ALL-SSA                                  
229700     MOVE   'WDD902 '         TO SSA1                                     
229800     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
229900          DELIMITED BY SIZE INTO SSA2                                     
230000     MOVE '  GE'              TO GODK-STATUSKODER                         
230100     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2              
230200     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
230300     PERFORM IMS-STATUSKONTROLL                                           
230400     .                                                                    
230500     EJECT                                                                
230600                                                                          
230700 IMS-GNP-WDD905 SECTION.                                                  
230800     MOVE 'IMS-GNP-WDD905  ' TO CURRENT-IMS-SECTION                       
230900                                                                          
231000     MOVE SPACE               TO ALL-SSA                                  
231100     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
231200          DELIMITED BY SIZE INTO SSA1                                     
231300     MOVE '  GE'              TO GODK-STATUSKODER                         
231400     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
231500     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
231600     PERFORM IMS-STATUSKONTROLL                                           
231700     .                                                                    
231800                                                                          
231900                                                                          
232000 IMS-GN-WDB601 SECTION.                                                   
232100     MOVE 'IMS-GN-WDB601   ' TO CURRENT-IMS-SECTION                       
232200                                                                          
232300     MOVE  SPACE              TO ALL-SSA                                  
232400     MOVE 'WDB601 '           TO SSA1                                     
232500     MOVE '  GEGB'            TO GODK-STATUSKODER                         
232600     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601 SSA1                    
232700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
232800     PERFORM IMS-STATUSKONTROLL                                           
232900     .                                                                    
233000                                                                          
233100                                                                          
233200 IMS-GU-WDL601 SECTION.                                                   
233300     MOVE 'IMS-GU-WDL601   ' TO CURRENT-IMS-SECTION                       
233400                                                                          
233500     MOVE SPACE               TO ALL-SSA                                  
233600     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
233700          DELIMITED BY SIZE INTO SSA1                                     
233800     MOVE '  GE'              TO GODK-STATUSKODER                         
233900     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL601 SSA1                    
234000     MOVE WDL6-STATUS-CODE    TO STATUS-WS                                
234100     PERFORM IMS-STATUSKONTROLL                                           
234200     .                                                                    
234300                                                                          
234400                                                                          
234500 IMS-GU-WDF116 SECTION.                                                   
234600     MOVE 'IMS-GU-WDF116   ' TO CURRENT-IMS-SECTION                       
234700                                                                          
234800     MOVE SPACE               TO ALL-SSA                                  
234900     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
235000          DELIMITED BY SIZE INTO SSA1                                     
235100     STRING 'WDF116  (IDDC     =' W-IDDC-X ')'                            
235200          DELIMITED BY SIZE INTO SSA2                                     
235300     MOVE '  GE'              TO GODK-STATUSKODER                         
235400     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF116 SSA1 SSA2               
235500     MOVE WDF1-STATUS-CODE    TO STATUS-WS                                
235600     PERFORM IMS-STATUSKONTROLL                                           
235700     .                                                                    
235800                                                                          
235900                                                                          
236000 IMS-STATUSKONTROLL SECTION.                                              
236100                                                                          
236200     SET STATUS-IX TO 1                                                   
236300     SEARCH GODK-STATUS                                                   
236400       AT END                                                             
236500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
236600           DELIMITED BY SIZE INTO FELTEXT                                 
236700         DISPLAY FELTEXT                                                  
236800         CALL FELLOG                                                      
236900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
237000         CONTINUE                                                         
237100     END-SEARCH                                                           
237200     .                                                                    
237300     EJECT                                                                
237400*    -COPY WY2000P2                                                       
237500*                                                                         
