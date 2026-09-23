000100 ID DIVISION.                                                             
000200 PROGRAM-ID.        W1112000.                                             
000300*AUTHOR.            INGRID DANIELSSON.                                    
000400*DATE-WRITTEN.      APRIL 1979.                                           
000500*                                                                         
000600*        -------------------------------------------------                
000700*        !                                                !               
000800*        !   PROGRAMMET SKÖTER  ERSÄTTNINGS-BEVAKNINGEN   !               
000900*        !                                                !               
001000*        --------------------------------------------------               
001100*                                                                         
001200* CCID 5929265 -                                                          
001300*        TEMP. CHANGE OF SUPERSESSION RULES                               
001400*                                                                         
001500*    SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP1                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900     SKIP1                                                                
002000 FILE-CONTROL.                                                            
002100     SKIP1                                                                
002200     SELECT W111PP   ASSIGN W11120D2.                                     
002300     SELECT W11122   ASSIGN W11120D3.                                     
002400     SELECT W11146   ASSIGN W11120D4.                                     
002500     SELECT W11121   ASSIGN W11120D5.                                     
002600     SELECT W11123   ASSIGN W11120D6.                                     
002700     SELECT W11124   ASSIGN W11120D7.                                     
002800     SELECT W11125   ASSIGN W11120D8.                                     
002900     SELECT W11126   ASSIGN W11120D9.                                     
003000     SELECT W11127A  ASSIGN W11120DA.                                     
003100     SELECT W11127B  ASSIGN W11120DB.                                     
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP1                                                                
003500 FILE SECTION.                                                            
003600     SKIP2                                                                
003700 FD  W111PP                                                               
003800     RECORDING F                                                          
003900     BLOCK CONTAINS 0                                                     
004000                          .                                               
004100*01  PPMS-POST   -COPY W111PPMS -L.                                       
004200     EJECT                                                                
004300 FD  W11122                                                               
004400     RECORDING F                                                          
004500     BLOCK CONTAINS 0                                                     
004600                          .                                               
004700*01  W11122-POST   -COPY W21801 -L.                                       
004800     EJECT                                                                
004900 FD  W11146                                                               
005000     RECORDING F                                                          
005100     BLOCK CONTAINS 0                                                     
005200                          .                                               
005300*01  W11146-POST   -COPY A310TB65    -L.                                  
005400     EJECT                                                                
005500 FD  W11121                                                               
005600     RECORDING F                                                          
005700     BLOCK CONTAINS 0                                                     
005800                          .                                               
005900*01  W11121-POST   -COPY W11121      -L.                                  
006000     EJECT                                                                
006100 FD  W11123                                                               
006200     RECORDING F                                                          
006300     BLOCK CONTAINS 0.                                                    
006400                                                                          
006500*01  W11123-POST   -COPY W11123      -L.                                  
006600     EJECT                                                                
006700 FD  W11124                                                               
006800     RECORDING F                                                          
006900     BLOCK CONTAINS 0.                                                    
007000                                                                          
007100*01  W111240-POST   -COPY W111240     -L.                                 
007200*01  W111241-POST   -COPY W111241     -L.                                 
007300*01  W111242-POST   -COPY W111242     -L.                                 
007400*01  W111243-POST   -COPY W111243     -L.                                 
007500     EJECT                                                                
007600 FD  W11125                                                               
007700     RECORDING F                                                          
007800     BLOCK CONTAINS 0.                                                    
007900                                                                          
008000*01  W11125-POST   -COPY W111250     -L.                                  
008100*01  W111253-POST   -COPY W111253     -L.                                 
008200     EJECT                                                                
008300 FD  W11126                                                               
008400     RECORDING F                                                          
008500     BLOCK CONTAINS 0.                                                    
008600                                                                          
008700*01  W11126-POST   -COPY W222RP1     -L.                                  
008800     EJECT                                                                
008900 FD  W11127A                                                              
009000     RECORDING F                                                          
009100     BLOCK CONTAINS 0.                                                    
009200                                                                          
009300*01  -COPY W11127      -L.                                                
009400     EJECT                                                                
009500 FD  W11127B                                                              
009600     RECORDING F                                                          
009700     BLOCK CONTAINS 0.                                                    
009800                                                                          
009900*01  W11127B-POST   -COPY W11127      -L.                                 
010000     EJECT                                                                
010100 WORKING-STORAGE SECTION.                                                 
010200*    -COPY WY2000W2                                                       
010300     SKIP3                                                                
010400 77  IDPGM                  PIC X(8)       VALUE 'W1112000'.              
010500 77  JA                     PIC X                    VALUE 'J'.           
010600 77  NEJ                    PIC X                    VALUE 'N'.           
010700 77  WS-FND                 PIC X                    VALUE 'N'.           
010800 77  WS-CNT                 PIC 9                    VALUE ZERO.          
010900 77  WS-ACTV-INVTRY         PIC X                    VALUE 'N'.           
011000 77  FL-POST-TILL-W11122    PIC X                    VALUE 'N'.           
011100 77  INPUT-EOF-SW           PIC X                    VALUE 'N'.           
011200     88 INPUT-EOF                                    VALUE 'J'.           
011300     SKIP1                                                                
011400 77  PROGRAM-NAMN           PIC X(8)       VALUE 'W1112000'.              
011500     EJECT                                                                
011600 01  SWITCHAR.                                                            
011700     03  SALDO-SW.                                                        
011800         05  SALDO-OK             PIC X   OCCURS 2.                       
011900     03  FILLER          REDEFINES SALDO-SW.                              
012000         05  C1-SALDO             PIC X.                                  
012100             88  C1-SALDO-OK                         VALUE 'J'.           
012200         05  C2-SALDO             PIC X.                                  
012300             88  C2-SALDO-OK                         VALUE 'J'.           
012400                                                                          
012500     03  WS-LAGER-SW                  PIC X.                              
012600        88  WS-LAGER-OK              VALUE 'J'.                           
012700        88  WS-LAGER-SAKNAS          VALUE 'N'.                           
012800                                                                          
012900     03  WS-ART-LAGER-SW              PIC X.                              
013000          88  WS-ART-LAGER-OK          VALUE 'J'.                         
013100                                                                          
013200     03  WS-STDPR-SW                  PIC X.                              
013300        88  WS-STDPR-OK              VALUE 'J'.                           
013400        88  WS-STDPR-SAKNAS          VALUE 'N'.                           
013500                                                                          
013600     03  WS-ART-STDPR-SW              PIC X.                              
013700          88  WS-ART-STDPR-OK          VALUE 'J'.                         
013800                                                                          
013900     03  WS-TILLK-DDGS-SW             PIC X.                              
014000        88  WS-TILLK-DDGS-OK         VALUE 'J'.                           
014100        88  WS-TILLK-EJ-DDGS         VALUE 'N'.                           
014200                                                                          
014300     03  WS-ART-TILLK-DDGS-SW         PIC X.                              
014400        88  WS-ART-TILLK-DDGS-OK     VALUE 'J'.                           
014500                                                                          
014600     03  PPMS-TRANS               PIC X.                                  
014700     03  KDERS-SW.                                                        
014800         05  ANDRAD-KDERS-SW      PIC X   OCCURS 2.                       
014900     03  FILLER          REDEFINES KDERS-SW.                              
015000             88  NAGOT-KDERS-ANDRAT         VALUE 'JJ' THRU 'NJ'.         
015100             88  INGET-KDERS-ANDRAT         VALUE 'NN'.                   
015200         05  C1-KDERS             PIC X.                                  
015300             88  ANDRAD-KDERS-C1                     VALUE 'J'.           
015400         05  C2-KDERS             PIC X.                                  
015500             88  ANDRAD-KDERS-C2                     VALUE 'J'.           
015600                                                                          
015700                                                                          
015800     03  WS-KDERS                 PIC 9(2)  VALUE ZERO.                   
015900     03  FILLER REDEFINES WS-KDERS.                                       
016000         05  WS-KDERS-1           PIC 9.                                  
016100         05  WS-KDERS-2           PIC 9.                                  
016200                                                                          
016300     03  KDERS-BLIVIT-PREL-SW     PIC X              VALUE 'N'.           
016400         88  KDERS-BLIVIT-PREL                       VALUE 'J'.           
016500                                                                          
016600     03  AC04-UPPDATERAD-SW       PIC X              VALUE 'N'.           
016700         88  AC04-UPPDATERAD                         VALUE 'J'.           
016800                                                                          
016900     03  AA01-UPPDATERAD-SW       PIC X              VALUE 'N'.           
017000         88  AA01-UPPDATERAD                         VALUE 'J'.           
017100                                                                          
017200     03  AA11-UPPDATERAD-SW       PIC X              VALUE 'N'.           
017300         88  AA11-UPPDATERAD                         VALUE 'J'.           
017400                                                                          
017500     03  INGEN-INV-SW             PIC X              VALUE 'N'.           
017600         88  INGEN-INV                               VALUE 'J'.           
017700                                                                          
017800     EJECT                                                                
017900 01  ARBETS-FALT.                                                         
018000     03  KDSTATUS           PIC S9(3)   COMP-3     OCCURS 2.              
018100     03  TIERSDAT           PIC S9(5)   COMP-3     OCCURS 2.              
018200     03  KDERS-OLD          PIC S9(3)   COMP-3.                           
018300     03  SUM-KVLS           PIC S9(9)   COMP-3.                           
018400     03  SUM-KVPB           PIC S9(8)V9 COMP-3.                           
018500                                                                          
018600     03  W009VADD-DATUM     PIC S9(5)   COMP-3.                           
018700     03  W009VADD-ANTAL     PIC S9(3)   COMP-3.                           
018800                                                                          
018900     03  WS-KVAKS           PIC S9(7)   COMP-3 VALUE ZERO.                
019000     03  WS-RETUR           PIC S9(7)   COMP-3 VALUE ZERO.                
019100     03  WS-KVOKS           PIC S9(7)   COMP-3 VALUE ZERO.                
019200     03  WS-IDLEVNR-NUM     PIC 9(5)           VALUE ZERO.                
019300     03  NUM-IDINK          PIC 9(3)           VALUE ZERO.                
019300     03  WS-KVBEART         PIC S9(7)   COMP-3 VALUE ZERO.                
019400                                                                          
019500*                                                                         
019600*01    -COPY WWDCLAND                                                     
019700*                                                                         
019800*01    -COPY WWPRODSL                                                     
019900                                                                          
020000 01  WS-TABELL-IDDC                     VALUE SPACE.                      
020100     03  FILLER                         OCCURS 15.                        
020200         05  WS-TAB-IDDC     PIC X(2).                                    
020300         05  WS-TAB-IDLANDX2 PIC X(2).                                    
020400                                                                          
020500 01  WS-IX                  PIC S9(3)   VALUE +1    COMP-3.               
020600 01  WS-IX-MAX              PIC S9(3)   VALUE +15   COMP-3.               
020700 01  WS-DCLAND-IX           PIC S9(3)   VALUE +1    COMP-3.               
020800                                                                          
020900 01  WS-WDH1-PARTS.                                                       
021000     03 WDH1-PARTS-TAB OCCURS 500 TIMES INDEXED BY INDX.                  
021100        05 WS-WDH1-IDARTNR  PIC S9(9) COMP-3 VALUE 0.                     
021200                                                                          
021300*      --- VALID IDDC CODES                                               
021400*                                                                         
021500*01    -COPY WWDCKONS                                                     
021600*01    -COPY WWDC99                                                       
021700       EJECT                                                              
021800 01  AAVVD                  PIC 9(5).                                     
021900 01  FILLER REDEFINES AAVVD.                                              
022000     03  AAVV               PIC 9(4).                                     
022100     03  FILLER REDEFINES AAVV.                                           
022200         05  AA             PIC 9(2).                                     
022300         05  VV             PIC 9(2).                                     
022400     03  D                  PIC 9(1).                                     
022500                                                                          
022600 01  WS-PPMS-IDARTNR        PIC 9(9)  VALUE ZERO.                         
022700 01  WS-PPMS-IDARTNR-TILLK  PIC X(9)  VALUE ZERO.                         
022800 01  WS-ALT-ERS             PIC 9(2).                                     
022900     88 ALT-ERS             VALUE 04 14 24 05 25 06 26                    
023000                                  08 28.                                  
023100 01  NOLL-RAKNARE           PIC S9(5) COMP-3  VALUE ZERO.                 
023200                                                                          
023300 01  W-IDAVTAL-RED          PIC 9(13).                                    
023400 01  W-IDAVTAL REDEFINES W-IDAVTAL-RED.                                   
023500     03  FILLER             PIC X.                                        
023600     03  W-PREFIX           PIC X(3).                                     
023700     03  W-AVTALNR          PIC X(6).                                     
023800     03  W-SUFFIX           PIC X(3).                                     
023900                                                                          
024000 01  W-PREFIX-NUM           PIC 9(3) VALUE ZERO.                          
024100 01  W-IDARTNR-8            PIC 9(8) VALUE ZERO.                          
024200                                                                          
024300 01  SUBPROGRAM.                                                          
024400     03    CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                  
024500     03    FELLOG          PIC X(8)    VALUE 'FELLOG  '.                  
024600     03    DATKORT         PIC X(8)    VALUE 'DATKORT '.                  
024700     03    POSTSUM         PIC X(8)    VALUE 'POSTSUM '.                  
024800     03    W009VADD        PIC X(8)    VALUE 'W009VADD'.                  
024900     SKIP3                                                                
025000 01  NYCKLAR.                                                             
025100     03  W-IDARTNR-X.                                                     
025200       05  W-IDARTNR     PIC S9(9)  COMP-3.                               
025300     03  W-WDD901KY-X.                                                    
025400       05  W-IDARTNR-D9  PIC S9(9)  COMP-3.                               
025500       05  W-IDDC-D9     PIC X(2).                                        
025600     03  W-IDARTNR-T-X.                                                   
025700       05  W-IDARTNR-T   PIC S9(9)  COMP-3.                               
025800     03  W-IDPTYP-X.                                                      
025900       05  W-IDPTYP      PIC X(3)   VALUE SPACE.                          
026000     03  W-WDD713KY-X.                                                    
026100         05  W-DAREGDAT  PIC 9(8)          VALUE ZERO.                    
026200         05  W-TIKLOCK   PIC S9(9)  COMP-3 VALUE ZERO.                    
026300     03  W-WDH1KEY-MIN-X.                                                 
026400         05  W-IDDC-WDH1-MIN     PIC X(2)     VALUE SPACES.               
026500         05  W-KDINVKAT-MIN      PIC S9(3)    VALUE ZERO COMP-3.          
026600         05  W-TISEGKEY-MIN      PIC S9(9)    VALUE ZERO COMP-3.          
026700         05  W-DAREGDAT-SORT-MIN PIC 9(8)     VALUE ZERO.                 
026800                                                                          
026900     03  W-WDH1KEY-MAX-X.                                                 
027000         05  W-IDDC-WDH1-MAX     PIC X(2)  VALUE SPACES.                  
027100         05  W-KDINVKAT-MAX      PIC S9(3) VALUE +999 COMP-3.             
027200         05  W-TISEGKEY-MAX     PIC S9(9) VALUE +999999999 COMP-3.        
027300         05  W-DAREGDAT-SORT-MAX PIC 9(8)  VALUE 99999999.                
027400                                                                          
027500     EJECT                                                                
027600 01  PARAM-TILL-DATUMKORT.                                                
027700     03  PROG-ID         PIC X(8)      VALUE 'W1112000'.                  
027800     03  KORT-ID         PIC X(6)      VALUE 'WDATUM'.                    
027900     SKIP3                                                                
028000*    03  -COPY WDATKORT                                                   
028100     SKIP2                                                                
028200 01  DAGENS-DAGNR        PIC 9(5).                                        
028300 01  FILLER REDEFINES DAGENS-DAGNR.                                       
028400     03 DAGENS-AA        PIC 99.                                          
028500     03 DAGENS-VV        PIC 99.                                          
028600     03 DAGENS-D         PIC 9.                                           
028700                                                                          
028800 01  WS-DAGENS-DATUM-8   PIC 9(8).                                        
028900 01  WS-DATUM-X REDEFINES WS-DAGENS-DATUM-8.                              
029000     03 WS-SEKEL         PIC 9(2).                                        
029100     03 WS-AAMMDD        PIC 9(6).                                        
029200                                                                          
029300 01  WS-DAGENS-DATUM     PIC 9(6).                                        
029400 01  FILLER REDEFINES WS-DAGENS-DATUM.                                    
029500     03  WS-DAGENS-AA    PIC 99.                                          
029600     03  WS-DAGENS-MM    PIC 99.                                          
029700     03  WS-DAGENS-DD    PIC 99.                                          
029800                                                                          
029900     EJECT                                                                
030000*   ----- PARAMETRAR TILL POSTSUM                                         
030100*01  -COPY W0005       -PRE POSTSUM-.                                     
030200     EJECT                                                                
030300 01  UT-AREA-START               PIC X(24)   VALUE                        
030400                                             'UT-AREA-START'.             
030500     SKIP2                                                                
030600*01  -COPY W440004     -PRE UT-.                                          
030700     EJECT                                                                
030800*01  -COPY W111PPMS    -PRE UT-PPMS-.                                     
030900     EJECT                                                                
031000*01  AREA      -COPY W11121      -PRE INVUT-                              
031100     EJECT                                                                
031200*01  AREA      -COPY W21801      -PRE W11122-                             
031300     EJECT                                                                
031400*01  AREA      -COPY A310TB65    -PRE A310-.                              
031500     EJECT                                                                
031600*01  AREA      -COPY W11123      -PRE W11123-.                            
031700     EJECT                                                                
031800 01  W11124-AREA.                                                         
031900     03  W11124-IDPTYP   PIC X(3).                                        
032000     03  FILLER          PIC X(17).                                       
032100                                                                          
032200 01  W111240-AREA     REDEFINES W11124-AREA.                              
032300*    03  -COPY W111240                                                    
032400                                                                          
032500 01  W111241-AREA     REDEFINES W11124-AREA.                              
032600*    03  -COPY W111241                                                    
032700                                                                          
032800 01  W111242-AREA     REDEFINES W11124-AREA.                              
032900*    03  -COPY W111242                                                    
033000     EJECT                                                                
033100 01  W111243-AREA     REDEFINES W11124-AREA.                              
033200*    03  -COPY W111243                                                    
033300     EJECT                                                                
033400 01  W11125-AREA.                                                         
033500     03  FILLER          PIC X(20).                                       
033600                                                                          
033700 01  W111250-AREA     REDEFINES W11125-AREA.                              
033800*    03  -COPY W111250  -PRE W111250-                                     
033900                                                                          
034000 01  W111251-AREA     REDEFINES W11125-AREA.                              
034100*    03  -COPY W111251  -PRE W111251-                                     
034200                                                                          
034300 01  W111252-AREA     REDEFINES W11125-AREA.                              
034400*    03  -COPY W111252  -PRE W111252-                                     
034500                                                                          
034600 01  W111253-AREA     REDEFINES W11125-AREA.                              
034700*    03  -COPY W111253  -PRE W111253-                                     
034800     EJECT                                                                
034900*01  AREA      -COPY W222RP1     -PRE W11126-.                            
035000     EJECT                                                                
035100*01  AREA      -COPY W11127      -PRE W11127-.                            
035200     EJECT                                                                
035300 01  IMS-AREA-START              PIC X(24)   VALUE                        
035400                                             'IMS-AREA-START'.            
035500     SKIP2                                                                
035600*01  AREA      -COPY WDK601      -PRE AA01-.                              
035700     EJECT                                                                
035800*01  AREA      -COPY WDK611      -PRE AA11-.                              
035900     EJECT                                                                
036000*01  AREA      -COPY WDD902      -PRE INLB11-.                            
036100     EJECT                                                                
036200*01  AREA      -COPY WDD704      -PRE AC04-.                              
036300     EJECT                                                                
036400*01  AREA      -COPY WDD701      -PRE ERSA01-                             
036500     EJECT                                                                
036600*01  AREA      -COPY WDD702      -PRE ERSA11-                             
036700     EJECT                                                                
036800*01  AREA      -COPY WDK611      -PRE ARTC11-                             
036900     EJECT                                                                
037000*01  AREA      -COPY WDK623      -PRE ARTC23-                             
037100     EJECT                                                                
037200                                                                          
037300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD701'.                      
037400 01  DLI-IO-WDD701.                                                       
037500*    03  -COPY WDD701                                                     
037600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD713'.                      
037700 01  DLI-IO-WDD713.                                                       
037800*    03  -COPY WDD713                                                     
037900     EJECT                                                                
038000                                                                          
038100                                                                          
038200*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
038300*                                                                         
038400 01  IMS-WS.                                                              
038500     03     FILLER         PIC X(8)    VALUE 'IMS-WS  '.                  
038600*                                                                         
038700*                            *** STATUSKOD FRÅN IMS                       
038800     03  STATUS-WS         PIC XX.                                        
038900         88  SEGMENT-FINNS             VALUE '  '.                        
039000         88  INSERTEN-OK               VALUE '  '.                        
039100         88  SEGMENT-SAKNAS            VALUE 'GE'.                        
039200         88  SEGMENT-FANNS-REDAN       VALUE 'II'.                        
039300         88  BASEN-SLUT                VALUE 'GB'.                        
039400*                                                                         
039500*                            *** SEGMENTNIVÅ FRÅN IMS                     
039600     03  LEVEL-WS          PIC XX.                                        
039700         88  ROTEN-SAKNAS              VALUE '00'.                        
039800     SKIP3                                                                
039900     03    SSA1            PIC X(121).                                    
040000     03    SSA2            PIC X(121).                                    
040100     03    SSA3            PIC X(50).                                     
040200     SKIP3                                                                
040300     03    GODK-STATUSKODER.                                              
040400         05    GODK-STATUS OCCURS 3  INDEXED BY STATUS-IX PIC XX.         
040500     SKIP3                                                                
040600*01      -COPY W0003                                                      
040700     EJECT                                                                
040800 01      DLI-IO-AREA     PIC X(1000)  VALUE SPACE.                        
040900     SKIP3                                                                
041000 01      DLI-IO-AREA2.                                                    
041100*   03    AREA   -COPY WDK901      -PRE ARTM-                             
041200     EJECT                                                                
041300 01      DLI-IO-AREA3.                                                    
041400*   03    INLE21 -COPY WDL221                                             
041500     EJECT                                                                
041600 01      DLI-IO-AREA4.                                                    
041700*   03    WDD201 -COPY WDD201                                             
041800     EJECT                                                                
041900 01      DLI-IO-AREA5.                                                    
042000*   03   -COPY WDH101                                                     
042100     EJECT                                                                
042200 01      DLI-IO-AREA6.                                                    
042300*   03   -COPY WDH111                                                     
042400     EJECT                                                                
042500 LINKAGE SECTION.                                                         
042600     SKIP3                                                                
042700*01      -COPY W0008     -PRE ARTC1-                                      
042800     05 KONCAT-KEY      PIC X(10).                                        
042900     EJECT                                                                
043000*01      -COPY W0008     -PRE AC2-                                        
043100     05 KONCAT-KEY.                                                       
043200        07  AC2-IDARTNR     PIC S9(9)  COMP-3.                            
043300     EJECT                                                                
043400*01      -COPY W0008     -PRE ERSA-                                       
043500     05 KONCAT-KEY      PIC X(8).                                         
043600     EJECT                                                                
043700*01      -COPY W0008     -PRE WDD7-                                       
043800     05 KONCAT-KEY      PIC X(8).                                         
043900     EJECT                                                                
044000*01      -COPY W0008     -PRE ARTM-                                       
044100     05 KONCAT-KEY      PIC X(8).                                         
044200     EJECT                                                                
044300*01      -COPY W0008     -PRE ARTC2-                                      
044400     05 KONCAT-KEY      PIC X(8).                                         
044500     EJECT                                                                
044600*01      -COPY W0008     -PRE ARTG-                                       
044700     05 KONCAT-KEY      PIC X(8).                                         
044800     EJECT                                                                
044900*01      -COPY W0008     -PRE INLB-                                       
045000     05 KONCAT-KEY      PIC X(8).                                         
045100     EJECT                                                                
045200*01      -COPY W0008     -PRE INLE-                                       
045300     05 KONCAT-KEY      PIC X(8).                                         
045400     EJECT                                                                
045500*01      -COPY W0008     -PRE WDH1-                                       
045600     05 KONCAT-KEY      PIC X(8).                                         
045700     EJECT                                                                
045800 PROCEDURE DIVISION USING  ARTC1-PCB AC2-PCB ERSA-PCB                     
045900                           WDD7-PCB ARTM-PCB ARTC2-PCB                    
046000                           ARTG-PCB INLB-PCB INLE-PCB WDH1-PCB.           
046100     ENTRY 'DLITCBL' USING ARTC1-PCB AC2-PCB ERSA-PCB                     
046200                           WDD7-PCB ARTM-PCB ARTC2-PCB                    
046300                           ARTG-PCB INLB-PCB INLE-PCB WDH1-PCB.           
046400                                                                          
046500     PERFORM A-INIT                                                       
046600     PERFORM S14-LOAD-WDH1-TABLE                                          
046700     PERFORM UNTIL BASEN-SLUT                                             
046800       MOVE ALL 'N' TO SWITCHAR                                           
046900       MOVE 'N'  TO WS-ACTV-INVTRY                                        
047000       MOVE ZERO TO KDERS-OLD                                             
047100                    WS-CNT                                                
047200                                                                          
047300       PERFORM IMS-GET-AC04-MED-AC2                                       
047400       IF SEGMENT-FINNS                                                   
047500         MOVE DLI-IO-AREA TO AC04-AREA                                    
047600         IF AC04-KDSTATUS-C1 = 1 OR 2 OR 3 OR 6                           
047700           MOVE AC2-IDARTNR    TO W-IDARTNR                               
047800*****************                                                         
047900* SEARCH THE INPUT FILE TO CHECK IF THERE WAS AN ACTIVE                   
048000* INVENTORY FOR THE PART. IF NOT FOUND, CHECK WDH1 TO SEE IF              
048100* THERE IS ANY INVENTORY IN PROGRESS NOW.                                 
048200           SET INDX TO +1                                                 
048300           SEARCH WDH1-PARTS-TAB                                          
048400              AT END                                                      
048500                 MOVE AC2-IDARTNR TO W-IDARTNR                            
048600                 PERFORM B-CHK-ACTV-INVTRY                                
048700              WHEN WS-WDH1-IDARTNR(INDX) = W-IDARTNR                      
048800                 PERFORM B-CHK-ACTV-INVTRY                                
048900           END-SEARCH                                                     
049000*****************                                                         
049100           IF WS-ACTV-INVTRY NOT = JA                                     
049200             PERFORM S100-LAS-ARTREG                                      
049300             MOVE ZERO TO WS-KVOKS                                        
049400                                                                          
049500             PERFORM IMS-GET-ARTM01                                       
049600             IF SEGMENT-FINNS                                             
049700               MOVE ZERO TO WS-KVOKS                                      
049800               COMPUTE WS-KVOKS = ARTM-ART-KVOKS-BULK +                   
049900               ARTM-ART-KVOKS-DAG + ARTM-ART-KVOKS-VOR                    
050000             END-IF                                                       
050100                                                                          
050200             IF AC04-KDSTATUS-C1 = 1                                      
050300               PERFORM C-STATUS-1                                         
050400             ELSE                                                         
050500               IF AC04-KDSTATUS-C1 = 2                                    
050600                 PERFORM D-STATUS-2                                       
050700               END-IF                                                     
050800               IF AC04-KDSTATUS-C1 = 3                                    
050900                 PERFORM E-STATUS-3                                       
051000               ELSE                                                       
051100                 IF AC04-KDSTATUS-C1 = 6                                  
051200                   PERFORM F-STATUS-6                                     
051300                 END-IF                                                   
051400               END-IF                                                     
051500             END-IF                                                       
051600                                                                          
051700             IF NAGOT-KDERS-ANDRAT                                        
051800               PERFORM S10-BEHANDLA-FLTPO1                                
051900             END-IF                                                       
052000                                                                          
052100             IF AA01-UPPDATERAD OR AA11-UPPDATERAD                        
052200               PERFORM S200-UPPDATERA-ARTREG                              
052300             END-IF                                                       
052400                                                                          
052500             IF AC04-UPPDATERAD                                           
052600               PERFORM G-BEHANDLA-AC04                                    
052700             END-IF                                                       
052800                                                                          
052900             IF NAGOT-KDERS-ANDRAT                                        
053000**             IF AA11-CLAG-KDERS > 09                                    
053100**             AND AA11-CLAG-KDERS < 30                                   
053200**               PERFORM S7-TRANS-TILL-SPECPR                             
053300**             END-IF                                                     
053400                                                                          
053500               PERFORM S3-TRANS-TILL-SATSSYST                             
053600               PERFORM S8-TRANS-TILL-PPMS                                 
053700               PERFORM S6-TRANS-TILL-VR-SYST                              
053800               PERFORM S9-TRANS-TILL-BASLAGER                             
053900               PERFORM S210-POST-TILL-W11122-WDR5                         
054000                                                                          
054100             END-IF                                                       
054200             IF KDERS-BLIVIT-PREL                                         
054300               PERFORM S4-TRANS-TILL-INVENTERINGSSYST                     
054400               PERFORM S5-TRANS-TILL-PROGNOSSYST                          
054500             END-IF                                                       
054600           END-IF                                                         
054700         END-IF                                                           
054800       END-IF                                                             
054900     END-PERFORM                                                          
055000                                                                          
055100     CLOSE W111PP                                                         
055200           W11122                                                         
055300           W11146                                                         
055400           W11121                                                         
055500           W11123                                                         
055600           W11124                                                         
055700           W11125                                                         
055800           W11126                                                         
055900           W11127A                                                        
056000           W11127B                                                        
056100                                                                          
056200     MOVE 'S' TO POSTSUM-OPKOD                                            
056300     CALL POSTSUM USING POSTSUM-PARM                                      
056400     MOVE ZERO TO RETURN-CODE                                             
056500     GOBACK                                                               
056600     .                                                                    
056700     EJECT                                                                
056800 A-INIT SECTION.                                                          
056900     OPEN INPUT W11127A                                                   
057000     OPEN OUTPUT                                                          
057100            W111PP                                                        
057200            W11122                                                        
057300            W11146                                                        
057400            W11121                                                        
057500            W11123                                                        
057600            W11124                                                        
057700            W11125                                                        
057800            W11126                                                        
057900            W11127B                                                       
058000                                                                          
058100     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
058200                                                                          
058300     CALL DATKORT USING PROG-ID KORT-ID DATUMKORT                         
058400                                                                          
058500     MOVE D-AAR           TO DAGENS-AA                                    
058600                             WS-DAGENS-AA                                 
058700     MOVE D-VECKA         TO DAGENS-VV                                    
058800     MOVE D-DAGNR         TO DAGENS-D                                     
058900     MOVE D-MAANAD        TO WS-DAGENS-MM                                 
059000     MOVE D-DAG           TO WS-DAGENS-DD                                 
059100     MOVE WS-DAGENS-DATUM TO WS-AAMMDD                                    
059200     MOVE 20              TO WS-SEKEL                                     
059300                                                                          
059400     PERFORM AA-SKAPA-TAB-IDDC                                            
059500                                                                          
059600     .                                                                    
059700     EJECT                                                                
059800 AA-SKAPA-TAB-IDDC SECTION.                                               
059900                                                                          
060000     MOVE +1          TO WS-IX                                            
060100     MOVE +1          TO WS-DCLAND-IX                                     
060200                                                                          
060300     PERFORM UNTIL WS-DCLAND-IX > DCLAND-IX-MAX                           
060400       IF (DCLAND-USA (WS-DCLAND-IX) OR                                   
060500          DCLAND-CANADA (WS-DCLAND-IX))                                   
060600          IF WS-IX > WS-IX-MAX                                            
060700            DISPLAY 'WS-TABELL-IDDC BEHÖVER UTÖKAS'                       
060800            CALL FELLOG                                                   
060900          ELSE                                                            
061000            MOVE DCLAND-IDDC (WS-DCLAND-IX) TO WS-TAB-IDDC (WS-IX)        
061100          END-IF                                                          
061200          ADD +1          TO WS-IX                                        
061300       END-IF                                                             
061400       ADD +1             TO WS-DCLAND-IX                                 
061500     END-PERFORM                                                          
061600                                                                          
061700     .                                                                    
061800                                                                          
061900     EJECT                                                                
062000 B-CHK-ACTV-INVTRY SECTION.                                               
062100*** CHECK IF AN ACTIVE INVENTORY IS IN PROGRESS FOR CDC. THIS IS          
062200*** CHECKED BY CHECKING IF AN ENTRY EXISTS FOR CDC IN                     
062300*** WDH1 FOR A PART. IF YES, WRITE THE PARTS INTO A FILE WHICH            
062400*** WILL BE CHECKED IN THE NEXT RUN OF THE PGM.                           
062500     MOVE WC-CDC-SE                TO W-IDDC-WDH1-MIN                     
062600                                      W-IDDC-WDH1-MAX                     
062700     MOVE NEJ                      TO WS-FND                              
062800     PERFORM IMS-GU-WDH101                                                
062900     IF SEGMENT-FINNS                                                     
063000        PERFORM IMS-GNP-WDH111                                            
063100        PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                        
063200                                     OR WS-FND = JA                       
063300          IF SEGMENT-FINNS                                                
063400             IF INV-KDINVKAT = 3                                          
063500* IF THE 1ST RECORD RETURNED IS KAT 3, CHECK AGAIN TO CONFIRM             
063600* IF THERE IS ANY KAT OTHER THAN 3. JUST KAT 3 IS NOT ACTIVE              
063700* INVENTORY AND THE PART SHOULD NOT BE WRITTEN INTO THE FILE              
063800                PERFORM IMS-GNP-WDH111                                    
063900                IF SEGMENT-FINNS                                          
064000                   ADD +1                     TO WS-CNT                   
064100                   MOVE JA                    TO WS-FND                   
064200                   MOVE JA                    TO WS-ACTV-INVTRY           
064300                   MOVE W-IDARTNR             TO W11127-IDARTNR           
064400                   MOVE INV-KDINVKAT          TO W11127-KDINVKAT          
064500                   PERFORM S223-SKRIV-W11127                              
064600                END-IF                                                    
064700             ELSE                                                         
064800                ADD +1                        TO WS-CNT                   
064900                MOVE JA                       TO WS-FND                   
065000                MOVE JA                       TO WS-ACTV-INVTRY           
065100                MOVE W-IDARTNR                TO W11127-IDARTNR           
065200                MOVE INV-KDINVKAT             TO W11127-KDINVKAT          
065300                PERFORM S223-SKRIV-W11127                                 
065400             END-IF                                                       
065500          END-IF                                                          
065600        END-PERFORM                                                       
065700     END-IF                                                               
065800     .                                                                    
065900     EJECT                                                                
066000 C-STATUS-1 SECTION.                                                      
066100     SKIP3                                                                
066200     MOVE AC04-TIERSDAT-PREL-C1   TO TMP1-YYWWD                           
066300     MOVE DAGENS-DAGNR            TO TMP2-YYWWD                           
066400     PERFORM WY2000P2                                                     
066500     IF  TMP1-YYWWD <= TMP2-YYWWD                                         
066600       MOVE +0 TO AC04-KDSTATUS-C1                                        
066700       MOVE JA TO AC04-UPPDATERAD-SW                                      
066800                                                                          
066900       MOVE DAGENS-DAGNR TO AA01-ART-TIERSDAT                             
067000       MOVE JA TO AA01-UPPDATERAD-SW                                      
067100                                                                          
067200       ADD +20 TO AA11-CLAG-KDERS                                         
067300       MOVE JA TO AA11-UPPDATERAD-SW                                      
067400       MOVE JA TO ANDRAD-KDERS-SW(1)                                      
067500                                                                          
067600       PERFORM S11-SKAPA-B65                                              
067700       PERFORM S12-SKAPA-INV-TRANS                                        
067800       PERFORM S13-SKAPA-W111253-POST                                     
067900                                                                          
068000       IF AA11-CLAG-KDKSP = +4                                            
068100         MOVE +1 TO AA11-CLAG-KDKSP                                       
068200         MOVE JA TO AA11-UPPDATERAD-SW                                    
068300       END-IF                                                             
068400     END-IF                                                               
068500     .                                                                    
068600     EJECT                                                                
068700 D-STATUS-2 SECTION.                                                      
068800                                                                          
068900     MOVE AC04-TIERSDAT-PREL-C1 TO AAVVD                                  
069000     IF AA11-CLAG-KDERS = 01 OR 02 OR 05                                  
069100        IF AA11-CLAG-REDIRLEV < 1                                         
069200           MOVE NEJ TO WS-TILLK-DDGS-SW                                   
069300           PERFORM DC-KONTR-OM-TILLK-DDGS                                 
069400           IF WS-TILLK-EJ-DDGS                                            
069500              MOVE AAVV                  TO W009VADD-DATUM                
069600              MOVE -2                    TO W009VADD-ANTAL                
069700              CALL W009VADD USING W009VADD-DATUM W009VADD-ANTAL           
069800              MOVE W009VADD-DATUM TO AAVV                                 
069900           END-IF                                                         
070000        ELSE                                                              
070100           IF AA11-CLAG-KDERS = 02 OR 05                                  
070200              MOVE NEJ TO WS-TILLK-DDGS-SW                                
070300              PERFORM DC-KONTR-OM-TILLK-DDGS                              
070400              IF WS-TILLK-EJ-DDGS                                         
070500                 MOVE AAVV                  TO W009VADD-DATUM             
070600                 MOVE -2                    TO W009VADD-ANTAL             
070700                 CALL W009VADD USING W009VADD-DATUM W009VADD-ANTAL        
070800                 MOVE W009VADD-DATUM TO AAVV                              
070900              END-IF                                                      
071000           END-IF                                                         
071100        END-IF                                                            
071200     END-IF                                                               
071300                                                                          
071400     MOVE AAVVD         TO TMP1-YYWWD                                     
071500     MOVE DAGENS-DAGNR  TO TMP2-YYWWD                                     
071600     PERFORM WY2000P2                                                     
071700     IF  TMP1-YYWWD <= TMP2-YYWWD                                         
071800                                                                          
071900       IF AA11-CLAG-KDERS  = 01 OR 04                                     
072000                                                                          
072100                                                                          
072200         MOVE JA TO WS-STDPR-SW                                           
072300         PERFORM DB-KONTR-AV-STDPRIS                                      
072400         IF WS-STDPR-OK                                                   
072500            MOVE +3 TO AC04-KDSTATUS-C1                                   
072600            MOVE JA TO AC04-UPPDATERAD-SW                                 
072700         END-IF                                                           
072800       ELSE                                                               
072900         IF AA11-CLAG-KDERS = 02 OR 05                                    
073000                                                                          
073100           MOVE JA TO WS-LAGER-SW                                         
073200           PERFORM DA-KONTR-AV-LAGER                                      
073300           MOVE AA01-ART-KDPRODSL TO TEST-KDPRODSL                        
073400           IF WS-LAGER-OK                                                 
073500*** NEDANSTÅENDE RAD INLAGD AV MONICA FÖR USA-ERSÄTTN ***                 
073600             OR KDPRODSL-LOCAL                                            
073700                                                                          
073800              MOVE +0 TO AC04-KDSTATUS-C1                                 
073900              MOVE JA TO AC04-UPPDATERAD-SW                               
074000                                                                          
074100              MOVE DAGENS-DAGNR TO AA01-ART-TIERSDAT                      
074200              MOVE JA TO AA01-UPPDATERAD-SW                               
074300                                                                          
074400              ADD +20 TO AA11-CLAG-KDERS                                  
074500              MOVE JA TO AA11-UPPDATERAD-SW                               
074600              MOVE JA TO ANDRAD-KDERS-SW (1)                              
074700              PERFORM S11-SKAPA-B65                                       
074800              PERFORM S12-SKAPA-INV-TRANS                                 
074900              PERFORM S13-SKAPA-W111253-POST                              
075000                                                                          
075100              IF AA11-CLAG-KDKSP = +4                                     
075200                MOVE +1 TO AA11-CLAG-KDKSP                                
075300                MOVE JA TO AA11-UPPDATERAD-SW                             
075400              END-IF                                                      
075500           END-IF                                                         
075600         ELSE                                                             
075700           DISPLAY 'STATUS = 2 BÖR EJ FINNAS FÖR ARTIKEL '                
075800           W-IDARTNR                                                      
075900         END-IF                                                           
076000       END-IF                                                             
076100     END-IF                                                               
076200     .                                                                    
076300     EJECT                                                                
076400 DA-KONTR-AV-LAGER SECTION.                                               
076500******************************************************************        
076600* ÄT FEB 92  KONTROLL ATT TILLKOMMANDE ARTIKLAR FINNS I LAGER    *        
076700*            PÅ C1 INNAN ERSÄTTNINGEN BLIR DEF                   *        
076800******************************************************************        
076900     SKIP3                                                                
077000     PERFORM IMS-GET-ERSA01                                               
077100     IF SEGMENT-FINNS                                                     
077200        PERFORM IMS-GET-ERSA11                                            
077300        PERFORM UNTIL SEGMENT-SAKNAS OR WS-LAGER-SAKNAS                   
077400           MOVE DLI-IO-AREA TO ERSA11-AREA                                
077500           IF ERSA11-FLTEXT = JA                                          
077600              CONTINUE                                                    
077700           ELSE                                                           
077800              MOVE NEJ TO WS-ART-LAGER-SW                                 
077900              MOVE ERSA11-IDARTNR-TILLK TO W-IDARTNR-T                    
078000              PERFORM IMS-GET-ARTC01                                      
078100              PERFORM IMS-GET-ARTC11                                      
078200              IF SEGMENT-FINNS                                            
078300                 MOVE DLI-IO-AREA TO ARTC11-AREA                          
078400                 IF ARTC11-CLAG-REDIRLEV = 1                              
078500                    MOVE JA TO WS-ART-LAGER-SW                            
078600                 ELSE                                                     
078700                    IF ARTC11-CLAG-KVLS > ZERO                            
078800                       MOVE JA TO WS-ART-LAGER-SW                         
078900                    END-IF                                                
079000                 END-IF                                                   
079100              END-IF                                                      
079200              IF WS-ART-LAGER-OK                                          
079300                 MOVE JA TO WS-LAGER-SW                                   
079400              ELSE                                                        
079500                 IF ARTC11-CLAG-KDERS > 20                                
079600                    MOVE JA TO WS-LAGER-SW                                
079700                 ELSE                                                     
079800                    MOVE NEJ TO WS-LAGER-SW                               
079900                 END-IF                                                   
080000              END-IF                                                      
080100           END-IF                                                         
080200           PERFORM IMS-GET-ERSA11                                         
080300        END-PERFORM                                                       
080400     END-IF                                                               
080500     .                                                                    
080600     EJECT                                                                
080700 DB-KONTR-AV-STDPRIS SECTION.                                             
080800******************************************************************        
080900* ÄT MAJ 98  KONTROLL ATT TILLKOMMANDE ARTIKLAR HAR STDPRIS      *        
081000******************************************************************        
081100                                                                          
081200     PERFORM IMS-GET-ERSA01                                               
081300     IF SEGMENT-FINNS                                                     
081400        PERFORM IMS-GET-ERSA11                                            
081500        PERFORM UNTIL SEGMENT-SAKNAS OR WS-STDPR-SAKNAS                   
081600           MOVE DLI-IO-AREA TO ERSA11-AREA                                
081700           IF ERSA11-FLTEXT = JA                                          
081800              CONTINUE                                                    
081900           ELSE                                                           
082000              MOVE NEJ TO WS-ART-STDPR-SW                                 
082100              MOVE ERSA11-IDARTNR-TILLK TO W-IDARTNR-T                    
082200              PERFORM IMS-GET-ARTC01                                      
082300              PERFORM IMS-GET-ARTC11                                      
082400              IF SEGMENT-FINNS                                            
082500                 MOVE DLI-IO-AREA TO ARTC11-AREA                          
082600                 IF ARTC11-CLAG-PRARTSTD > ZERO   OR                      
082700                    ARTC11-CLAG-KDERS  > 20                               
082800                    MOVE JA TO WS-ART-STDPR-SW                            
082900                 END-IF                                                   
083000              END-IF                                                      
083100              IF WS-ART-STDPR-OK                                          
083200                 MOVE JA TO WS-STDPR-SW                                   
083300              ELSE                                                        
083400                 MOVE NEJ TO WS-STDPR-SW                                  
083500              END-IF                                                      
083600           END-IF                                                         
083700           PERFORM IMS-GET-ERSA11                                         
083800        END-PERFORM                                                       
083900     END-IF                                                               
084000     .                                                                    
084100     EJECT                                                                
084200 DC-KONTR-OM-TILLK-DDGS SECTION.                                          
084300     SKIP3                                                                
084400     PERFORM IMS-GET-ERSA01                                               
084500     IF SEGMENT-FINNS                                                     
084600        PERFORM IMS-GET-ERSA11                                            
084700        PERFORM UNTIL SEGMENT-SAKNAS OR WS-TILLK-DDGS-OK                  
084800           MOVE DLI-IO-AREA TO ERSA11-AREA                                
084900           IF ERSA11-FLTEXT = JA                                          
085000              CONTINUE                                                    
085100           ELSE                                                           
085200              MOVE NEJ TO WS-ART-TILLK-DDGS-SW                            
085300              MOVE ERSA11-IDARTNR-TILLK TO W-IDARTNR-T                    
085400              PERFORM IMS-GET-ARTC01                                      
085500              PERFORM IMS-GET-ARTC11                                      
085600              IF SEGMENT-FINNS                                            
085700                 MOVE DLI-IO-AREA TO ARTC11-AREA                          
085800                 IF ARTC11-CLAG-REDIRLEV = 1                              
085900                    MOVE JA TO WS-ART-TILLK-DDGS-SW                       
086000                 END-IF                                                   
086100              END-IF                                                      
086200              IF WS-ART-TILLK-DDGS-OK                                     
086300                IF (AA11-CLAG-KDERS  = 02 OR 05) AND                      
086400                    ARTC11-CLAG-KDERS <= 20                               
086500                  MOVE JA TO WS-TILLK-DDGS-SW                             
086600                ELSE                                                      
086700                  IF AA11-CLAG-KDERS  = 01                                
086800                     MOVE JA TO WS-TILLK-DDGS-SW                          
086900                  END-IF                                                  
087000                END-IF                                                    
087100              END-IF                                                      
087200           END-IF                                                         
087300           PERFORM IMS-GET-ERSA11                                         
087400        END-PERFORM                                                       
087500     END-IF                                                               
087600     .                                                                    
087700     EJECT                                                                
087800 E-STATUS-3  SECTION.                                                     
087900                                                                          
088000     IF AA11-CLAG-KDERS = 09 OR 01 OR 04                                  
088100        IF INLB11-KVBR = ZERO                                             
088200           MOVE AC04-TIERSDAT-REG TO AAVVD                                
088300           MOVE AAVV              TO W009VADD-DATUM                       
088400           MOVE +8                TO W009VADD-ANTAL                       
088500           CALL W009VADD USING W009VADD-DATUM W009VADD-ANTAL              
088600           MOVE W009VADD-DATUM TO AAVV                                    
088700           MOVE AAVVD         TO TMP1-YYWWD                               
088800           MOVE DAGENS-DAGNR  TO TMP2-YYWWD                               
088900           PERFORM WY2000P2                                               
089000           IF TMP2-YYWWD > TMP1-YYWWD                                     
089100              PERFORM S11-SKAPA-B65                                       
089200           END-IF                                                         
089300        END-IF                                                            
089400     END-IF                                                               
089500                                                                          
089600* CCID 5929265 -                                                          
089700     PERFORM IMS-GU-WDD701                                                
089800     IF SEGMENT-FINNS                                                     
089900       COMPUTE W-DAREGDAT = 99999999 -                                    
090000                            WS-DAGENS-DATUM-8                             
090100       END-COMPUTE                                                        
090200       PERFORM IMS-GNP-WDD713                                             
090300       IF SEGMENT-FINNS                                                   
090400         IF TMP-KDERSTMP = 'A' AND                                        
090500            TMP-DAREGDAT-9KOMPL = W-DAREGDAT                              
090600           PERFORM EB-KONTR-AV-SALDON                                     
090700         ELSE                                                             
090800           PERFORM EA-KONTR-AV-SALDON                                     
090900         END-IF                                                           
091000       ELSE                                                               
091100         PERFORM EA-KONTR-AV-SALDON                                       
091200       END-IF                                                             
091300     END-IF                                                               
091400                                                                          
091500     IF C1-SALDO-OK  AND  AC04-KDSTATUS-C1 = 3                            
091600       MOVE AA01-ART-KDPRODSL TO TEST-KDPRODSL                            
091700       IF KDPRODSL-LOCAL                                                  
091800          MOVE +0 TO AC04-KDSTATUS-C1                                     
091900          MOVE JA TO AC04-UPPDATERAD-SW                                   
092000          MOVE DAGENS-DAGNR TO AA01-ART-TIERSDAT                          
092100          MOVE JA TO AA01-UPPDATERAD-SW                                   
092200          ADD +20 TO AA11-CLAG-KDERS                                      
092300          MOVE JA TO AA11-UPPDATERAD-SW                                   
092400          MOVE JA TO ANDRAD-KDERS-SW (1)                                  
092500          PERFORM S12-SKAPA-INV-TRANS                                     
092600          PERFORM S13-SKAPA-W111253-POST                                  
092700          PERFORM S11-SKAPA-B65                                           
092800       ELSE                                                               
092900          IF INGEN-INV                                                    
093000             MOVE +0 TO AC04-KDSTATUS-C1                                  
093100             MOVE JA TO AC04-UPPDATERAD-SW                                
093200             MOVE DAGENS-DAGNR TO AA01-ART-TIERSDAT                       
093300             MOVE JA TO AA01-UPPDATERAD-SW                                
093400             ADD +20 TO AA11-CLAG-KDERS                                   
093500             MOVE JA TO AA11-UPPDATERAD-SW                                
093600             MOVE JA TO ANDRAD-KDERS-SW (1)                               
093700             PERFORM S11-SKAPA-B65                                        
093800             PERFORM S13-SKAPA-W111253-POST                               
093900          ELSE                                                            
094000             MOVE +4 TO AC04-KDSTATUS-C1                                  
094100             MOVE DAGENS-DAGNR TO AC04-TIERSDAT-PREL-C1                   
094200             MOVE JA TO AC04-UPPDATERAD-SW                                
094300             ADD +10 TO AA11-CLAG-KDERS                                   
094400             MOVE JA TO AA11-UPPDATERAD-SW                                
094500             MOVE JA TO ANDRAD-KDERS-SW (1)                               
094600             MOVE JA TO KDERS-BLIVIT-PREL-SW                              
094700             PERFORM S11-SKAPA-B65                                        
094800          END-IF                                                          
094900       END-IF                                                             
095000     END-IF                                                               
095100     IF NAGOT-KDERS-ANDRAT                                                
095200       IF AA11-CLAG-KDKSP = +4                                            
095300         MOVE +1 TO AA11-CLAG-KDKSP                                       
095400         MOVE JA TO AA11-UPPDATERAD-SW                                    
095500       END-IF                                                             
095600     END-IF                                                               
095700     .                                                                    
095800     EJECT                                                                
095900 EA-KONTR-AV-SALDON SECTION.                                              
096000*****************************************************************         
096100* ÄT 98:1     RETURER (KDRT 07) SKA INTE ÖKA AK-SALDO           *         
096200*****************************************************************         
096300                                                                          
096400     IF INLB11-KVBR = 0  OR AA01-ART-IDLEVNR = '1002 '                    
               IF AA11-CLAG-KVBEART < ZERO                                      
                 MOVE ZERO              TO WS-KVBEART                           
               ELSE                                                             
                 MOVE AA11-CLAG-KVBEART TO WS-KVBEART                           
               END-IF                                                           
096500                                                                          
096600         COMPUTE WS-KVAKS = AA11-CLAG-KVAKS-CDC +                         
096700                            AA11-CLAG-KVAKS-T   +                         
096800                            AA11-CLAG-KVAKS-PAV +                         
096900                            WS-KVBEART                                    
097000         IF WS-KVAKS > 0                                                  
097100            PERFORM IMS-GET-INLE01                                        
097200            IF SEGMENT-FINNS                                              
097300               MOVE '310' TO W-IDPTYP                                     
097400               PERFORM IMS-GET-INLE21                                     
097500               PERFORM UNTIL SEGMENT-SAKNAS                               
097600                  IF MOT-KDRT = 07                                        
097700                     COMPUTE WS-RETUR =                                   
097800                          MOT-KVAVIS - MOT-KVANTMOT                       
097900                     SUBTRACT WS-RETUR FROM WS-KVAKS                      
098000                  END-IF                                                  
098100                  PERFORM IMS-GET-INLE21                                  
098200               END-PERFORM                                                
098300            END-IF                                                        
098400         END-IF                                                           
098500         IF WS-KVAKS  < +1 AND                                            
098600           (AA11-CLAG-KVLS  - AA11-CLAG-KVRESS  -                         
098700            WS-KVOKS) < +1                                                
098800               MOVE JA TO SALDO-OK (1)                                    
098900         END-IF                                                           
099000     END-IF                                                               
099100                                                                          
099200*** INLAGT 970908 - INGEN INV KDERS DIREKT TILL > 20                      
099300                                                                          
099400     IF AA11-CLAG-KVLS       = 0                                          
099500     AND AA11-CLAG-ADLAGOMR  = 0                                          
099600     AND AA11-CLAG-PRARTSTD  = 0                                          
099700        MOVE JA TO INGEN-INV-SW                                           
099800        MOVE JA TO SALDO-OK (1)                                           
099900     END-IF                                                               
100000*** INLAGT 071220 - INGEN INV KDERS DIREKT TILL > 20                      
100100                                                                          
100200*    IF AA11-CLAG-KVLS      = 0                                           
100300*    AND (AA01-ART-KDPRODSL = 71 OR 74)                                   
100400*       MOVE JA TO INGEN-INV-SW                                           
100500*    END-IF                                                               
100600     .                                                                    
100700     EJECT                                                                
100800                                                                          
100900 EB-KONTR-AV-SALDON SECTION.                                              
101000*****************************************************************         
101100* CCID 5629265                                                  *         
101200*****************************************************************         
101300                                                                          
101400     IF INLB11-KVBR = 0  OR AA01-ART-IDLEVNR = '1002 '                    
101500                                                                          
101600       MOVE ZERO TO WS-KVAKS                                              
101700       MOVE AA11-CLAG-KVAKS-T TO WS-KVAKS                                 
101800       IF WS-KVAKS  < +1 AND                                              
101900          ((AA11-CLAG-KVLS   -                                            
102000          AA11-CLAG-KVRESS -                                              
102100          WS-KVOKS) < +1)                                                 
102200         MOVE JA TO SALDO-OK (1)                                          
102300         PERFORM S2-TRANS-AK-BEVAKN                                       
102400       END-IF                                                             
102500     END-IF                                                               
102600                                                                          
102700*** INLAGT 970908 - INGEN INV KDERS DIREKT TILL > 20                      
102800                                                                          
102900     IF AA11-CLAG-KVLS       = 0                                          
103000     AND AA11-CLAG-ADLAGOMR  = 0                                          
103100     AND AA11-CLAG-PRARTSTD  = 0                                          
103200        MOVE JA TO INGEN-INV-SW                                           
103300        MOVE JA TO SALDO-OK (1)                                           
103400     END-IF                                                               
103500*** INLAGT 071220 - INGEN INV KDERS DIREKT TILL > 20                      
103600                                                                          
103700*    IF AA11-CLAG-KVLS      = 0                                           
103800*    AND (AA01-ART-KDPRODSL = 71 OR 74)                                   
103900*       MOVE JA TO INGEN-INV-SW                                           
104000*    END-IF                                                               
104100     .                                                                    
104200     EJECT                                                                
104300                                                                          
104400 F-STATUS-6 SECTION.                                                      
104500                                                                          
104600     IF AC04-KDSTATUS-C1 = 6                                              
104700       IF AA11-CLAG-KVLS - AA11-CLAG-KVRESS - WS-KVOKS < +1               
104800           MOVE +0 TO AC04-KDSTATUS-C1                                    
104900           MOVE JA TO AC04-UPPDATERAD-SW                                  
105000           ADD +20 TO AA11-CLAG-KDERS                                     
105100           MOVE JA TO AA11-UPPDATERAD-SW                                  
105200           MOVE JA TO ANDRAD-KDERS-SW (1)                                 
105300        END-IF                                                            
105400     END-IF                                                               
105500                                                                          
105600     IF AC04-KDSTATUS-C1 = 0                                              
105700       MOVE DAGENS-DAGNR TO AA01-ART-TIERSDAT                             
105800       MOVE JA TO AA01-UPPDATERAD-SW                                      
105900       PERFORM S13-SKAPA-W111253-POST                                     
106000     END-IF                                                               
106100                                                                          
106200     IF NAGOT-KDERS-ANDRAT                                                
106300       IF AA11-CLAG-KDKSP = +4                                            
106400         MOVE +1 TO AA11-CLAG-KDKSP                                       
106500         MOVE JA TO AA11-UPPDATERAD-SW                                    
106600       END-IF                                                             
106700     END-IF                                                               
106800     .                                                                    
106900     EJECT                                                                
107000 G-BEHANDLA-AC04 SECTION.                                                 
107100***                                                                       
107200* SKRIVER POST PÅ FIL W11125 FÖR UPPDATERING                              
107300* AV WDD7                                                                 
107400***                                                                       
107500     MOVE '103'                 TO W111252-IDPTYP                         
107600     MOVE W-IDARTNR             TO W111252-IDARTNR                        
107700     MOVE AC04-KDSTATUS-C1      TO W111252-KDSTATUS-C1                    
107800     MOVE AC04-KDSTATUS-C2      TO W111252-KDSTATUS-C2                    
107900     MOVE AC04-TIERSDAT-PREL-C1 TO W111252-TIERSDAT-PREL-C1               
108000     MOVE AC04-TIERSDAT-PREL-C2 TO W111252-TIERSDAT-PREL-C2               
108100     PERFORM S222-SKRIV-W11125                                            
108200     .                                                                    
108300     EJECT                                                                
108400 S2-TRANS-AK-BEVAKN SECTION.                                              
108500***                                                                       
108600* SKRIVER POST PÅ FIL W11124 FÖR UPPLÄGG AV SEGMENT PÅ                    
108700* WDR5 HTYP 1133                                                          
108800***                                                                       
108900     MOVE SPACE           TO W111243-AREA                                 
109000     MOVE '243'           TO 1134-IDPTYP                                  
109100     MOVE W-IDARTNR       TO 1134-IDARTNR                                 
109200     MOVE 'I'             TO 1134-KDUPPD                                  
109300     PERFORM S221-SKRIV-W11124                                            
109400     .                                                                    
109500     EJECT                                                                
109600 S3-TRANS-TILL-SATSSYST SECTION.                                          
109700***                                                                       
109800* SKRIVER POST PÅ FIL W11124 FÖR UPPLÄGG AV SEGMENT PÅ                    
109900* WDG3 HTYP 2303                                                          
110000***                                                                       
110100     IF (AA01-ART-IDLEVNR = '1002 ') OR (AA01-ART-FLIART = JA)            
110200       MOVE '242'           TO 2303-IDPTYP                                
110300       MOVE ZERO            TO 2303-IDARTNR-SATS                          
110400       MOVE ZERO            TO 2303-IDARTNR-ING                           
110500       IF AA01-ART-IDLEVNR = '1002 '                                      
110600         MOVE W-IDARTNR     TO 2303-IDARTNR-SATS                          
110700       END-IF                                                             
110800       IF AA01-ART-FLIART = JA                                            
110900         MOVE W-IDARTNR     TO 2303-IDARTNR-ING                           
111000       END-IF                                                             
111100       MOVE AA11-CLAG-KDERS TO 2303-KDERS-NEW                             
111200       MOVE KDERS-OLD       TO 2303-KDERS-OLD                             
111300       MOVE DAGENS-DAGNR    TO 2303-TIERSDAT-PREL                         
111400                                                                          
111500       PERFORM S221-SKRIV-W11124                                          
111600     END-IF                                                               
111700     .                                                                    
111800     EJECT                                                                
111900 S4-TRANS-TILL-INVENTERINGSSYST SECTION.                                  
112000                                                                          
112100     IF KDERS-OLD  < AA11-CLAG-KDERS                                      
112200       MOVE W-IDARTNR  TO INVUT-IDARTNR                                   
112300       MOVE WC-CDC-SE  TO INVUT-IDDC                                      
112400       MOVE 3          TO INVUT-KDINVKAT                                  
112500                                                                          
112600       WRITE W11121-POST FROM INVUT-AREA                                  
112700                                                                          
112800       MOVE 'W11121'       TO POSTSUM-FDNAMN                              
112900       MOVE 'W11121D5'     TO POSTSUM-DDNAMN2                             
113000       MOVE SPACE          TO POSTSUM-TRANSTYP                            
113100       CALL POSTSUM USING POSTSUM-PARM                                    
113200     END-IF                                                               
113300     .                                                                    
113400     EJECT                                                                
113500 S5-TRANS-TILL-PROGNOSSYST SECTION.                                       
113600***                                                                       
113700* SKRIVER POST PÅ FIL W11126 MED RP1-TRANSAR TILL W222                    
113800***                                                                       
113900                                                                          
114000     IF KDERS-OLD < AA11-CLAG-KDERS                                       
114100        MOVE 'RP1'      TO W11126-IDPTYP                                  
114200        MOVE W-IDARTNR  TO W11126-IDARTNR                                 
114300        MOVE +0         TO W11126-KVPB-SEP                                
114400        MOVE +1         TO W11126-KDCLAGER                                
114500                                                                          
114600        PERFORM S225-SKRIV-W11126                                         
114700     END-IF                                                               
114800     .                                                                    
114900     EJECT                                                                
115000 S6-TRANS-TILL-VR-SYST SECTION.                                           
115100***                                                                       
115200* SKRIVER POST PÅ FIL W11123 FÖR UPPLÄGG AV SEGMENT PÅ                    
115300* WDG3 HTYP 9101                                                          
115400***                                                                       
115500                                                                          
115600     IF AA11-CLAG-KDERS = +0 OR > +10                                     
115700       MOVE '100'           TO W11123-IDPTYP                              
115800       MOVE W-IDARTNR       TO W11123-IDARTNR                             
115900       MOVE KDERS-OLD       TO W11123-KDERS-OLD                           
116000       MOVE AA11-CLAG-KDERS TO W11123-KDERS-NEW                           
116100                                                                          
116200       PERFORM S220-SKRIV-W11123                                          
116300     END-IF                                                               
116400     .                                                                    
116500     EJECT                                                                
116600*S7-TRANS-TILL-SPECPR SECTION.                                            
116700***                                                                       
116800* SKRIVER POST PÅ FIL W11127 FÖR UPPLÄGG AV SEGMENT PÅ                    
116900* WDG3 HTYP 3108                                                          
117000***                                                                       
117100                                                                          
117200*    MOVE '100'           TO W11127-IDPTYP                                
117300*    MOVE W-IDARTNR       TO W11127-IDARTNR                               
117400*    MOVE AA11-CLAG-KDERS TO W11127-KDERS                                 
117500*    PERFORM S223-SKRIV-W11127                                            
117600*    .                                                                    
117700 S7-ACTV-INV-FILE SECTION.                                                
117800***                                                                       
117900* SAVE PARTS WITH ACTIVE INVENTORY- TO BE USED IN THE NEXT RUN            
118000***                                                                       
118100                                                                          
118200*    .                                                                    
118300     EJECT                                                                
118400 S8-TRANS-TILL-PPMS SECTION.                                              
118500                                                                          
118600     MOVE NEJ TO PPMS-TRANS                                               
118700     IF KDERS-OLD < 10 AND AA11-CLAG-KDERS > 10                           
118800       MOVE JA TO PPMS-TRANS                                              
118900     ELSE                                                                 
119000       EVALUATE TRUE                                                      
119100       WHEN KDERS-OLD > 10 AND AA11-CLAG-KDERS < 10                       
119200         MOVE JA TO PPMS-TRANS                                            
119300       WHEN KDERS-OLD > 10 AND AA11-CLAG-KDERS > 10                       
119400         MOVE JA TO PPMS-TRANS                                            
119500       END-EVALUATE                                                       
119600     END-IF                                                               
119700     IF PPMS-TRANS = JA                                                   
119800       MOVE SPACE TO UT-PPMS-W111PPMS                                     
119900       MOVE 'RPP'                   TO UT-PPMS-IDPTYP                     
120000       MOVE W-IDARTNR               TO UT-PPMS-IDARTNR                    
120100       ACCEPT UT-PPMS-TIAAMMDD FROM DATE                                  
120200       ACCEPT UT-PPMS-TIKLOCK FROM TIME                                   
120300       IF AA11-CLAG-KDERS > 10                                            
120400         MOVE AA11-CLAG-KDERS   TO UT-PPMS-KDERS                          
120500       ELSE                                                               
120600         MOVE ZERO              TO UT-PPMS-KDERS                          
120700       END-IF                                                             
120800       MOVE AA11-CLAG-KDERS TO WS-ALT-ERS                                 
120900       IF AA11-CLAG-KDERS = ZERO                                          
121000         CONTINUE                                                         
121100       ELSE                                                               
121200         EVALUATE TRUE                                                    
121300         WHEN AA11-CLAG-KDERS = 09 OR 19 OR 29 OR 52                      
121400           CONTINUE                                                       
121500         WHEN ALT-ERS                                                     
121600           MOVE 'FLERA'              TO UT-PPMS-ANMARKNING                
121700          WHEN OTHER                                                      
121800           PERFORM IMS-GET-ERSA01                                         
121900           MOVE DLI-IO-AREA TO ERSA01-AREA                                
122000           IF ERSA01-KVKORT > 1                                           
122100             MOVE 'FLERA'           TO UT-PPMS-ANMARKNING                 
122200           ELSE                                                           
122300             MOVE ZERO TO NOLL-RAKNARE                                    
122400             PERFORM IMS-GET-ERSA11                                       
122500             MOVE DLI-IO-AREA TO ERSA11-AREA                              
122600             MOVE ERSA11-IDARTNR-TILLK TO WS-PPMS-IDARTNR                 
122700             MOVE WS-PPMS-IDARTNR TO WS-PPMS-IDARTNR-TILLK                
122800             INSPECT WS-PPMS-IDARTNR-TILLK TALLYING                       
122900             NOLL-RAKNARE FOR LEADING ZERO                                
123000             ADD +1 TO NOLL-RAKNARE                                       
123100             UNSTRING WS-PPMS-IDARTNR-TILLK INTO                          
123200             UT-PPMS-ANMARKNING WITH POINTER NOLL-RAKNARE                 
123300           END-IF                                                         
123400         END-EVALUATE                                                     
123500       END-IF                                                             
123600       WRITE PPMS-POST  FROM UT-PPMS-W111PPMS                             
123700       MOVE 'W111PP'   TO POSTSUM-FDNAMN                                  
123800       MOVE 'W11120D2' TO POSTSUM-DDNAMN2                                 
123900       MOVE 'UTP'      TO POSTSUM-TRANSTYP                                
124000       CALL POSTSUM USING POSTSUM-PARM                                    
124100     END-IF                                                               
124200     .                                                                    
124300     EJECT                                                                
124400 S9-TRANS-TILL-BASLAGER SECTION.                                          
124500*****************************************************************         
124600*  ÄT NOV 92  TRANS 1158 TILL ERSÄTTNINGSBEVAKNING BASLAGER VID           
124700*             NY EK > 10. TRANSEN LÄSES OCH DELEATAS I W115D1.            
124800*                                                                         
124900*****************************************************************         
125000                                                                          
125100     IF AA11-CLAG-KDERS > +10                                             
125200        PERFORM IMS-GET-ARTG01                                            
125300        IF SEGMENT-FINNS                                                  
125400           MOVE '240'           TO 1158-IDPTYP                            
125500           MOVE W-IDARTNR       TO 1158-IDARTNR                           
125600           MOVE AA11-CLAG-KDERS TO 1158-KDERS                             
125700           MOVE WS-DAGENS-DATUM TO 1158-TIREGDAT                          
125800           PERFORM S221-SKRIV-W11124                                      
125900        END-IF                                                            
126000     END-IF                                                               
126100     .                                                                    
126200     EJECT                                                                
126300 S10-BEHANDLA-FLTPO1 SECTION.                                             
126400*****************************************************************         
126500*  ÄT JAN 93  NÄR EN ARTIKEL BLIR DEFINITIVT ERSATT ,EK > 20,   *         
126600*             SKALL FLAGGA TPO1 SÄTTAS TILL NEJ.                *         
126700*     AUG 93  UPPDATERING FLTPO1 PÅ TILLK ART VID EK > 10       *         
126800*****************************************************************         
126900                                                                          
127000     IF AA11-CLAG-FLTPO1 = JA                                             
127100        IF AA11-CLAG-KDERS > 10                                           
127200           MOVE AA11-CLAG-KDERS TO WS-KDERS                               
127300           IF WS-KDERS-2 = 1 OR 2 OR 3                                    
127400              PERFORM IMS-GET-ERSA01                                      
127500              MOVE DLI-IO-AREA TO ERSA01-AREA                             
127600              IF ERSA01-KVKORT > 1                                        
127700                 CONTINUE                                                 
127800              ELSE                                                        
127900                 PERFORM IMS-GET-ERSA11                                   
128000                                                                          
128100                 IF SEGMENT-FINNS                                         
128200                    MOVE DLI-IO-AREA TO ERSA11-AREA                       
128300                    MOVE ERSA11-IDARTNR-TILLK TO W-IDARTNR-T              
128400                    PERFORM IMS-GU-ARTC11                                 
128500                    MOVE DLI-IO-AREA TO ARTC11-AREA                       
128600                                                                          
128700                    IF ARTC11-CLAG-FLTPO1 = NEJ                           
128800                       MOVE W-IDARTNR-T TO W111251-IDARTNR                
128900                       MOVE JA          TO W111251-FLTPO1                 
129000                       MOVE '102'       TO W111251-IDPTYP                 
129100                       PERFORM S222-SKRIV-W11125                          
129200                    END-IF                                                
129300                 END-IF                                                   
129400              END-IF                                                      
129500           END-IF                                                         
129600        END-IF                                                            
129700     END-IF                                                               
129800                                                                          
129900     IF AA11-CLAG-KDERS  > +20                                            
130000       IF AA11-CLAG-KDERS = +27 OR +28                                    
130100* ------ TILFÄLLIG ERS                                                    
130200         CONTINUE                                                         
130300       ELSE                                                               
130400         MOVE NEJ TO AA11-CLAG-FLTPO1                                     
130500         MOVE JA  TO AA11-UPPDATERAD-SW                                   
130600       END-IF                                                             
130700     END-IF                                                               
130800     .                                                                    
130900     EJECT                                                                
131000 S11-SKAPA-B65 SECTION.                                                   
131100*****************************************************************         
131200* ÄT JUNI 95  B65-POST TILL INKÖP SKAPAS OM AVTAL MED LEV FINNS *         
131300*             -  EK 09 NÄR BESTÄLLN REST = 0                    *         
131400*                (KDAVT SÄTTS DÅ TILL NOLL FÖR ATT MARKERA ATT  *         
131500*                 ARTIKELN SKICKATS TILL INKÖP)                 *         
131600*             -  ÖVR ERS NÄR STATUS > 2                         *         
131700*             -  EK 52 (1113)                                   *         
131800* ÄT 98:1     -  EK 09 01 04 NÄR KVBR = 0 OCH ERS.REG.DATUM     *         
131900*                PASSERAD MED 8 VECKOR ELLER EK BLIVIT PREL     *         
132000*****************************************************************         
132100                                                                          
132200     IF AA11-CLAG-IDINK (1:1) NOT NUMERIC                                 
132300        MOVE ZERO       TO NUM-IDINK                                      
132400     ELSE                                                                 
132500        IF AA11-CLAG-IDINK(1:3) NUMERIC                                   
132600           MOVE AA11-CLAG-IDINK(1:3) TO NUM-IDINK                         
132700        ELSE                                                              
132800           MOVE ZERO    TO NUM-IDINK                                      
132900        END-IF                                                            
133000     END-IF                                                               
133100                                                                          
133200     IF AA11-CLAG-KDAVT = 1                                               
133300        MOVE W-IDARTNR TO W-IDARTNR-T                                     
133400        PERFORM IMS-GET-ARTC01                                            
133500        PERFORM IMS-GET-ARTC23                                            
133600        PERFORM UNTIL SEGMENT-SAKNAS                                      
133700           MOVE DLI-IO-AREA TO ARTC23-AREA                                
133800           MOVE ARTC23-AVT-IDAVTAL TO W-IDAVTAL-RED                       
133900           MOVE W-PREFIX TO W-PREFIX-NUM                                  
134000***             VI INKLUDERAR ÄVEN NAP AVTAL,PREFIX = 004                 
134100           IF (W-PREFIX-NUM > 99 AND W-PREFIX-NUM < 790) OR               
134200           (W-PREFIX-NUM > 799 AND W-PREFIX-NUM < 987) OR                 
134300           (W-PREFIX-NUM > 987 AND W-PREFIX-NUM < 1000) OR                
134400           (W-PREFIX-NUM = 004)                                           
134500               IF ARTC23-AVT-IDLEVNR-AVT (5:1) = SPACE                    
134600***            LEVNUM SKALL TILLS VIDARE VARA NUMERISKT I X(5)            
134700                  MOVE ZERO                   TO TALLY                    
134800                  INSPECT ARTC23-AVT-IDLEVNR-AVT TALLYING TALLY           
134900                          FOR CHARACTERS BEFORE INITIAL SPACE             
135000                  IF TALLY = ZERO                                         
135100                     MOVE ZERO                TO WS-IDLEVNR-NUM           
135200                  ELSE                                                    
135300                     MOVE ARTC23-AVT-IDLEVNR-AVT (1:TALLY)                
135400                                          TO WS-IDLEVNR-NUM               
135500                  END-IF                                                  
135600                  MOVE WS-IDLEVNR-NUM         TO A310-LEVNUM              
135700               ELSE                                                       
135800                  MOVE ARTC23-AVT-IDLEVNR-AVT TO A310-LEVNUM              
135900               END-IF                                                     
136000               MOVE ARTC23-AVT-IDAVTAL     TO W-IDAVTAL-RED               
136100               MOVE W-PREFIX               TO A310-BESTPREF               
136200               MOVE W-AVTALNR              TO A310-BESTLNR                
136300               MOVE W-SUFFIX               TO A310-BESTSUFF               
136400               MOVE SPACE                  TO A310-LEVNUM-GODSM           
136500                                              A310-ANT-BESTANN            
136600               MOVE 'B65'                  TO A310-KT                     
136700               MOVE WS-DAGENS-DATUM        TO A310-DATUM-UTSKR            
136800               MOVE W-IDARTNR              TO W-IDARTNR-8                 
136900               MOVE W-IDARTNR-8            TO A310-ARTNR                  
137000                                                                          
137100               PERFORM S111-SKRIV-W11146                                  
137200                                                                          
137300               IF AA11-CLAG-KDERS = 09 OR 01 OR 04 OR                     
137400                                   19 OR 11 OR 14                         
137500                  MOVE ZERO TO AA11-CLAG-KDAVT                            
137600                  MOVE JA TO AA11-UPPDATERAD-SW                           
137700               END-IF                                                     
137800           ELSE                                                           
137900              IF (W-PREFIX-NUM > 639 AND W-PREFIX-NUM < 660)              
138000                 PERFORM S11A-SKRIV-1142                                  
138100                 IF AA11-CLAG-KDERS = 09 OR 01 OR 04 OR                   
138200                                    19 OR 11 OR 14                        
138300                    MOVE ZERO TO AA11-CLAG-KDAVT                          
138400                    MOVE JA TO AA11-UPPDATERAD-SW                         
138500                 END-IF                                                   
138600              END-IF                                                      
138700           END-IF                                                         
138800                                                                          
138900           PERFORM IMS-GET-ARTC23                                         
139000        END-PERFORM                                                       
139100     END-IF                                                               
139200     .                                                                    
139300     EJECT                                                                
139400 S12-SKAPA-INV-TRANS SECTION.                                             
139500******************************************************************        
139600* SAMTLIGA ARTIKLAR MED NY EK > 20 SKICKAS VIDARE TILL INV       *        
139700* ÄT FÖR NDC                                                     *        
139800******************************************************************        
139900     MOVE W-IDARTNR  TO INVUT-IDARTNR                                     
140000     MOVE 3          TO INVUT-KDINVKAT                                    
140100                                                                          
140200     MOVE 'W11121'       TO POSTSUM-FDNAMN                                
140300     MOVE 'W11121D5'     TO POSTSUM-DDNAMN2                               
140400     MOVE SPACE          TO POSTSUM-TRANSTYP                              
140500                                                                          
140600     MOVE +1             TO WS-IX                                         
140700     PERFORM UNTIL WS-IX > WS-IX-MAX OR                                   
140800       WS-TAB-IDDC (WS-IX) = SPACE                                        
140900                                                                          
141000       MOVE WS-TAB-IDDC (WS-IX) TO INVUT-IDDC                             
141100       WRITE W11121-POST FROM INVUT-AREA                                  
141200       CALL POSTSUM USING POSTSUM-PARM                                    
141300       ADD +1            TO WS-IX                                         
141400     END-PERFORM                                                          
141500                                                                          
141600     .                                                                    
141700     EJECT                                                                
141800 S13-SKAPA-W111253-POST SECTION.                                          
141900                                                                          
142000     PERFORM IMS-GET-ARTG01                                               
142100     IF SEGMENT-FINNS                                                     
142200        IF ART-KDANSKQ NOT = '9'                                          
142300           MOVE W-IDARTNR    TO W111253-IDARTNR                           
142400           MOVE 0            TO W111253-KDANSKQ                           
142500           MOVE '104'        TO W111253-IDPTYP                            
142600                                                                          
142700           CALL POSTSUM USING POSTSUM-PARM                                
142800           WRITE W111253-POST  FROM W11125-AREA                           
142900           MOVE 'W11125'       TO POSTSUM-FDNAMN                          
143000           MOVE 'W11120D8'     TO POSTSUM-DDNAMN2                         
143100           MOVE '104'          TO POSTSUM-TRANSTYP                        
143200           CALL POSTSUM USING POSTSUM-PARM                                
143300                                                                          
143400           IF ART-KDANSKQ = '2'                                           
143500              MOVE SPACE     TO W111241-AREA                              
143600              MOVE '241'     TO 1142-IDPTYP                               
143700              MOVE W-IDARTNR TO 1142-IDARTNR                              
143800              MOVE '1'       TO 1142-KDSEGKEY                             
143900              PERFORM S221-SKRIV-W11124                                   
144000           END-IF                                                         
144100        END-IF                                                            
144200     END-IF                                                               
144300     EJECT                                                                
144400     .                                                                    
144500 S14-LOAD-WDH1-TABLE SECTION.                                             
144600                                                                          
144700     SET INDX                  TO +1                                      
144800     PERFORM S14A-READ-INPUT                                              
144900     PERFORM UNTIL INPUT-EOF                                              
145000         MOVE W11127-IDARTNR   TO WS-WDH1-IDARTNR(INDX)                   
145100         SET INDX UP BY +1                                                
145200         PERFORM S14A-READ-INPUT                                          
145300     END-PERFORM                                                          
145400     EJECT                                                                
145500     .                                                                    
145600 S14A-READ-INPUT SECTION.                                                 
145700                                                                          
145800     READ W11127A       INTO W11127-AREA                                  
145900     AT END                                                               
146000       SET INPUT-EOF      TO TRUE                                         
146100     END-READ                                                             
146200     .                                                                    
146300     EJECT                                                                
146400 S11A-SKRIV-1142 SECTION.                                                 
146500***                                                                       
146600* SKRIVER POST PÅ FILEN W11124 FÖR UPPDATERING                            
146700* AV WDG3 HTYP 1142                                                       
146800***                                                                       
146900                                                                          
147000     MOVE '241'     TO 1142-IDPTYP                                        
147100     MOVE W-IDARTNR TO 1142-IDARTNR                                       
147200     MOVE '2'       TO 1142-KDSEGKEY                                      
147300     MOVE 'A'       TO 1142-KDSVAR                                        
147400     PERFORM S221-SKRIV-W11124                                            
147500     .                                                                    
147600     EJECT                                                                
147700 S111-SKRIV-W11146 SECTION.                                               
147800     SKIP3                                                                
147900     WRITE W11146-POST FROM A310-AREA                                     
148000     MOVE 'W11146'     TO POSTSUM-FDNAMN                                  
148100     MOVE 'W11120D4'   TO POSTSUM-DDNAMN2                                 
148200     MOVE SPACE        TO POSTSUM-TRANSTYP                                
148300     CALL POSTSUM USING POSTSUM-PARM                                      
148400     .                                                                    
148500     EJECT                                                                
148600 S100-LAS-ARTREG SECTION.                                                 
148700*****************************************************************         
148800*  ÄT FEB 92  VID KONTROLL AV BESTÄLLNINGSREST UNDANTAS LEVNR   *         
148900*             8261 (RENOVERAR BYTES-OBJEKT)                     *         
149000*****************************************************************         
149100     PERFORM IMS-GET-AA01                                                 
149200     IF SEGMENT-SAKNAS                                                    
149300       DISPLAY 'ARTIKEL SAKNAS PÅ ARTREG ' W-IDARTNR                      
149400       CALL FELLOG                                                        
149500     ELSE                                                                 
149600       MOVE DLI-IO-AREA TO AA01-AREA                                      
149700                                                                          
149800       PERFORM IMS-GET-AA11                                               
149900       MOVE DLI-IO-AREA TO AA11-AREA                                      
150000       MOVE AA11-CLAG-KDERS TO KDERS-OLD                                  
150100                                                                          
150200       MOVE ZERO TO INLB11-KVBR                                           
150300       MOVE SPACE TO STATUS-WS                                            
150400       MOVE AC2-IDARTNR   TO W-IDARTNR-D9                                 
150500       MOVE WC-CDC-SE     TO W-IDDC-D9                                    
150600       PERFORM IMS-GET-INLB01                                             
150700       IF SEGMENT-FINNS                                                   
150800          PERFORM UNTIL (SEGMENT-SAKNAS) OR (INLB11-KVBR > ZERO)          
150900             PERFORM IMS-GNP-INLB11                                       
151000             IF SEGMENT-FINNS                                             
151100                MOVE DLI-IO-AREA TO INLB11-AREA                           
151200                IF INLB11-IDLEVNR = '8261 '                               
151300                   MOVE ZERO TO INLB11-KVBR                               
151400                END-IF                                                    
151500             END-IF                                                       
151600          END-PERFORM                                                     
151700       END-IF                                                             
151800     END-IF                                                               
151900     .                                                                    
152000     EJECT                                                                
152100 S200-UPPDATERA-ARTREG SECTION.                                           
152200***                                                                       
152300* SKRIVER POST PÅ FILEN W11125 FÖR UPPDATERING                            
152400* AV WDK601/11                                                            
152500***                                                                       
152600     IF AA01-UPPDATERAD                                                   
152700       MOVE '100'             TO W111250-IDPTYP                           
152800       MOVE W-IDARTNR         TO W111250-IDARTNR                          
152900       MOVE AA01-ART-TIERSDAT TO W111250-TIERSDAT                         
153000       PERFORM S222-SKRIV-W11125                                          
153100     END-IF                                                               
153200     IF AA11-UPPDATERAD                                                   
153300       MOVE '101'             TO W111250-IDPTYP                           
153400       MOVE W-IDARTNR         TO W111251-IDARTNR                          
153500       MOVE AA11-CLAG-FLTPO1  TO W111251-FLTPO1                           
153600       MOVE AA11-CLAG-KDERS   TO W111251-KDERS                            
153700       MOVE AA11-CLAG-KDKSP   TO W111251-KDKSP                            
153800       MOVE AA11-CLAG-KDAVT   TO W111251-KDAVT                            
153900       PERFORM S222-SKRIV-W11125                                          
154000     END-IF                                                               
154100     .                                                                    
154200     EJECT                                                                
154300  S210-POST-TILL-W11122-WDR5 SECTION.                                     
154400                                                                          
154500     IF NAGOT-KDERS-ANDRAT                                                
154600       IF (KDERS-OLD < 10 AND AA11-CLAG-KDERS > 10)                       
154700         MOVE JA TO FL-POST-TILL-W11122                                   
154800       END-IF                                                             
154900       IF (KDERS-OLD > 10 AND AA11-CLAG-KDERS < 10)                       
155000         MOVE JA TO FL-POST-TILL-W11122                                   
155100       END-IF                                                             
155200     END-IF                                                               
155300     IF FL-POST-TILL-W11122 = JA                                          
155400       PERFORM S211-SKAPA-POST-TILL-W11122                                
155500     END-IF                                                               
155600     .                                                                    
155700     EJECT                                                                
155800  S211-SKAPA-POST-TILL-W11122 SECTION.                                    
155900                                                                          
156000     MOVE W-IDARTNR TO W11122-IDARTNR                                     
156100     PERFORM S212-SKRIV-W11122                                            
156200     .                                                                    
156300     EJECT                                                                
156400  S212-SKRIV-W11122 SECTION.                                              
156500                                                                          
156600     WRITE W11122-POST   FROM W11122-AREA                                 
156700     MOVE 'W11122'       TO POSTSUM-FDNAMN                                
156800     MOVE 'W11120D3'     TO POSTSUM-DDNAMN2                               
156900     MOVE SPACE          TO POSTSUM-TRANSTYP                              
157000     CALL POSTSUM USING POSTSUM-PARM                                      
157100     .                                                                    
157200     EJECT                                                                
157300  S220-SKRIV-W11123 SECTION.                                              
157400                                                                          
157500     WRITE W11123-POST   FROM W11123-AREA                                 
157600     MOVE 'W11123'       TO POSTSUM-FDNAMN                                
157700     MOVE 'W11120D6'     TO POSTSUM-DDNAMN2                               
157800     MOVE SPACE          TO POSTSUM-TRANSTYP                              
157900     CALL POSTSUM USING POSTSUM-PARM                                      
158000     .                                                                    
158100     EJECT                                                                
158200  S221-SKRIV-W11124 SECTION.                                              
158300                                                                          
158400     IF      W11124-IDPTYP = '240'                                        
158500       WRITE W111240-POST   FROM W111240-AREA                             
158600     ELSE IF W11124-IDPTYP = '241'                                        
158700       WRITE W111241-POST   FROM W111241-AREA                             
158800     ELSE IF W11124-IDPTYP = '242'                                        
158900       WRITE W111242-POST   FROM W111242-AREA                             
159000     ELSE IF W11124-IDPTYP = '243'                                        
159100       WRITE W111243-POST   FROM W111243-AREA                             
159200     ELSE                                                                 
159300       DISPLAY 'W11120 - FEL POSTTYP PÅ W11124 ' W11124-IDPTYP            
159400       CALL FELLOG                                                        
159500     END-IF END-IF END-IF END-IF                                          
159600                                                                          
159700     MOVE 'W11124'       TO POSTSUM-FDNAMN                                
159800     MOVE 'W11120D7'     TO POSTSUM-DDNAMN2                               
159900     MOVE W11124-IDPTYP  TO POSTSUM-TRANSTYP                              
160000     CALL POSTSUM USING POSTSUM-PARM                                      
160100     .                                                                    
160200     EJECT                                                                
160300  S222-SKRIV-W11125 SECTION.                                              
160400                                                                          
160500     WRITE W11125-POST   FROM W11125-AREA                                 
160600     MOVE 'W11125'       TO POSTSUM-FDNAMN                                
160700     MOVE 'W11120D8'     TO POSTSUM-DDNAMN2                               
160800     MOVE SPACE          TO POSTSUM-TRANSTYP                              
160900     CALL POSTSUM USING POSTSUM-PARM                                      
161000     .                                                                    
161100     EJECT                                                                
161200  S223-SKRIV-W11127 SECTION.                                              
161300                                                                          
161400     WRITE W11127B-POST  FROM W11127-AREA                                 
161500     MOVE 'W11127'       TO POSTSUM-FDNAMN                                
161600     MOVE 'W11120DB'     TO POSTSUM-DDNAMN2                               
161700     MOVE SPACE          TO POSTSUM-TRANSTYP                              
161800     CALL POSTSUM USING POSTSUM-PARM                                      
161900     .                                                                    
162000     EJECT                                                                
162100  S225-SKRIV-W11126 SECTION.                                              
162200                                                                          
162300     WRITE W11126-POST   FROM W11126-AREA                                 
162400     MOVE 'W11126'       TO POSTSUM-FDNAMN                                
162500     MOVE 'W11120D9'     TO POSTSUM-DDNAMN2                               
162600     MOVE SPACE          TO POSTSUM-TRANSTYP                              
162700     CALL POSTSUM USING POSTSUM-PARM                                      
162800     .                                                                    
162900     EJECT                                                                
163000**************************                                                
163100* IMS SECTIONER                                                           
163200     SKIP3                                                                
163300 IMS-GET-AA01 SECTION.                                                    
163400*                         WDK601                                          
163500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
163600     DELIMITED BY SIZE INTO SSA1                                          
163700     MOVE '  GE' TO GODK-STATUSKODER                                      
163800     CALL CBLTDLI USING GU ARTC1-PCB DLI-IO-AREA SSA1                     
163900     MOVE ARTC1-STATUS-CODE TO STATUS-WS                                  
164000     PERFORM IMS-STATUSKONTROLL                                           
164100     .                                                                    
164200     SKIP3                                                                
164300 IMS-GET-AA11 SECTION.                                                    
164400*                         WDK611                                          
164500     MOVE  'WLARTC11 '  TO  SSA1                                          
164600     MOVE '  ' TO GODK-STATUSKODER                                        
164700     CALL CBLTDLI USING GNP ARTC1-PCB DLI-IO-AREA SSA1                    
164800     MOVE ARTC1-STATUS-CODE  TO STATUS-WS                                 
164900     PERFORM IMS-STATUSKONTROLL                                           
165000     .                                                                    
165100     EJECT                                                                
165200 IMS-GET-INLB01 SECTION.                                                  
165300*                                                                         
165400     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
165500     DELIMITED BY SIZE INTO SSA1                                          
165600     MOVE '  GE' TO GODK-STATUSKODER                                      
165700     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1                      
165800     MOVE INLB-STATUS-CODE  TO STATUS-WS                                  
165900     PERFORM IMS-STATUSKONTROLL                                           
166000     .                                                                    
166100     SKIP3                                                                
166200 IMS-GNP-INLB11 SECTION.                                                  
166300*                                                                         
166400     MOVE  'WLINLB11 '  TO  SSA1                                          
166500     MOVE '  GE' TO GODK-STATUSKODER                                      
166600     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1                     
166700     MOVE INLB-STATUS-CODE  TO STATUS-WS                                  
166800     PERFORM IMS-STATUSKONTROLL                                           
166900     .                                                                    
167000     SKIP3                                                                
167100 IMS-GET-AC04-MED-AC2 SECTION.                                            
167200*                                                                         
167300     MOVE  'WLERSA13 ' TO SSA1                                            
167400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
167500     CALL CBLTDLI USING GN AC2-PCB DLI-IO-AREA SSA1                       
167600     MOVE AC2-STATUS-CODE  TO STATUS-WS                                   
167700     PERFORM IMS-STATUSKONTROLL                                           
167800     .                                                                    
167900     EJECT                                                                
168000 IMS-GET-ERSA01 SECTION.                                                  
168100*                                                                         
168200     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
168300     DELIMITED BY SIZE INTO SSA1                                          
168400     MOVE '  ' TO GODK-STATUSKODER                                        
168500     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA SSA1                      
168600     MOVE ERSA-STATUS-CODE  TO STATUS-WS                                  
168700     PERFORM IMS-STATUSKONTROLL                                           
168800     .                                                                    
168900     SKIP3                                                                
169000 IMS-GET-ERSA11 SECTION.                                                  
169100*                                                                         
169200     MOVE 'WLERSA11 ' TO SSA1                                             
169300     MOVE '  GE' TO GODK-STATUSKODER                                      
169400     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
169500     MOVE ERSA-STATUS-CODE  TO STATUS-WS                                  
169600     PERFORM IMS-STATUSKONTROLL                                           
169700     .                                                                    
169800     SKIP3                                                                
169900 IMS-GU-WDD701 SECTION.                                                   
170000     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
170100          DELIMITED BY SIZE INTO SSA1                                     
170200     MOVE '  GE'              TO GODK-STATUSKODER                         
170300     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD701 SSA1                    
170400     MOVE WDD7-STATUS-CODE    TO STATUS-WS                                
170500     PERFORM IMS-STATUSKONTROLL                                           
170600     .                                                                    
170700     SKIP3                                                                
170800 IMS-GNP-WDD713 SECTION.                                                  
170900     STRING 'WDD713  (WDD713KY>=' W-WDD713KY-X ')'                        
171000          DELIMITED BY SIZE INTO SSA1                                     
171100     MOVE '  GE'              TO GODK-STATUSKODER                         
171200     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD713 SSA1                   
171300     MOVE WDD7-STATUS-CODE    TO STATUS-WS                                
171400     PERFORM IMS-STATUSKONTROLL                                           
171500     .                                                                    
171600     SKIP3                                                                
171700 IMS-GET-ARTM01 SECTION.                                                  
171800*                                                                         
171900     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
172000     DELIMITED BY SIZE INTO SSA1                                          
172100     MOVE '  GE'                 TO GODK-STATUSKODER                      
172200     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA2 SSA1                     
172300     MOVE ARTM-STATUS-CODE       TO STATUS-WS                             
172400     PERFORM IMS-STATUSKONTROLL                                           
172500     .                                                                    
172600     EJECT                                                                
172700 IMS-GET-ARTC01 SECTION.                                                  
172800*                                                                         
172900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-T-X ')'                       
173000     DELIMITED BY SIZE INTO SSA1                                          
173100     MOVE '  GE' TO GODK-STATUSKODER                                      
173200     CALL CBLTDLI USING GU ARTC2-PCB DLI-IO-AREA SSA1                     
173300     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
173400     PERFORM IMS-STATUSKONTROLL                                           
173500     .                                                                    
173600     SKIP3                                                                
173700 IMS-GET-ARTC11 SECTION.                                                  
173800*                                                                         
173900     MOVE 'WLARTC11 ' TO SSA1                                             
174000     MOVE '  GE' TO GODK-STATUSKODER                                      
174100     CALL CBLTDLI USING GNP ARTC2-PCB DLI-IO-AREA SSA1                    
174200     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
174300     PERFORM IMS-STATUSKONTROLL                                           
174400     .                                                                    
174500     SKIP3                                                                
174600 IMS-GU-ARTC11 SECTION.                                                   
174700*                                                                         
174800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-T-X ')'                       
174900     DELIMITED BY SIZE INTO SSA1                                          
175000     MOVE 'WLARTC11 ' TO SSA2                                             
175100     MOVE '  GE' TO GODK-STATUSKODER                                      
175200     CALL CBLTDLI USING GU ARTC2-PCB DLI-IO-AREA SSA1 SSA2                
175300     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
175400     PERFORM IMS-STATUSKONTROLL                                           
175500     .                                                                    
175600     EJECT                                                                
175700 IMS-GET-ARTC23 SECTION.                                                  
175800*                                                                         
175900     MOVE 'WLARTC11 ' TO SSA1                                             
176000     MOVE 'WLARTC23 ' TO SSA2                                             
176100     MOVE '  GE' TO GODK-STATUSKODER                                      
176200     CALL CBLTDLI USING GNP ARTC2-PCB DLI-IO-AREA SSA1 SSA2               
176300     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
176400     PERFORM IMS-STATUSKONTROLL                                           
176500     .                                                                    
176600     SKIP3                                                                
176700 IMS-GET-ARTG01 SECTION.                                                  
176800*                                                                         
176900     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
177000     DELIMITED BY SIZE INTO SSA1                                          
177100     MOVE '  GE' TO GODK-STATUSKODER                                      
177200     CALL CBLTDLI USING GU ARTG-PCB DLI-IO-AREA4 SSA1                     
177300     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
177400     PERFORM IMS-STATUSKONTROLL                                           
177500     .                                                                    
177600     SKIP3                                                                
177700 IMS-GET-INLE01 SECTION.                                                  
177800*                                                                         
177900     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
178000             DELIMITED BY SIZE INTO SSA1                                  
178100     MOVE '  GE' TO GODK-STATUSKODER                                      
178200     CALL CBLTDLI USING GU INLE-PCB DLI-IO-AREA3 SSA1                     
178300     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
178400     PERFORM IMS-STATUSKONTROLL                                           
178500     .                                                                    
178600     EJECT                                                                
178700 IMS-GET-INLE21 SECTION.                                                  
178800*                                                                         
178900     MOVE 'WLINLE11 ' TO SSA1                                             
179000     STRING 'WLINLE21(IDPTYP   =' W-IDPTYP-X ')'                          
179100             DELIMITED BY SIZE INTO SSA2                                  
179200     MOVE '  GE' TO GODK-STATUSKODER                                      
179300     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-AREA3 SSA1 SSA2               
179400     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
179500     PERFORM IMS-STATUSKONTROLL                                           
179600     .                                                                    
179700     SKIP3                                                                
179800 IMS-GU-WDH101 SECTION.                                                   
179900                                                                          
180000     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
180100            DELIMITED BY SIZE INTO SSA1                                   
180200     MOVE '  GE' TO GODK-STATUSKODER                                      
180300     CALL CBLTDLI USING GU WDH1-PCB DLI-IO-AREA5 SSA1                     
180400     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
180500     PERFORM IMS-STATUSKONTROLL                                           
180600     .                                                                    
180700     EJECT                                                                
180800 IMS-GNP-WDH111 SECTION.                                                  
180900                                                                          
181000     STRING 'WDH111  (WDH111KY>=' W-WDH1KEY-MIN-X                         
181100                    '&WDH111KY<=' W-WDH1KEY-MAX-X ')'                     
181200            DELIMITED BY SIZE INTO SSA1                                   
181300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
181400     CALL CBLTDLI USING GNP WDH1-PCB DLI-IO-AREA6 SSA1                    
181500     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
181600     PERFORM IMS-STATUSKONTROLL                                           
181700     .                                                                    
181800     EJECT                                                                
181900 IMS-STATUSKONTROLL SECTION.                                              
182000     SET STATUS-IX TO 1                                                   
182100     SEARCH GODK-STATUS AT END CALL FELLOG                                
182200     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
182300     CONTINUE                                                             
182400     END-SEARCH                                                           
182500     .                                                                    
182600     EJECT                                                                
182700*    -COPY WY2000P2                                                       
