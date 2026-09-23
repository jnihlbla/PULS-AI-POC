000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1111800.                                                
000400 AUTHOR.         FRONTEC, GÖTEBORG.                                       
000500 DATE-WRITTEN.   96/08/12.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        BEHANDLAR INVENTERADE ARTIKLAR.                                  
001100*        PROGRAMMET BESTÅR AV EN SEKTION SOM HAR BRYTITS UT UR            
001200*        W11120 PGA OMSKRIVNING TILL BMP.                                 
001300*        UTFILER SKAPAS MED DE UPPDATERINGAR SOM SKA GÖRAS PÅ             
001400*        WDD7, WDK6 OCH WDG3                                              
001500*                                                                         
001600*        PROGRAMMET LÄSER  WLARTG (WDD2)                                  
001700*                          WLERSA (WDD7)                                  
001800*                          WLARTC (WDK6)                                  
001900*                          WLARTM (WDK9)                                  
001910*                          WDH1                                           
002000*                                                                         
002100*    SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP1                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500     SKIP1                                                                
002600 FILE-CONTROL.                                                            
002700     SKIP1                                                                
002800     SELECT W51385   ASSIGN W11118D1.                                     
002810     SELECT W11128   ASSIGN W11118D9.                                     
002900                                                                          
003000     SELECT W11114   ASSIGN W11118D2.                                     
003100     SELECT W11115   ASSIGN W11118D3.                                     
003200     SELECT W11116   ASSIGN W11118D4.                                     
003300     SELECT W11118   ASSIGN W11118D6.                                     
003400     SELECT W11119   ASSIGN W11118D7.                                     
003500     SELECT W11121   ASSIGN W11118D8.                                     
003510     SELECT W11128A  ASSIGN W11118DA.                                     
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP1                                                                
003900 FILE SECTION.                                                            
004000     SKIP2                                                                
004100 FD  W51385                                                               
004200     RECORDING F                                                          
004300     BLOCK CONTAINS 0.                                                    
004400                                                                          
004500*01  W51385-POST   -COPY W51385      -L.                                  
004600     SKIP2                                                                
004700     EJECT                                                                
004800 FD  W11114                                                               
004900     RECORDING F                                                          
005000     BLOCK CONTAINS 0.                                                    
005100                                                                          
005200*01  W11114-POST   -COPY W21801 -L.                                       
005300     EJECT                                                                
005400 FD  W11121                                                               
005500     RECORDING F                                                          
005600     BLOCK CONTAINS 0.                                                    
005700                                                                          
005800*01  W11121-POST   -COPY W11121 -L.                                       
005900     EJECT                                                                
006000 FD  W11115                                                               
006100     RECORDING F                                                          
006200     BLOCK CONTAINS 0.                                                    
006300                                                                          
006400*01  PPMS-POST   -COPY W111PPMS -L.                                       
006500     EJECT                                                                
006600 FD  W11116                                                               
006700     RECORDING F                                                          
006800     BLOCK CONTAINS 0.                                                    
006900                                                                          
007000*01  W11116-POST   -COPY W11123 -L.                                       
007100     EJECT                                                                
007200 FD  W11118                                                               
007300     RECORDING F                                                          
007400     BLOCK CONTAINS 0.                                                    
007500                                                                          
007600*01  W111240-POST   -COPY W111240     -L.                                 
007700*01  W111241-POST   -COPY W111241     -L.                                 
007800*01  W111242-POST   -COPY W111242     -L.                                 
007900*01  W111243-POST   -COPY W111243     -L.                                 
008000     EJECT                                                                
008100 FD  W11119                                                               
008200     RECORDING F                                                          
008300     BLOCK CONTAINS 0.                                                    
008400                                                                          
008500*01  W11119-POST   -COPY W111251 -L.                                      
008600*01  W111191-POST   -COPY W111253 -L.                                     
008700     EJECT                                                                
008710 FD  W11128                                                               
008720     RECORDING F                                                          
008730     BLOCK CONTAINS 0.                                                    
008740                                                                          
008750*01  -COPY W11128      -L.                                                
008760     EJECT                                                                
008770 FD  W11128A                                                              
008780     RECORDING F                                                          
008790     BLOCK CONTAINS 0.                                                    
008791                                                                          
008792*01  W11128A-POST   -COPY W11128      -L.                                 
008793     EJECT                                                                
008800 WORKING-STORAGE SECTION.                                                 
008900     SKIP3                                                                
009000*    -- CHECKED BY WY2000                                                 
009100     SKIP3                                                                
009200 77  IDPGM                  PIC X(8)       VALUE 'W1111800'.              
009300 77  JA                     PIC X                    VALUE 'J'.           
009400 77  NEJ                    PIC X                    VALUE 'N'.           
009410 77  WS-CNT                 PIC 9                    VALUE ZERO.          
009411 77  WS-FND                 PIC X                    VALUE 'N'.           
009420 77  WS-ACTV-INVTRY         PIC X                    VALUE 'N'.           
009500 77  FL-POST-TILL-W11122    PIC X                    VALUE 'N'.           
009600 77  WS-KVAKS               PIC S9(7)        COMP-3 VALUE ZERO.           
009700 77  WS-RETUR               PIC S9(7)        COMP-3 VALUE ZERO.           
009710 77  INPUT-EOF-SW           PIC X                    VALUE 'N'.           
009720     88 INPUT-EOF                                    VALUE 'J'.           
009800 77  PROGRAM-NAMN           PIC X(8)       VALUE 'W1111800'.              
009900     EJECT                                                                
010000 01  SWITCHAR.                                                            
010100                                                                          
010200     03  PPMS-TRANS               PIC X.                                  
010300     03  KDERS-SW.                                                        
010400         05  ANDRAD-KDERS-SW      PIC X   OCCURS 2.                       
010500     03  FILLER          REDEFINES KDERS-SW.                              
010600             88  NAGOT-KDERS-ANDRAT         VALUE 'JJ' THRU 'NJ'.         
010700             88  INGET-KDERS-ANDRAT         VALUE 'NN'.                   
010800         05  C1-KDERS             PIC X.                                  
010900             88  ANDRAD-KDERS-C1                     VALUE 'J'.           
011000         05  C2-KDERS             PIC X.                                  
011100             88  ANDRAD-KDERS-C2                     VALUE 'J'.           
011200                                                                          
011300     03  WS-KDERS                 PIC 9(2)  VALUE ZERO.                   
011400     03  FILLER REDEFINES WS-KDERS.                                       
011500         05  WS-KDERS-1           PIC 9.                                  
011600         05  WS-KDERS-2           PIC 9.                                  
011700                                                                          
011800     03  KDERS-BLIVIT-PREL-SW     PIC X              VALUE 'N'.           
011900         88  KDERS-BLIVIT-PREL                       VALUE 'J'.           
012000                                                                          
012100     03  AC04-UPPDATERAD-SW       PIC X              VALUE 'N'.           
012200         88  AC04-UPPDATERAD                         VALUE 'J'.           
012300                                                                          
012400     03  AA01-UPPDATERAD-SW       PIC X              VALUE 'N'.           
012500         88  AA01-UPPDATERAD                         VALUE 'J'.           
012600                                                                          
012700     03  AA11-UPPDATERAD-SW       PIC X              VALUE 'N'.           
012800         88  AA11-UPPDATERAD                         VALUE 'J'.           
012900                                                                          
013000     03  ERS01-DELETE-SW          PIC X              VALUE 'N'.           
013100         88  ERS01-DELETE                            VALUE 'J'.           
013200                                                                          
013300     03  EOF-W51385-SW            PIC X              VALUE 'N'.           
013400         88  EOF-W51385                              VALUE 'J'.           
013500                                                                          
013600     EJECT                                                                
013700 01  ARBETS-FALT.                                                         
013800     03  KDSTATUS           PIC S9(3)   COMP-3     OCCURS 2.              
013900     03  TIERSDAT           PIC S9(5)   COMP-3     OCCURS 2.              
014000     03  KDERS-OLD          PIC S9(3)   COMP-3.                           
014100                                                                          
014200     03  W009VADD-DATUM     PIC S9(5)   COMP-3.                           
014300     03  W009VADD-ANTAL     PIC S9(3)   COMP-3.                           
014400                                                                          
014500     03  WS-KVOKS           PIC S9(7)   COMP-3 VALUE ZERO.                
014600     03  W-KDANSKQ          PIC X       VALUE SPACE.                      
014700                                                                          
014703*                                                                         
014704*01    -COPY WWDCLAND                                                     
014705                                                                          
014710 01  WS-TABELL-IDDC                     VALUE SPACE.                      
014720     03  FILLER                         OCCURS 15.                        
014730         05  WS-TAB-IDDC     PIC X(2).                                    
014731         05  WS-TAB-IDLANDX2 PIC X(2).                                    
014740                                                                          
014750 01  WS-IX                  PIC S9(3)   VALUE +1    COMP-3.               
014760 01  WS-IX-MAX              PIC S9(3)   VALUE +15   COMP-3.               
014761 01  WS-DCLAND-IX           PIC S9(3)   VALUE +1    COMP-3.               
014762 01  WS-WDH1-PARTS.                                                       
014763     03 WDH1-PARTS-TAB OCCURS 500 TIMES INDEXED BY INDX.                  
014764        05 WS-WDH1-IDARTNR  PIC S9(9) COMP-3 VALUE 0.                     
014770                                                                          
014800*      --- VALID IDDC CODES                                               
014900*                                                                         
015000*01    -COPY WWDC99                                                       
015010*01    -COPY WWDCKONS                                                     
015100       EJECT                                                              
015200 01  W-SPAR-FALT.                                                         
015300     03  W-SPAR-W11119.                                                   
015400         05  W-SP-IDARTNR   PIC S9(9)   COMP-3.                           
015500         05  W-SP-FLTPO1    PIC X.                                        
015600         05  W-SP-KDERS     PIC S9(3)   COMP-3.                           
015700         05  W-SP-KDKSP     PIC S9(1)   COMP-3.                           
015800         05  W-SP-KDAVT     PIC S9(1)   COMP-3.                           
015900                                                                          
016000 01  AAVVD                  PIC 9(5).                                     
016100 01  FILLER REDEFINES AAVVD.                                              
016200     03  AAVV               PIC 9(4).                                     
016300     03  FILLER REDEFINES AAVV.                                           
016400         05  AA             PIC 9(2).                                     
016500         05  VV             PIC 9(2).                                     
016600     03  D                  PIC 9(1).                                     
016700                                                                          
016800 01  WS-PPMS-IDARTNR        PIC 9(9)  VALUE ZERO.                         
016900 01  WS-PPMS-IDARTNR-TILLK  PIC X(9)  VALUE ZERO.                         
017000 01  WS-ALT-ERS             PIC 9(2).                                     
017100     88 ALT-ERS             VALUE 04 14 24 05 25 06 26                    
017200                                  08 18 28.                               
017300 01  NOLL-RAKNARE           PIC S9(5) COMP-3  VALUE ZERO.                 
017400                                                                          
017500 01  W-IDAVTAL-RED          PIC 9(13).                                    
017600 01  W-IDAVTAL REDEFINES W-IDAVTAL-RED.                                   
017700     03  FILLER             PIC X.                                        
017800     03  W-PREFIX           PIC X(3).                                     
017900     03  W-AVTALNR          PIC X(6).                                     
018000     03  W-SUFFIX           PIC X(3).                                     
018100                                                                          
018200 01  W-IDARTNR-8            PIC 9(8) VALUE ZERO.                          
018300                                                                          
018400 01  SUBPROGRAM.                                                          
018500     03    CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                  
018600     03    FELLOG          PIC X(8)    VALUE 'FELLOG  '.                  
018700     03    DATKORT         PIC X(8)    VALUE 'DATKORT '.                  
018800     03    POSTSUM         PIC X(8)    VALUE 'POSTSUM '.                  
018900     03    W009VADD        PIC X(8)    VALUE 'W009VADD'.                  
019000     SKIP3                                                                
019100 01  NYCKLAR.                                                             
019200     03  W-IDARTNR-X.                                                     
019300       05  W-IDARTNR     PIC S9(9)  COMP-3.                               
019400     03  W-IDARTNR-T-X.                                                   
019500       05  W-IDARTNR-T   PIC S9(9)  COMP-3.                               
019600     03  W-LOW-26        PIC X(26)  VALUE LOW-VALUE.                      
019700     03  W-1133-KEY-X.                                                    
019800       05  W-WDGXKEY     PIC X(4)   VALUE '1133'.                         
019900       05  FILLER        PIC X(26)  VALUE LOW-VALUE.                      
020000     03  W-IDPTYP-X.                                                      
020100       05  W-IDPTYP      PIC X(3)   VALUE SPACE.                          
020110     03  W-WDH1KEY-MIN-X.                                                 
020120       05  W-IDDC-WDH1-MIN     PIC X(2)  VALUE SPACES.                    
020130       05  W-KDINVKAT-MIN      PIC S9(3) VALUE ZERO COMP-3.               
020140       05  W-TISEGKEY-MIN      PIC S9(9) VALUE ZERO COMP-3.               
020150       05  W-DAREGDAT-SORT-MIN PIC 9(8)  VALUE ZERO.                      
020160                                                                          
020170     03  W-WDH1KEY-MAX-X.                                                 
020180       05  W-IDDC-WDH1-MAX     PIC X(2)  VALUE SPACES.                    
020190       05  W-KDINVKAT-MAX      PIC S9(3) VALUE +999 COMP-3.               
020191       05  W-TISEGKEY-MAX      PIC S9(9) VALUE +999999999 COMP-3.         
020192       05  W-DAREGDAT-SORT-MAX PIC 9(8)  VALUE 99999999.                  
020200                                                                          
020300                                                                          
020400     EJECT                                                                
020500 01  PARAM-TILL-DATUMKORT.                                                
020600     03  PROG-ID         PIC X(8)      VALUE 'W1111800'.                  
020700     03  KORT-ID         PIC X(6)      VALUE 'WDATUM'.                    
020800     SKIP3                                                                
020900*    03  -COPY WDATKORT                                                   
021000     SKIP2                                                                
021100 01  DAGENS-DAGNR        PIC 9(5).                                        
021200 01  FILLER REDEFINES DAGENS-DAGNR.                                       
021300     03 DAGENS-AA        PIC 99.                                          
021400     03 DAGENS-VV        PIC 99.                                          
021500     03 DAGENS-D         PIC 9.                                           
021600                                                                          
021700 01  WS-DAGENS-DATUM     PIC 9(6).                                        
021800 01  FILLER REDEFINES WS-DAGENS-DATUM.                                    
021900     03  WS-DAGENS-AA    PIC 99.                                          
022000     03  WS-DAGENS-MM    PIC 99.                                          
022100     03  WS-DAGENS-DD    PIC 99.                                          
022200                                                                          
022300     EJECT                                                                
022400*   ----- PARAMETRAR TILL POSTSUM                                         
022500*01  -COPY W0005       -PRE POSTSUM-.                                     
022600     EJECT                                                                
022700 01  IN-AREA-START               PIC X(24)   VALUE                        
022800                                             'IN-AREA-START'.             
022900     SKIP2                                                                
023000*01  AREA      -COPY W51385      -PRE INVIN-                              
023100     EJECT                                                                
023200 01  UT-AREA-START               PIC X(24)   VALUE                        
023300                                             'UT-AREA-START'.             
023400     SKIP2                                                                
023500                                                                          
023600*01  -COPY W440004     -PRE UT-.                                          
023700     EJECT                                                                
023800*01  -COPY W111PPMS    -PRE UT-PPMS-.                                     
023900     EJECT                                                                
024000*01  AREA      -COPY W21801      -PRE W11114-                             
024100     EJECT                                                                
024200*01  AREA      -COPY W11123      -PRE W11116-.                            
024300     EJECT                                                                
024400*01  AREA      -COPY W11121      -PRE INVUT-.                             
024500     EJECT                                                                
024510*01  AREA      -COPY W11128      -PRE W11128-.                            
024520     EJECT                                                                
024600 01  W11118-AREA.                                                         
024700     03  W11118-IDPTYP   PIC X(3).                                        
024800     03  FILLER          PIC X(17).                                       
024900                                                                          
025000 01  W111240-AREA     REDEFINES W11118-AREA.                              
025100*    03  -COPY W111240                                                    
025200                                                                          
025300 01  W111241-AREA     REDEFINES W11118-AREA.                              
025400*    03  -COPY W111241                                                    
025500                                                                          
025600 01  W111242-AREA     REDEFINES W11118-AREA.                              
025700*    03  -COPY W111242                                                    
025800                                                                          
025900 01  W111243-AREA     REDEFINES W11118-AREA.                              
026000*    03  -COPY W111243                                                    
026100     EJECT                                                                
026200 01  W11119-AREA.                                                         
026300     03  FILLER          PIC X(20).                                       
026400                                                                          
026500 01  W111190-AREA     REDEFINES W11119-AREA.                              
026600*    03  -COPY W111250  -PRE W111190-                                     
026700                                                                          
026800 01  W111191-AREA     REDEFINES W11119-AREA.                              
026900*    03  -COPY W111251  -PRE W111191-                                     
027000                                                                          
027100 01  W111192-AREA     REDEFINES W11119-AREA.                              
027200*    03  -COPY W111252  -PRE W111192-                                     
027300                                                                          
027400 01  W111193-AREA     REDEFINES W11119-AREA.                              
027500*    03  -COPY W111253  -PRE W111193-                                     
027600     EJECT                                                                
027700 01  IMS-AREA-START              PIC X(24)   VALUE                        
027800                                             'IMS-AREA-START'.            
027900     SKIP2                                                                
028000*01  AREA      -COPY WDK601      -PRE AA01-.                              
028100     EJECT                                                                
028200*01  AREA      -COPY WDK611      -PRE AA11-.                              
028300     EJECT                                                                
028400*01  AREA      -COPY WDD704      -PRE AC04-.                              
028500     EJECT                                                                
028600*01  AREA      -COPY WDD701      -PRE ERSA01-                             
028700     EJECT                                                                
028800*01  AREA      -COPY WDD702      -PRE ERSA11-                             
028900     EJECT                                                                
029000*01  AREA      -COPY WDK611      -PRE ARTC11-                             
029100     EJECT                                                                
029200                                                                          
029300*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
029400*                                                                         
029500 01  IMS-WS.                                                              
029600     03     FILLER         PIC X(8)    VALUE 'IMS-WS  '.                  
029700*                                                                         
029800*                            *** STATUSKOD FRÅN IMS                       
029900     03  STATUS-WS         PIC XX.                                        
030000         88  SEGMENT-FINNS             VALUE '  '.                        
030100         88  SEGMENT-SAKNAS            VALUE 'GE'.                        
030110         88  BASEN-SLUT                VALUE 'GB'.                        
030200*                                                                         
030300*                            *** SEGMENTNIVÅ FRÅN IMS                     
030400     03  LEVEL-WS          PIC XX.                                        
030500         88  ROTEN-SAKNAS              VALUE '00'.                        
030600     SKIP3                                                                
030700     03    SSA1            PIC X(121).                                    
030800     03    SSA2            PIC X(121).                                    
030900     03    SSA3            PIC X(50).                                     
031000     SKIP3                                                                
031100     03    GODK-STATUSKODER.                                              
031200         05    GODK-STATUS OCCURS 3  INDEXED BY STATUS-IX PIC XX.         
031300     SKIP3                                                                
031400*01      -COPY W0003                                                      
031500     EJECT                                                                
031600 01      DLI-IO-AREA     PIC X(700)  VALUE SPACE.                         
031700     SKIP3                                                                
031800 01      DLI-IO-AREA2.                                                    
031900*   03    AREA   -COPY WDK901      -PRE ARTM-                             
032000     EJECT                                                                
032100 01      DLI-IO-WDD2.                                                     
032200*   03    AREA   -COPY WDD201      -PRE NYP-                              
032300     EJECT                                                                
032400 01      DLI-IO-WDR5.                                                     
032500*   03    AREA   -COPY WDGX1134    -PRE 1134-                             
032600     EJECT                                                                
032700 01      DLI-IO-WDL2.                                                     
032800*   03    AREA   -COPY WDL221      -PRE MOT-                              
032900     EJECT                                                                
032910 01      DLI-IO-WDH101.                                                   
032920*   03   -COPY WDH101                                                     
032930     EJECT                                                                
032940 01      DLI-IO-WDH111.                                                   
032950*   03   -COPY WDH111                                                     
032960     EJECT                                                                
033000 LINKAGE SECTION.                                                         
033100     SKIP3                                                                
033200*01      -COPY W0008     -PRE ARTC1-.                                     
033300     05  FILLER                  PIC X.                                   
033400     EJECT                                                                
033500*01      -COPY W0008     -PRE AC1-.                                       
033600     05  FILLER                  PIC X.                                   
033700     EJECT                                                                
033800*01      -COPY W0008     -PRE ERSA-.                                      
033900     05  FILLER                  PIC X.                                   
034000     EJECT                                                                
034100*01      -COPY W0008     -PRE ARTM-.                                      
034200     05  FILLER                  PIC X.                                   
034300     EJECT                                                                
034400*01      -COPY W0008     -PRE ARTC2-.                                     
034500     05  FILLER                  PIC X.                                   
034600     EJECT                                                                
034700*01      -COPY W0008     -PRE ARTG-.                                      
034800     05  FILLER                  PIC X.                                   
034900     EJECT                                                                
035000*01      -COPY W0008     -PRE WDR5-.                                      
035100     05  FILLER                  PIC X.                                   
035200     EJECT                                                                
035300*01      -COPY W0008     -PRE WDL2-.                                      
035400     05  FILLER                  PIC X.                                   
035500     EJECT                                                                
035510*01      -COPY W0008     -PRE WDH1-.                                      
035520     05  FILLER                  PIC X.                                   
035530     EJECT                                                                
035600 PROCEDURE DIVISION USING  ARTC1-PCB AC1-PCB                              
035700                           ERSA-PCB ARTM-PCB                              
035800                           ARTC2-PCB ARTG-PCB                             
035900                           WDR5-PCB WDL2-PCB WDH1-PCB.                    
036000                                                                          
036100     ENTRY 'DLITCBL' USING ARTC1-PCB AC1-PCB                              
036200                           ERSA-PCB ARTM-PCB ARTC2-PCB                    
036300                           ARTC2-PCB ARTG-PCB WDR5-PCB                    
036400                           WDL2-PCB WDH1-PCB.                             
036500                                                                          
036600     PERFORM A-INIT                                                       
036700     PERFORM B-KONTR-INVENTERADE-ARTIKLAR                                 
036800                                                                          
036900     CLOSE W51385                                                         
037000           W11114                                                         
037100           W11115                                                         
037200           W11116                                                         
037300           W11118                                                         
037400           W11119                                                         
037500           W11121                                                         
037510           W11128                                                         
037520           W11128A                                                        
037600                                                                          
037700     MOVE 'S' TO POSTSUM-OPKOD                                            
037800     CALL POSTSUM USING POSTSUM-PARM                                      
037900     MOVE ZERO TO RETURN-CODE                                             
038000     GOBACK                                                               
038100     .                                                                    
038200     EJECT                                                                
038300 A-INIT SECTION.                                                          
038400                                                                          
038500     OPEN INPUT  W51385                                                   
038510                 W11128                                                   
038600     OPEN OUTPUT W11114                                                   
038700                 W11115                                                   
038800                 W11116                                                   
038900                 W11118                                                   
039000                 W11119                                                   
039100                 W11121                                                   
039110                 W11128A                                                  
039200                                                                          
039300     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
039400                                                                          
039500     CALL DATKORT USING PROG-ID KORT-ID DATUMKORT                         
039600                                                                          
039700     MOVE D-AAR      TO DAGENS-AA                                         
039800                        WS-DAGENS-AA                                      
039900     MOVE D-VECKA    TO DAGENS-VV                                         
040000     MOVE D-DAGNR    TO DAGENS-D                                          
040100     MOVE D-MAANAD   TO WS-DAGENS-MM                                      
040200     MOVE D-DAG      TO WS-DAGENS-DD                                      
040210                                                                          
040220     PERFORM AA-SKAPA-TAB-IDDC                                            
040230                                                                          
040300     .                                                                    
040400     EJECT                                                                
040500 AA-SKAPA-TAB-IDDC SECTION.                                               
040600                                                                          
040610     MOVE +1          TO WS-IX                                            
040611     MOVE +1          TO WS-DCLAND-IX                                     
040612                                                                          
040613     PERFORM UNTIL WS-DCLAND-IX > DCLAND-IX-MAX                           
040614       IF (DCLAND-USA (WS-DCLAND-IX) OR                                   
040615          DCLAND-CANADA (WS-DCLAND-IX))                                   
040617          IF WS-IX > WS-IX-MAX                                            
040618            DISPLAY 'WS-TABELL-IDDC BEHÖVER UTÖKAS'                       
040619            CALL FELLOG                                                   
040620          ELSE                                                            
040621            MOVE DCLAND-IDDC (WS-DCLAND-IX) TO WS-TAB-IDDC (WS-IX)        
040622          END-IF                                                          
040623          ADD +1          TO WS-IX                                        
040624       END-IF                                                             
040625       ADD +1             TO WS-DCLAND-IX                                 
040627     END-PERFORM                                                          
040628                                                                          
040629                                                                          
040630     .                                                                    
040631     EJECT                                                                
040640 B-KONTR-INVENTERADE-ARTIKLAR SECTION.                                    
040650                                                                          
040700     PERFORM S1-LAES-INVENT-W51385                                        
040710     PERFORM S11-LOAD-WDH1-TABLE                                          
040800     PERFORM IMS-GET-WDR501                                               
040900     PERFORM UNTIL EOF-W51385                                             
041000       MOVE ALL 'N' TO SWITCHAR                                           
041010       MOVE 'N'     TO WS-ACTV-INVTRY                                     
041011                       WS-FND                                             
041020       MOVE ZERO    TO WS-CNT                                             
041100       MOVE INVIN-IDARTNR   TO W-IDARTNR                                  
041200       MOVE INVIN-IDDC      TO WS-IDDC                                    
041300       IF CDC-SE                                                          
041400          PERFORM IMS-GET-UNIK-AC04-MED-AC1                               
041500          IF SEGMENT-FINNS                                                
041600            MOVE DLI-IO-AREA           TO AC04-AREA                       
041700            MOVE AC04-KDSTATUS-C1      TO KDSTATUS (1)                    
041800            MOVE AC04-KDSTATUS-C2      TO KDSTATUS (2)                    
041900            MOVE AC04-TIERSDAT-PREL-C1 TO TIERSDAT (1)                    
042000            MOVE AC04-TIERSDAT-PREL-C2 TO TIERSDAT (2)                    
042100                                                                          
042200            IF KDSTATUS (1) = 4                                           
042300              PERFORM BA-LAS-ARTREG                                       
042400              IF AA11-CLAG-KDERS NUMERIC                                  
042500                 MOVE ZERO TO WS-KVOKS                                    
042600                 PERFORM IMS-GET-ARTM01                                   
042700                 IF SEGMENT-FINNS                                         
042800                   COMPUTE WS-KVOKS = ARTM-ART-KVOKS-BULK +               
042900                   ARTM-ART-KVOKS-DAG + ARTM-ART-KVOKS-VOR                
043000                 END-IF                                                   
043100                                                                          
043200                 PERFORM IMS-GET-WDGX1134                                 
043300                 IF SEGMENT-SAKNAS                                        
043400                  COMPUTE WS-KVAKS = AA11-CLAG-KVAKS-CDC                  
043500                  IF WS-KVAKS > 0                                         
043600                    PERFORM IMS-GET-WDL201                                
043700                    IF SEGMENT-FINNS                                      
043800                       MOVE '310' TO W-IDPTYP                             
043900                       PERFORM IMS-GET-WDL221                             
044000                       PERFORM UNTIL SEGMENT-SAKNAS                       
044100                         IF MOT-MOT-KDRT = 07                             
044200                            COMPUTE WS-RETUR =                            
044300                            MOT-MOT-KVAVIS - MOT-MOT-KVANTMOT             
044400                            SUBTRACT WS-RETUR FROM WS-KVAKS               
044500                         END-IF                                           
044600                         PERFORM IMS-GET-WDL221                           
044700                       END-PERFORM                                        
044800                    END-IF                                                
044900                  END-IF                                                  
045000                  IF (WS-KVAKS +                                          
045100                     AA11-CLAG-KVLS  - AA11-CLAG-KVRESS  -                
045200                     WS-KVOKS) > 0                                        
045500                     PERFORM BB-BACKA-TILL-STATUS-3                       
045600                   ELSE                                                   
045700                     PERFORM BC-FORTSATT-TILL-STATUS-0                    
045800                   END-IF                                                 
045900                 ELSE                                                     
046000                   IF (AA11-CLAG-KVLS   -                                 
046100                     AA11-CLAG-KVRESS    -                                
046200                     WS-KVOKS > 0)                                        
046500                     PERFORM BB-BACKA-TILL-STATUS-3                       
046600                   ELSE                                                   
046700                     PERFORM BC-FORTSATT-TILL-STATUS-0                    
046800                   END-IF                                                 
046900                   PERFORM S2-TRANS-AK-BEVAKN                             
047000                 END-IF                                                   
047100                                                                          
047200                 PERFORM S216-SKRIV-W111192                               
047300                                                                          
047400                 IF NAGOT-KDERS-ANDRAT                                    
047500                   PERFORM S3-TRANS-TILL-SATSSYST                         
047600                   PERFORM S6-TRANS-TILL-VR-SYST                          
047700                   PERFORM S8-TRANS-TILL-PPMS                             
047800                   PERFORM S9-TRANS-TILL-BASLAGER                         
047900                   PERFORM S10-BEHANDLA-FLTPO1                            
048000                   PERFORM S210-POST-TILL-W11122-WDR5                     
048100                 END-IF                                                   
048200                 IF AA01-UPPDATERAD OR NAGOT-KDERS-ANDRAT                 
048300                   PERFORM BE-BEHANDLA-KDKSP                              
048400                   PERFORM BD-UPPDATERA-ARTREG                            
048500                 END-IF                                                   
048600              END-IF                                                      
048700            ELSE                                                          
048800              DISPLAY 'STATUS NOT 4  FÖR ARTIKEL ' W-IDARTNR              
048900              ' STATUS-C1 ' KDSTATUS (1)                                  
049000            END-IF                                                        
049100          ELSE                                                            
049200            DISPLAY '04-SEGMENT SAKNAS PÅ WDD7 FÖR ARTIKEL '              
049300            W-IDARTNR                                                     
049400          END-IF                                                          
049500       ELSE                                                               
049600          DISPLAY 'IDDC = ' WS-IDDC                                       
049700       END-IF                                                             
049800       PERFORM S1-LAES-INVENT-W51385                                      
049900     END-PERFORM                                                          
050000     .                                                                    
050100     EJECT                                                                
050200 BA-LAS-ARTREG SECTION.                                                   
050300                                                                          
050400     PERFORM IMS-GET-AA01                                                 
050500     IF SEGMENT-SAKNAS                                                    
050600       DISPLAY 'BEGÄRD ARTIKEL SAKNAS PÅ ARTREG ' W-IDARTNR               
050700       CALL FELLOG                                                        
050800     ELSE                                                                 
050900       MOVE DLI-IO-AREA      TO AA01-AREA                                 
051000       MOVE AA01-ART-IDARTNR TO W-SP-IDARTNR                              
051100       PERFORM IMS-GET-AA11                                               
051200       MOVE DLI-IO-AREA      TO AA11-AREA                                 
051300       MOVE AA11-CLAG-FLTPO1 TO W-SP-FLTPO1                               
051400       MOVE AA11-CLAG-KDERS  TO KDERS-OLD                                 
051500                                W-SP-KDERS                                
051600       MOVE AA11-CLAG-KDKSP  TO W-SP-KDKSP                                
051700       MOVE AA11-CLAG-KDAVT  TO W-SP-KDAVT                                
051800     END-IF                                                               
051900     .                                                                    
052000     EJECT                                                                
052100 BB-BACKA-TILL-STATUS-3 SECTION.                                          
052200                                                                          
052210*****************                                                         
052220* SEARCH THE FILE OF ACTIVE INV. PARTS TO CHECK IF THERE WAS AN           
052230* ACTIVE INVENTORY FOR THE PART. IF NOT FOUND, CHECK WDH1                 
052240* TO SEE IF THERE IS ANY INVENTORY IN PROGRESS NOW.                       
052250     SET INDX TO +1                                                       
052260     SEARCH WDH1-PARTS-TAB                                                
052270        AT END                                                            
052280           MOVE INVIN-IDARTNR TO W-IDARTNR                                
052290           PERFORM SS-CHK-ACTV-INVTRY                                     
052291        WHEN WS-WDH1-IDARTNR(INDX) = W-IDARTNR                            
052292           PERFORM SS-CHK-ACTV-INVTRY                                     
052293     END-SEARCH                                                           
052294*****************                                                         
052295     IF WS-ACTV-INVTRY NOT = JA                                           
052300        ADD -10 TO AA11-CLAG-KDERS                                        
052400                   W-SP-KDERS                                             
052500        MOVE JA TO ANDRAD-KDERS-SW (1)                                    
052600                   AA11-UPPDATERAD-SW                                     
052700                                                                          
052800        MOVE 3 TO KDSTATUS (1)                                            
052900        MOVE 0 TO TIERSDAT (1)                                            
052910     END-IF                                                               
053000                                                                          
053100     .                                                                    
053200     EJECT                                                                
053300 BC-FORTSATT-TILL-STATUS-0 SECTION.                                       
053400                                                                          
053500                                                                          
053510*****************                                                         
053520* SEARCH THE FILE OF ACTIVE INV. PARTS TO CHECK IF THERE WAS AN           
053530* ACTIVE INVENTORY FOR THE PART. IF NOT FOUND, CHECK WDH1                 
053540* TO SEE IF THERE IS ANY INVENTORY IN PROGRESS NOW.                       
053550     SET INDX TO +1                                                       
053560     SEARCH WDH1-PARTS-TAB                                                
053570        AT END                                                            
053580           MOVE INVIN-IDARTNR TO W-IDARTNR                                
053590           PERFORM SS-CHK-ACTV-INVTRY                                     
053591        WHEN WS-WDH1-IDARTNR(INDX) = W-IDARTNR                            
053592           PERFORM SS-CHK-ACTV-INVTRY                                     
053593     END-SEARCH                                                           
053594*****************                                                         
053600     MOVE NEJ TO ERS01-DELETE-SW                                          
053700     MOVE JA TO ANDRAD-KDERS-SW (1)                                       
053800                AA11-UPPDATERAD-SW                                        
053900     IF AA11-CLAG-KDERS    = 19      AND                                  
054000        AA01-ART-IDLEVNR   = 'BQ8VA' AND                                  
054100        AA11-CLAG-REDIRLEV < 1.0                                          
054200        MOVE ZERO TO AA11-CLAG-KDERS                                      
054300                     W-SP-KDERS                                           
054400        MOVE ZERO TO AA11-CLAG-KDKSP                                      
054500                     W-SP-KDKSP                                           
054600        MOVE ZERO TO AA01-ART-TIERSDAT                                    
054700        MOVE JA   TO W-SP-FLTPO1                                          
054800        MOVE JA TO ERS01-DELETE-SW                                        
054900     ELSE                                                                 
054910        IF WS-ACTV-INVTRY NOT = JA                                        
055000           ADD +10 TO AA11-CLAG-KDERS                                     
055100                      W-SP-KDERS                                          
055200           MOVE DAGENS-DAGNR TO AA01-ART-TIERSDAT                         
055300        END-IF                                                            
055310     END-IF                                                               
055400                                                                          
055500     MOVE JA TO AA01-UPPDATERAD-SW                                        
055600                                                                          
055700     MOVE +0 TO KDSTATUS (1)                                              
055800                                                                          
055900     IF ERS01-DELETE                                                      
056000        CONTINUE                                                          
056100     ELSE                                                                 
056200        PERFORM S12-SKAPA-INV-TRANS                                       
056300     END-IF                                                               
056400                                                                          
056500     PERFORM IMS-GET-ARTG01                                               
056600     IF SEGMENT-FINNS                                                     
056700       IF AA11-CLAG-KDERS > 20                                            
056800         IF NYP-ART-KDANSKQ NOT = '9'                                     
056900           MOVE ZERO TO W-KDANSKQ                                         
057000           PERFORM S217-SKRIV-W111193                                     
057100         END-IF                                                           
057200                                                                          
057300         IF NYP-ART-KDANSKQ = '2'                                         
057400            MOVE SPACE     TO W111241-AREA                                
057500            MOVE '241'     TO 1142-IDPTYP                                 
057600            MOVE W-IDARTNR TO 1142-IDARTNR                                
057700            MOVE '1'       TO 1142-KDSEGKEY                               
057800            PERFORM S214-SKRIV-W11118                                     
057900         END-IF                                                           
058000       END-IF                                                             
058100     END-IF                                                               
058200     .                                                                    
058300     EJECT                                                                
058400 BD-UPPDATERA-ARTREG SECTION.                                             
058500                                                                          
058600     IF AA01-UPPDATERAD                                                   
058700       MOVE '100'             TO W111190-IDPTYP                           
058800       MOVE AA01-ART-IDARTNR  TO W111190-IDARTNR                          
058900       MOVE AA01-ART-TIERSDAT TO W111190-TIERSDAT                         
059000       PERFORM S215-SKRIV-W11119                                          
059100     END-IF                                                               
059200     IF AA11-UPPDATERAD                                                   
059300       MOVE '101'        TO W111191-IDPTYP                                
059400       MOVE W-SP-IDARTNR TO W111191-IDARTNR                               
059500       MOVE W-SP-FLTPO1  TO W111191-FLTPO1                                
059600       MOVE W-SP-KDERS   TO W111191-KDERS                                 
059700       MOVE W-SP-KDKSP   TO W111191-KDKSP                                 
059800       MOVE W-SP-KDAVT   TO W111191-KDAVT                                 
059900       PERFORM S215-SKRIV-W11119                                          
060000     END-IF                                                               
060100     IF ERS01-DELETE                                                      
060200       MOVE '105'             TO W111190-IDPTYP                           
060300       MOVE AA01-ART-IDARTNR  TO W111190-IDARTNR                          
060400       MOVE ZERO              TO W111190-TIERSDAT                         
060500       PERFORM S215-SKRIV-W11119                                          
060600     END-IF                                                               
060700     .                                                                    
060800     EJECT                                                                
060900 BE-BEHANDLA-KDKSP SECTION.                                               
061000                                                                          
061100     SKIP3                                                                
061200     IF NAGOT-KDERS-ANDRAT                                                
061300       IF (KDERS-OLD > +0 AND < +10) AND                                  
061400       (AA11-CLAG-KDERS > +9) AND                                         
061500       (AA11-CLAG-KDKSP = +4)                                             
061600                                                                          
061700         MOVE +1 TO AA11-CLAG-KDKSP                                       
061800                    W-SP-KDKSP                                            
061900         MOVE JA TO AA11-UPPDATERAD-SW                                    
062000       ELSE                                                               
062100         IF (KDERS-OLD > +9) AND                                          
062200         (AA11-CLAG-KDERS > +0 AND < +10) AND                             
062300         (AA11-CLAG-KDKSP = +1)                                           
062400                                                                          
062500           MOVE +4 TO AA11-CLAG-KDKSP                                     
062600                      W-SP-KDKSP                                          
062700           MOVE JA TO AA11-UPPDATERAD-SW                                  
062800         END-IF                                                           
062900       END-IF                                                             
063000     END-IF                                                               
063100     .                                                                    
063200     EJECT                                                                
063210 SS-CHK-ACTV-INVTRY SECTION.                                              
063220*** CHECK IF AN ACTIVE INVENTORY IS IN PROGRESS FOR CDC. THIS IS          
063230*** CHECKED BY CHECKING IF AN ENTRY EXISTS FOR CDC IN                     
063240*** WDH1 FOR A PART. IF YES, WRITE THE PARTS INTO A FILE WHICH            
063250*** WILL BE CHECKED IN THE NEXT RUN OF THE PGM.                           
063260     MOVE WC-CDC-SE              TO W-IDDC-WDH1-MIN                       
063270                                    W-IDDC-WDH1-MAX                       
063304     MOVE NEJ                    TO WS-FND                                
063305     PERFORM IMS-GU-WDH101                                                
063306     IF SEGMENT-FINNS                                                     
063307        PERFORM IMS-GNP-WDH111                                            
063308        PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                        
063309                                     OR WS-FND = JA                       
063310          IF SEGMENT-FINNS                                                
063311             IF INV-KDINVKAT = 3                                          
063312* IF THE 1ST RECORD RETURNED IS KAT 3, CHECK AGAIN TO CONFIRM             
063313* IF THERE IS ANY KAT OTHER THAN 3. JUST KAT 3 IS NOT ACTIVE              
063314* INVENTORY AND THE PART SHOULD NOT BE WRITTEN INTO THE FILE              
063315                PERFORM IMS-GNP-WDH111                                    
063316                IF SEGMENT-FINNS                                          
063317                   ADD +1                     TO WS-CNT                   
063318                   MOVE JA                    TO WS-FND                   
063319                   MOVE JA                    TO WS-ACTV-INVTRY           
063320                   MOVE W-IDARTNR             TO W11128-IDARTNR           
063321                   MOVE INV-KDINVKAT          TO W11128-KDINVKAT          
063322                   PERFORM S218-SKRIV-W11128A                             
063323                END-IF                                                    
063324             ELSE                                                         
063325                ADD +1                        TO WS-CNT                   
063326                MOVE JA                       TO WS-FND                   
063327                MOVE JA                       TO WS-ACTV-INVTRY           
063328                MOVE W-IDARTNR                TO W11128-IDARTNR           
063329                MOVE INV-KDINVKAT             TO W11128-KDINVKAT          
063330                PERFORM S218-SKRIV-W11128A                                
063331             END-IF                                                       
063332          END-IF                                                          
063334        END-PERFORM                                                       
063335     END-IF                                                               
063339     .                                                                    
063340     EJECT                                                                
063350 S1-LAES-INVENT-W51385  SECTION.                                          
063400                                                                          
063500     READ W51385 INTO INVIN-AREA                                          
063600     AT END                                                               
063700       MOVE JA TO EOF-W51385-SW                                           
063800     NOT AT END                                                           
063900       MOVE 'W51385'       TO POSTSUM-FDNAMN                              
064000       MOVE 'W11118D1'     TO POSTSUM-DDNAMN2                             
064100       MOVE SPACE          TO POSTSUM-TRANSTYP                            
064200       CALL POSTSUM USING POSTSUM-PARM                                    
064300     END-READ                                                             
064400     .                                                                    
064500     EJECT                                                                
064600 S2-TRANS-AK-BEVAKN SECTION.                                              
064700                                                                          
064800     MOVE '243'           TO 1134-IDPTYP                                  
064900     MOVE W-IDARTNR       TO 1134-IDARTNR                                 
065000     MOVE 'D'             TO 1134-KDUPPD                                  
065100     PERFORM S214-SKRIV-W11118                                            
065200     .                                                                    
065300     EJECT                                                                
065400 S3-TRANS-TILL-SATSSYST SECTION.                                          
065500                                                                          
065600     IF (AA01-ART-IDLEVNR = '1002 ') OR (AA01-ART-FLIART = JA)            
065700       MOVE '242'          TO 2303-IDPTYP                                 
065800       MOVE ZERO           TO 2303-IDARTNR-SATS                           
065900       MOVE ZERO           TO 2303-IDARTNR-ING                            
066000       IF AA01-ART-IDLEVNR = '1002 '                                      
066100         MOVE W-IDARTNR    TO 2303-IDARTNR-SATS                           
066200       END-IF                                                             
066300       IF AA01-ART-FLIART = JA                                            
066400         MOVE W-IDARTNR    TO 2303-IDARTNR-ING                            
066500       END-IF                                                             
066600       MOVE AA11-CLAG-KDERS TO 2303-KDERS-NEW                             
066700       MOVE KDERS-OLD       TO 2303-KDERS-OLD                             
066800       MOVE DAGENS-DAGNR    TO 2303-TIERSDAT-PREL                         
066900                                                                          
067000       PERFORM S214-SKRIV-W11118                                          
067100     END-IF                                                               
067200     .                                                                    
067300     EJECT                                                                
067400 S6-TRANS-TILL-VR-SYST SECTION.                                           
067500                                                                          
067600***                                                                       
067700* SKRIVER POST PÅ FIL W11123 FÖR UPPLÄGG AV SEGMENT                       
067800* PÅ WDG3 HTYP 9101                                                       
067900***                                                                       
068000     SKIP3                                                                
068100     IF AA11-CLAG-KDERS = +0   OR   > +10                                 
068200       MOVE '100'           TO W11116-IDPTYP                              
068300       MOVE W-IDARTNR       TO W11116-IDARTNR                             
068400       MOVE KDERS-OLD       TO W11116-KDERS-OLD                           
068500       MOVE AA11-CLAG-KDERS TO W11116-KDERS-NEW                           
068600                                                                          
068700       PERFORM S213-SKRIV-W11116                                          
068800     END-IF                                                               
068900     .                                                                    
069000     EJECT                                                                
069100 S8-TRANS-TILL-PPMS SECTION.                                              
069200                                                                          
069300     MOVE NEJ TO PPMS-TRANS                                               
069400     IF KDERS-OLD < 10 AND AA11-CLAG-KDERS > 10                           
069500       MOVE JA TO PPMS-TRANS                                              
069600     ELSE                                                                 
069700       EVALUATE TRUE                                                      
069800       WHEN KDERS-OLD > 10 AND AA11-CLAG-KDERS < 10                       
069900         MOVE JA TO PPMS-TRANS                                            
070000       WHEN KDERS-OLD > 10 AND AA11-CLAG-KDERS > 10                       
070100         MOVE JA TO PPMS-TRANS                                            
070200       END-EVALUATE                                                       
070300     END-IF                                                               
070400     IF PPMS-TRANS = JA                                                   
070500       MOVE SPACE TO UT-PPMS-W111PPMS                                     
070600       MOVE 'RPP'                   TO UT-PPMS-IDPTYP                     
070700       MOVE W-IDARTNR               TO UT-PPMS-IDARTNR                    
070800       ACCEPT UT-PPMS-TIAAMMDD FROM DATE                                  
070900       ACCEPT UT-PPMS-TIKLOCK FROM TIME                                   
071000       IF AA11-CLAG-KDERS > 10                                            
071100         MOVE AA11-CLAG-KDERS   TO UT-PPMS-KDERS                          
071200       ELSE                                                               
071300         MOVE ZERO              TO UT-PPMS-KDERS                          
071400       END-IF                                                             
071500       MOVE AA11-CLAG-KDERS TO WS-ALT-ERS                                 
071600       IF AA11-CLAG-KDERS = ZERO                                          
071700         CONTINUE                                                         
071800       ELSE                                                               
071900         EVALUATE TRUE                                                    
072000         WHEN AA11-CLAG-KDERS = 09 OR 19 OR 29 OR 52                      
072100           CONTINUE                                                       
072200         WHEN ALT-ERS                                                     
072300           MOVE 'FLERA'              TO UT-PPMS-ANMARKNING                
072400          WHEN OTHER                                                      
072500           PERFORM IMS-GET-ERSA01                                         
072600           MOVE DLI-IO-AREA TO ERSA01-AREA                                
072700           IF ERSA01-KVKORT > 1                                           
072800             MOVE 'FLERA'           TO UT-PPMS-ANMARKNING                 
072900           ELSE                                                           
073000             MOVE ZERO TO NOLL-RAKNARE                                    
073100             PERFORM IMS-GET-ERSA11                                       
073200             MOVE DLI-IO-AREA TO ERSA11-AREA                              
073300             MOVE ERSA11-IDARTNR-TILLK TO WS-PPMS-IDARTNR                 
073400             MOVE WS-PPMS-IDARTNR TO WS-PPMS-IDARTNR-TILLK                
073500             INSPECT WS-PPMS-IDARTNR-TILLK TALLYING                       
073600             NOLL-RAKNARE FOR LEADING ZERO                                
073700             ADD +1 TO NOLL-RAKNARE                                       
073800             UNSTRING WS-PPMS-IDARTNR-TILLK INTO                          
073900             UT-PPMS-ANMARKNING WITH POINTER NOLL-RAKNARE                 
074000           END-IF                                                         
074100         END-EVALUATE                                                     
074200       END-IF                                                             
074300       WRITE PPMS-POST  FROM UT-PPMS-W111PPMS                             
074400       MOVE 'W11115'   TO POSTSUM-FDNAMN                                  
074500       MOVE 'W11118D3' TO POSTSUM-DDNAMN2                                 
074600       MOVE 'UTP'      TO POSTSUM-TRANSTYP                                
074700       CALL POSTSUM USING POSTSUM-PARM                                    
074800     END-IF                                                               
074900     .                                                                    
075000     EJECT                                                                
075100 S9-TRANS-TILL-BASLAGER SECTION.                                          
075200                                                                          
075300*****************************************************************         
075400*  ÄT NOV 92  TRANS 1158 TILL ERSÄTTNINGSBEVAKNING BASLAGER VID *         
075500*             NY EK > 10. TRANSEN LÄSES OCH DELEATAS I W115D1.  *         
075600*****************************************************************         
075700                                                                          
075800                                                                          
075900     IF AA11-CLAG-KDERS > +10                                             
076000        PERFORM IMS-GET-ARTG01                                            
076100        IF SEGMENT-FINNS                                                  
076200           MOVE AA11-CLAG-KDERS TO 1158-KDERS                             
076300           MOVE W-IDARTNR       TO 1158-IDARTNR                           
076400           MOVE WS-DAGENS-DATUM TO 1158-TIREGDAT                          
076500           MOVE '240'           TO 1158-IDPTYP                            
076600           PERFORM S214-SKRIV-W11118                                      
076700        END-IF                                                            
076800     END-IF                                                               
076900     .                                                                    
077000     EJECT                                                                
077100 S10-BEHANDLA-FLTPO1 SECTION.                                             
077200                                                                          
077300*****************************************************************         
077400*  ÄT JAN 93  NÄR EN ARTIKEL BLIR DEFINITIVT ERSATT ,EK > 20,   *         
077500*             SKALL FLAGGA TPO1 SÄTTAS TILL NEJ.                *         
077600*     AUG 93  UPPDATERING FLTPO1 PÅ TILLK ART VID EK > 10       *         
077700*****************************************************************         
077800                                                                          
077900     IF AA11-CLAG-FLTPO1 = JA                                             
078000        IF AA11-CLAG-KDERS > 10                                           
078100           MOVE AA11-CLAG-KDERS TO WS-KDERS                               
078200           IF WS-KDERS-2 = 1 OR 2 OR 3                                    
078300              PERFORM IMS-GET-ERSA01                                      
078400              MOVE DLI-IO-AREA TO ERSA01-AREA                             
078500              IF ERSA01-KVKORT > 1                                        
078600                 CONTINUE                                                 
078700              ELSE                                                        
078800                 PERFORM IMS-GET-ERSA11                                   
078900                 IF SEGMENT-FINNS                                         
079000                    MOVE DLI-IO-AREA TO ERSA11-AREA                       
079100                    MOVE ERSA11-IDARTNR-TILLK TO W-IDARTNR-T              
079200                    PERFORM IMS-GU-ARTC11                                 
079300                    MOVE DLI-IO-AREA TO ARTC11-AREA                       
079400                                                                          
079500                    IF ARTC11-CLAG-FLTPO1 = NEJ                           
079600                       MOVE '102'       TO W111191-IDPTYP                 
079700                       MOVE W-IDARTNR-T TO W111191-IDARTNR                
079800                       MOVE JA          TO W111191-FLTPO1                 
079900                       PERFORM S215-SKRIV-W11119                          
080000                    END-IF                                                
080100                 END-IF                                                   
080200              END-IF                                                      
080300           END-IF                                                         
080400        END-IF                                                            
080500     END-IF                                                               
080600                                                                          
080700     IF AA11-CLAG-KDERS  > +20                                            
080800       IF AA11-CLAG-KDERS = +27 OR +28                                    
080900* ------ TILFÄLLIG ERS                                                    
081000         CONTINUE                                                         
081100       ELSE                                                               
081200         MOVE NEJ TO AA11-CLAG-FLTPO1                                     
081300         MOVE JA  TO AA11-UPPDATERAD-SW                                   
081400       END-IF                                                             
081500     END-IF                                                               
081600     .                                                                    
081700     EJECT                                                                
081710 S11-LOAD-WDH1-TABLE SECTION.                                             
081720                                                                          
081730     SET INDX                  TO +1                                      
081740     PERFORM S11A-READ-INPUT                                              
081750     PERFORM UNTIL INPUT-EOF                                              
081760         MOVE W11128-IDARTNR   TO WS-WDH1-IDARTNR(INDX)                   
081770         SET INDX UP BY +1                                                
081780         PERFORM S11A-READ-INPUT                                          
081790     END-PERFORM                                                          
081791     EJECT                                                                
081792     .                                                                    
081793 S11A-READ-INPUT SECTION.                                                 
081794                                                                          
081795     READ W11128        INTO W11128-AREA                                  
081796     AT END                                                               
081797       SET INPUT-EOF      TO TRUE                                         
081798     END-READ                                                             
081799     .                                                                    
081800     EJECT                                                                
081810 S12-SKAPA-INV-TRANS SECTION.                                             
081900******************************************************************        
082000* SAMTLIGA ARTIKLAR MED NY EK > 20 SKICKAS VIDARE TILL INV       *        
082100* ÄT FÖR NDC                                                     *        
082200******************************************************************        
082300                                                                          
082310     MOVE 'W11121'       TO POSTSUM-FDNAMN                                
082320     MOVE 'W11118D8'     TO POSTSUM-DDNAMN2                               
082330     MOVE SPACE          TO POSTSUM-TRANSTYP                              
082340                                                                          
082400     MOVE W-IDARTNR      TO INVUT-IDARTNR                                 
082500     MOVE 3              TO INVUT-KDINVKAT                                
082510                                                                          
082520     MOVE +1             TO WS-IX                                         
082530     PERFORM UNTIL WS-IX > WS-IX-MAX OR                                   
082540       WS-TAB-IDDC (WS-IX) = SPACE                                        
082541                                                                          
082542        MOVE WS-TAB-IDDC (WS-IX) TO INVUT-IDDC                            
082543        WRITE W11121-POST FROM INVUT-AREA                                 
082544        CALL POSTSUM USING POSTSUM-PARM                                   
082550        ADD +1           TO WS-IX                                         
082560     END-PERFORM                                                          
084600                                                                          
084700     .                                                                    
084800     EJECT                                                                
084920  S210-POST-TILL-W11122-WDR5 SECTION.                                     
085000                                                                          
085100     IF NAGOT-KDERS-ANDRAT                                                
085200       IF (KDERS-OLD < 10 AND AA11-CLAG-KDERS > 10)                       
085300         MOVE JA TO FL-POST-TILL-W11122                                   
085400       END-IF                                                             
085500       IF (KDERS-OLD > 10 AND AA11-CLAG-KDERS < 10)                       
085600         MOVE JA TO FL-POST-TILL-W11122                                   
085700       END-IF                                                             
085800     END-IF                                                               
085900     IF FL-POST-TILL-W11122 = JA                                          
086000       PERFORM S211-SKAPA-POST-TILL-W11122                                
086100     END-IF                                                               
086200     .                                                                    
086300     EJECT                                                                
086400  S211-SKAPA-POST-TILL-W11122 SECTION.                                    
086500                                                                          
086600     MOVE W-IDARTNR TO W11114-IDARTNR                                     
086700     PERFORM S212-SKRIV-W11114                                            
086800     .                                                                    
086900     EJECT                                                                
087000  S212-SKRIV-W11114 SECTION.                                              
087100                                                                          
087200     WRITE W11114-POST   FROM W11114-AREA                                 
087300     MOVE 'W11114'       TO POSTSUM-FDNAMN                                
087400     MOVE 'W11118D2'     TO POSTSUM-DDNAMN2                               
087500     MOVE SPACE          TO POSTSUM-TRANSTYP                              
087600     CALL POSTSUM USING POSTSUM-PARM                                      
087700     .                                                                    
087800     EJECT                                                                
087900  S213-SKRIV-W11116 SECTION.                                              
088000                                                                          
088100***                                                                       
088200* SKRIVER POST PÅ FIL W11116 FÖR SENARE UPPLÄGG AV SEGMENT                
088300* PÅ WDG3 HTYP 9101                                                       
088400***                                                                       
088500       WRITE W11116-POST FROM W11116-AREA                                 
088600       MOVE 'W11116'        TO POSTSUM-FDNAMN                             
088700       MOVE 'W11118D4'      TO POSTSUM-DDNAMN2                            
088800       MOVE SPACE           TO POSTSUM-TRANSTYP                           
088900       CALL POSTSUM USING POSTSUM-PARM                                    
089000       .                                                                  
089100     EJECT                                                                
089200  S214-SKRIV-W11118 SECTION.                                              
089300***                                                                       
089400* SKRIVER POST PÅ FIL W11118 FÖR SENARE UPPLÄGG AV                        
089500* SEGMENT PÅ WDG3 HTYP 1157/1158 SAMT HTYP 2303  (TIDIGARE 2301)          
089600***                                                                       
089700                                                                          
089800     IF      W11118-IDPTYP = '240'                                        
089900       WRITE W111240-POST   FROM W111240-AREA                             
090000     ELSE IF W11118-IDPTYP = '241'                                        
090100       WRITE W111241-POST   FROM W111241-AREA                             
090200     ELSE IF W11118-IDPTYP = '242'                                        
090300       WRITE W111242-POST   FROM W111242-AREA                             
090400     ELSE IF W11118-IDPTYP = '243'                                        
090500       WRITE W111243-POST   FROM W111243-AREA                             
090600     ELSE                                                                 
090700       DISPLAY 'W11118 - FEL POSTTYP PÅ W11118 ' W11118-IDPTYP            
090800       CALL FELLOG                                                        
090900     END-IF END-IF END-IF END-IF                                          
091000                                                                          
091100     MOVE 'W11118'      TO POSTSUM-FDNAMN                                 
091200     MOVE 'W11118D6'    TO POSTSUM-DDNAMN2                                
091300     MOVE W11118-IDPTYP TO POSTSUM-TRANSTYP                               
091400     CALL POSTSUM USING POSTSUM-PARM                                      
091500     .                                                                    
091600     EJECT                                                                
091700  S215-SKRIV-W11119 SECTION.                                              
091800***                                                                       
091900* SKRIVER POST PÅ FIL W11119 FÖR SENARE UPPDATERING                       
092000* AV WDK6 (WLARTC01 ELLER WLARTC11)                                       
092100***                                                                       
092200                                                                          
092300     WRITE W11119-POST FROM W11119-AREA                                   
092400     MOVE 'W11119'     TO POSTSUM-FDNAMN                                  
092500     MOVE 'W11118D7'   TO POSTSUM-DDNAMN2                                 
092600     MOVE SPACE        TO POSTSUM-TRANSTYP                                
092700     CALL POSTSUM USING POSTSUM-PARM                                      
092800     .                                                                    
092900     EJECT                                                                
093000  S216-SKRIV-W111192 SECTION.                                             
093100***                                                                       
093200* SKRIVER POST PÅ FIL W11119 FÖR SENARE UPPDATERING                       
093300* AV WDD704 (WLERSA13)                                                    
093400***                                                                       
093500     MOVE W-IDARTNR    TO W111192-IDARTNR                                 
093600     MOVE KDSTATUS (1) TO W111192-KDSTATUS-C1                             
093700     MOVE KDSTATUS (2) TO W111192-KDSTATUS-C2                             
093800     MOVE TIERSDAT (1) TO W111192-TIERSDAT-PREL-C1                        
093900     MOVE TIERSDAT (2) TO W111192-TIERSDAT-PREL-C2                        
094000     MOVE '103'        TO W111192-IDPTYP                                  
094100                                                                          
094200     PERFORM S215-SKRIV-W11119                                            
094300     .                                                                    
094400     EJECT                                                                
094500  S217-SKRIV-W111193 SECTION.                                             
094600***                                                                       
094700* SKRIVER POST PÅ FIL W11119 FÖR SENARE UPPDATERING                       
094800* AV WDD201 (NYPON)(WLARTG)                                               
094900***                                                                       
095000     MOVE W-IDARTNR    TO W111193-IDARTNR                                 
095100     MOVE W-KDANSKQ    TO W111193-KDANSKQ                                 
095200     MOVE '104'        TO W111193-IDPTYP                                  
095300                                                                          
095400     WRITE W11119-POST FROM W111191-AREA                                  
095500     MOVE 'W11119'     TO POSTSUM-FDNAMN                                  
095600     MOVE 'W11118D7'   TO POSTSUM-DDNAMN2                                 
095700     MOVE SPACE        TO POSTSUM-TRANSTYP                                
095800     CALL POSTSUM USING POSTSUM-PARM                                      
095900                                                                          
096000     .                                                                    
096100     EJECT                                                                
096110  S218-SKRIV-W11128A SECTION.                                             
096120                                                                          
096130     WRITE W11128A-POST  FROM W11128-AREA                                 
096140     MOVE 'W11128A'      TO POSTSUM-FDNAMN                                
096150     MOVE 'W11128DA'     TO POSTSUM-DDNAMN2                               
096160     MOVE SPACE          TO POSTSUM-TRANSTYP                              
096170     CALL POSTSUM USING POSTSUM-PARM                                      
096180     .                                                                    
096190     EJECT                                                                
096200**************************                                                
096300* IMS SECTIONER                                                           
096400     SKIP3                                                                
096500 IMS-GET-AA01 SECTION.                                                    
096600*                         WDK601                                          
096700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
096800     DELIMITED BY SIZE INTO SSA1                                          
096900     MOVE '  GE' TO GODK-STATUSKODER                                      
097000     CALL CBLTDLI USING GU ARTC1-PCB DLI-IO-AREA SSA1                     
097100     MOVE ARTC1-STATUS-CODE TO STATUS-WS                                  
097200     PERFORM IMS-STATUSKONTROLL                                           
097300     .                                                                    
097400     SKIP3                                                                
097500 IMS-GET-AA11 SECTION.                                                    
097600*                         WDK611                                          
097700     MOVE  'WLARTC11 '  TO  SSA1                                          
097800     MOVE '  ' TO GODK-STATUSKODER                                        
097900     CALL CBLTDLI USING GNP ARTC1-PCB DLI-IO-AREA SSA1                    
098000     MOVE ARTC1-STATUS-CODE  TO STATUS-WS                                 
098100     PERFORM IMS-STATUSKONTROLL                                           
098200     .                                                                    
098300     SKIP3                                                                
098400 IMS-GET-UNIK-AC04-MED-AC1 SECTION.                                       
098500*                                                                         
098600     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
098700     DELIMITED BY SIZE INTO SSA1                                          
098800     MOVE 'WLERSA13  ' TO SSA2                                            
098900     MOVE '  GE' TO GODK-STATUSKODER                                      
099000     CALL CBLTDLI USING GU AC1-PCB DLI-IO-AREA SSA1 SSA2                  
099100     MOVE AC1-STATUS-CODE  TO STATUS-WS                                   
099200     PERFORM IMS-STATUSKONTROLL                                           
099300     .                                                                    
099400     EJECT                                                                
099500 IMS-GET-ERSA01 SECTION.                                                  
099600*                                                                         
099700     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
099800     DELIMITED BY SIZE INTO SSA1                                          
099900     MOVE '  ' TO GODK-STATUSKODER                                        
100000     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA SSA1                      
100100     MOVE ERSA-STATUS-CODE  TO STATUS-WS                                  
100200     PERFORM IMS-STATUSKONTROLL                                           
100300     .                                                                    
100400     SKIP3                                                                
100500 IMS-GET-ERSA11 SECTION.                                                  
100600*                                                                         
100700     MOVE 'WLERSA11 ' TO SSA1                                             
100800     MOVE '  GE' TO GODK-STATUSKODER                                      
100900     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
101000     MOVE ERSA-STATUS-CODE  TO STATUS-WS                                  
101100     PERFORM IMS-STATUSKONTROLL                                           
101200     .                                                                    
101300     SKIP3                                                                
101400 IMS-GET-ARTM01 SECTION.                                                  
101500*                                                                         
101600     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
101700     DELIMITED BY SIZE INTO SSA1                                          
101800     MOVE '  GE'                 TO GODK-STATUSKODER                      
101900     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA2 SSA1                     
102000     MOVE ARTM-STATUS-CODE       TO STATUS-WS                             
102100     PERFORM IMS-STATUSKONTROLL                                           
102200     .                                                                    
102300     SKIP3                                                                
102400 IMS-GU-ARTC11 SECTION.                                                   
102500*                                                                         
102600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-T-X ')'                       
102700     DELIMITED BY SIZE INTO SSA1                                          
102800     MOVE 'WLARTC11 ' TO SSA2                                             
102900     MOVE '  GE' TO GODK-STATUSKODER                                      
103000     CALL CBLTDLI USING GU ARTC2-PCB DLI-IO-AREA SSA1 SSA2                
103100     MOVE ARTC2-STATUS-CODE TO STATUS-WS                                  
103200     PERFORM IMS-STATUSKONTROLL                                           
103300     .                                                                    
103400     EJECT                                                                
103500 IMS-GET-ARTG01 SECTION.                                                  
103600*                                                                         
103700     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
103800     DELIMITED BY SIZE INTO SSA1                                          
103900     MOVE '  GE' TO GODK-STATUSKODER                                      
104000     CALL CBLTDLI USING GU ARTG-PCB DLI-IO-WDD2 SSA1                      
104100     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
104200     PERFORM IMS-STATUSKONTROLL                                           
104300     .                                                                    
104400     SKIP3                                                                
104500 IMS-GET-WDR501 SECTION.                                                  
104600*                                                                         
104700     STRING 'WDR501  (WDGXKEY  =' W-1133-KEY-X ')'                        
104800          DELIMITED BY SIZE INTO SSA1                                     
104900     MOVE '  ' TO GODK-STATUSKODER                                        
105000     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDR5 SSA1                      
105100     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
105200     PERFORM IMS-STATUSKONTROLL                                           
105300     .                                                                    
105400     SKIP3                                                                
105500 IMS-GET-WDGX1134 SECTION.                                                
105600                                                                          
105700     STRING 'WDGX1134*F(IDARTNR  =' W-IDARTNR-X ')'                       
105800           DELIMITED BY SIZE INTO SSA1                                    
105900     MOVE '  GE' TO GODK-STATUSKODER                                      
106000     CALL CBLTDLI USING GHNP WDR5-PCB DLI-IO-WDR5 SSA1                    
106100     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
106200     PERFORM IMS-STATUSKONTROLL                                           
106300     .                                                                    
106400     SKIP3                                                                
106500 IMS-GET-WDL201 SECTION.                                                  
106600*                                                                         
106700     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
106800             DELIMITED BY SIZE INTO SSA1                                  
106900     MOVE '  GE' TO GODK-STATUSKODER                                      
107000     CALL CBLTDLI USING GU WDL2-PCB DLI-IO-WDL2 SSA1                      
107100     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
107200     PERFORM IMS-STATUSKONTROLL                                           
107300     .                                                                    
107400     SKIP3                                                                
107500 IMS-GET-WDL221 SECTION.                                                  
107600*                                                                         
107700     MOVE 'WDL211  ' TO SSA1                                              
107800     STRING 'WDL221  (IDPTYP   =' W-IDPTYP-X ')'                          
107900             DELIMITED BY SIZE INTO SSA2                                  
108000     MOVE '  GE' TO GODK-STATUSKODER                                      
108100     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL2 SSA1 SSA2                
108200     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
108300     PERFORM IMS-STATUSKONTROLL                                           
108400     .                                                                    
108500     SKIP3                                                                
108595 IMS-GU-WDH101 SECTION.                                                   
108596                                                                          
108597     STRING 'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                         
108598            DELIMITED BY SIZE INTO SSA1                                   
108599     MOVE '  GE' TO GODK-STATUSKODER                                      
108600     CALL CBLTDLI USING GU WDH1-PCB DLI-IO-WDH101 SSA1                    
108601     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
108602     PERFORM IMS-STATUSKONTROLL                                           
108603     .                                                                    
108604     EJECT                                                                
108605 IMS-GNP-WDH111 SECTION.                                                  
108606                                                                          
108607     STRING 'WDH111  (WDH111KY>=' W-WDH1KEY-MIN-X                         
108608                    '&WDH111KY<=' W-WDH1KEY-MAX-X ')'                     
108609            DELIMITED BY SIZE INTO SSA1                                   
108610     MOVE '  GEGB' TO GODK-STATUSKODER                                    
108611     CALL CBLTDLI USING GNP WDH1-PCB DLI-IO-WDH111 SSA1                   
108612     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
108613     PERFORM IMS-STATUSKONTROLL                                           
108614     .                                                                    
108615     EJECT                                                                
108620 IMS-STATUSKONTROLL SECTION.                                              
108700     SET STATUS-IX TO 1                                                   
108800     SEARCH GODK-STATUS AT END CALL FELLOG                                
108900     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
109000     CONTINUE                                                             
109100     END-SEARCH                                                           
109200     .                                                                    
