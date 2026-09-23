000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9109100.                                                
000300 AUTHOR.         SATHISH THIRUVENGADAM.                                   
000400 DATE-WRITTEN.   22/11/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*    LOGGING OF ETA.                                                      
001000*    TO COMPARE THE RESULTS OF SLDO & ETD MODULES.                        
001100*                                                                         
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- INPUT DATA                                                 
002200     SELECT W91091                     ASSIGN TO W91091D1.                
002300     SKIP2                                                                
002400*          --- ETA DATA FROM OLD PROCESS                                  
002500     SELECT SLDO                       ASSIGN TO W91091D2.                
002600     SKIP2                                                                
002700*          --- ETA DATA FROM NEW PROCESS                                  
002800     SELECT ETD                        ASSIGN TO W91091D3.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W91091                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  POST -COPY W91091   -PRE IN-        -L.                              
003900     SKIP3                                                                
004000 FD  SLDO                                                                 
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  POST -COPY W91092   -PRE UT-SLDO-   -L.                              
004500     SKIP3                                                                
004600 FD  ETD                                                                  
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  POST -COPY W91092   -PRE UT-ETD-    -L.                              
005100     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300                                                                          
005400 77  IDPGM                       PIC X(8)    VALUE 'W9109100'.            
005500 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005600 77  YES                         PIC X       VALUE 'J'.                   
005700 77  NOO                         PIC X       VALUE 'N'.                   
005800     EJECT                                                                
005900                                                                          
006000                                                                          
006100 77  W91091-EOF-SW               PIC X       VALUE 'N'.                   
006200     88  END-OF-W91091                       VALUE 'Y'.                   
006300     EJECT                                                                
006400*                                                                         
006500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006600 01  FILLER REDEFINES TODAYS-DATE.                                        
006700     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006800     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006900     03  TODAYS-DATE-DAY         PIC 9(2).                                
007000     EJECT                                                                
007100 01    WS-KVAVBART               PIC 9(7)    VALUE ZERO.                  
007200 01    FILLER REDEFINES WS-KVAVBART.                                      
007300   03  FILLER                    PIC X.                                   
007400   03  WS-KVAVBART-X             PIC X(6).                                
007500                                                                          
007600 01    WS-TIAAMMDD               PIC 9(7)    VALUE ZERO.                  
007700 01    FILLER REDEFINES WS-TIAAMMDD.                                      
007800   03  FILLER                    PIC X.                                   
007900   03  WS-TIAAMMDD-X             PIC X(6).                                
008000                                                                          
008100 01  GENERAL-SUBPROGRAMS.                                                 
008200*                                                                         
008300     03  ABEND                   PIC X(8)   VALUE 'ABEND'.                
008400     03  POSTSUM                 PIC X(8)   VALUE 'POSTSUM'.              
008500     03  W911SLDO                PIC X(8)   VALUE 'W911SLDO'.             
008600     03  W911ETD                 PIC X(8)   VALUE 'W911ETD '.             
008700     SKIP2                                                                
008800*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
008900                                                                          
009000 01   FILLER             PIC X(5)  VALUE 'SLDO '.                         
009100*   -COPY W911SLDO                                                        
009200                                                                          
009300 01   FILLER             PIC X(5)  VALUE 'ETD  '.                         
009400*   -COPY W911ETD                                                         
009500                                                                          
009600*    --- PARAMETERS TO ABEND                                              
009700                                                                          
009800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010100     SKIP2                                                                
010200 01  ERROR-TEXT.                                                          
010300     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
010400     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
010500     EJECT                                                                
010600*    --- PARAMETRAR TILL POSTSUM                                          
010700*                                                                         
010800*01  -COPY W0005   -PRE  POSTSUM-                                         
010900     EJECT                                                                
011000 01  IN-AREA-START               PIC X(24)   VALUE                        
011100                                             'IN-AREA-START'.             
011200     SKIP2                                                                
011300                                                                          
011400*01  AREA -COPY W91091     -PRE IN-                                       
011500*                                                                         
011600     EJECT                                                                
011700 01  SLDO-AREA-START             PIC X(24)   VALUE                        
011800                                 'SLDO-AREA-START  '.                     
011900     SKIP2                                                                
012000*01  AREA -COPY W91092       -PRE SLDO-UT-                                
012100     EJECT                                                                
012200 01  ETD-AREA-START              PIC X(24)   VALUE                        
012300                                 'ETD-AREA-START  '.                      
012400     SKIP2                                                                
012500*01  AREA -COPY W91092       -PRE ETD-UT-                                 
012600     EJECT                                                                
012700 LINKAGE SECTION.                                                         
012800                                                                          
012900 01  SLDO-WDF1-PCB               PIC X.                                   
013000 01  SLDO-WDF2-PCB               PIC X.                                   
013100 01  SLDO-WDF2A-PCB              PIC X.                                   
013200 01  SLDO-WDK7-PCB               PIC X.                                   
013300 01  SLDO-WDK6-PCB               PIC X.                                   
013400 01  SLDO-XXKJ-PCB               PIC X.                                   
013500 01  SLDO-WDD7-PCB               PIC X.                                   
013600 01  SLDO-BENA-PCB               PIC X.                                   
013700 01  SLDO-WDB3-PCB               PIC X.                                   
013710 01  SLDO-WDR6-PCB               PIC X.                                   
013800                                                                          
013900 01  SLDO-KREG-GMTA-PCB          PIC X.                                   
014000 01  SLDO-KREG-GMTB-PCB          PIC X.                                   
014100 01  SLDO-KREG-GMTC-PCB          PIC X.                                   
014200 01  SLDO-KREG-BETC-PCB          PIC X.                                   
014300                                                                          
014400 01  SLDO-KVAN-WDB2-PCB          PIC X.                                   
014500 01  SLDO-KVAN-WDC1-PCB          PIC X.                                   
014600                                                                          
014700 01  SLDO-AREG-WDK6-PCB          PIC X.                                   
014800 01  SLDO-AREG-WDK7-PCB          PIC X.                                   
014900                                                                          
015000 01  SLDO-DLEV-LEVF-PCB          PIC X.                                   
015100 01  SLDO-DLEV-LEVG-PCB          PIC X.                                   
015200 01  SLDO-DLEV-LEVA-PCB          PIC X.                                   
015300 01  SLDO-DLEV-ARTS-PCB          PIC X.                                   
015400 01  SLDO-DLEV-WDB6-PCB          PIC X.                                   
015500 01  SLDO-DLEV-FILA-PCB          PIC X.                                   
015600                                                                          
015700 01  SLDO-SPAR-WDF8-PCB          PIC X.                                   
015800 01  SLDO-SPAR-WDF8A-PCB         PIC X.                                   
015900 01  SLDO-SPAR-WDK6-PCB          PIC X.                                   
016000                                                                          
016100 01  SLDO-SDCA-ARTS-PCB          PIC X.                                   
016200 01  SLDO-SDCA-WDB6-PCB          PIC X.                                   
016300 01  SLDO-SDCA-WDK9-PCB          PIC X.                                   
016400 01  SLDO-SDCA-WDR6-PCB          PIC X.                                   
016500 01  SLDO-SDCA-WDK6-PCB          PIC X.                                   
016600 01  SLDO-SDCA-WDQ4B-PCB         PIC X.                                   
016700 01  SLDO-SDCA-WDQ2-PCB          PIC X.                                   
016800 01  SLDO-SDCA-WDQ4-PCB          PIC X.                                   
016900 01  SLDO-SDCA-WDB6-2-PCB        PIC X.                                   
016900 01  SLDO-SDCA-WDK6-2-PCB        PIC X.                                   
016900 01  SLDO-SDCA-WDK7-2-PCB        PIC X.                                   
016900 01  SLDO-SDCA-WDK7-3-PCB        PIC X.                                   
016900                                                                          
017000 01  SLDO-NDCA-USEA-PCB          PIC X.                                   
017100 01  SLDO-NDCA-WDK7-PCB          PIC X.                                   
017200 01  SLDO-NDCA-WDL6-PCB          PIC X.                                   
017300 01  SLDO-NDCA-WDB6-PCB          PIC X.                                   
017400                                                                          
017500 01  SLDO-RANS-XXKM-PCB          PIC X.                                   
017600 01  SLDO-RANS-ARTM-PCB          PIC X.                                   
017700 01  SLDO-RANS-ARTS-PCB          PIC X.                                   
017800                                                                          
017900 01  SLDO-CDCA-ARTM-PCB          PIC X.                                   
018000 01  SLDO-CDCA-INLB-PCB          PIC X.                                   
018100 01  SLDO-CDCA-WDB2-PCB          PIC X.                                   
018200 01  SLDO-CDCA-WDC1-PCB          PIC X.                                   
018300                                                                          
018400 01  SLDO-CLDC-WDB6-PCB          PIC X.                                   
018500                                                                          
018600 01  SLDO-ETA-ARTC-PCB           PIC X.                                   
018700 01  SLDO-ETA-WDK7-PCB           PIC X.                                   
018800 01  SLDO-ETA-inlc-PCB           PIC X.                                   
018900 01  SLDO-ETA-LEVA-PCB           PIC X.                                   
019000 01  SLDO-ETA-WDB6-PCB           PIC X.                                   
019100 01  SLDO-ETA-WDD9-PCB           PIC X.                                   
019200                                                                          
019210 01  SLDO-XDCA-USEA-PCB          PIC X.                                   
019220 01  SLDO-XDCA-WDB6-PCB          PIC X.                                   
019230 01  SLDO-XDCA-WDK6-PCB          PIC X.                                   
019240 01  SLDO-XDCA-WDK7-PCB          PIC X.                                   
019250 01  SLDO-XDCA-WDK9-PCB          PIC X.                                   
019260 01  SLDO-XDCA-WDL6-PCB          PIC X.                                   
019270 01  SLDO-XDCA-WDQ4B-PCB         PIC X.                                   
019280 01  SLDO-XDCA-WDQ2-PCB          PIC X.                                   
019290 01  SLDO-XDCA-WDQ4-PCB          PIC X.                                   
019291 01  SLDO-XDCA-WDR6-PCB          PIC X.                                   
019292 01  SLDO-XDCA-WDB6-2-PCB        PIC X.                                   
019292 01  SLDO-XDCA-WDK6-2-PCB        PIC X.                                   
019292 01  SLDO-XDCA-WDK7-2-PCB        PIC X.                                   
019292 01  SLDO-XDCA-WDK7-3-PCB        PIC X.                                   
019292                                                                          
019300 01  ETD-WDF1-PCB               PIC X.                                    
019400 01  ETD-WDK6-PCB               PIC X.                                    
019500 01  ETD-WDK7-PCB               PIC X.                                    
019600 01  ETD-XXKJ-PCB               PIC X.                                    
019700 01  ETD-WDD7-PCB               PIC X.                                    
019800 01  ETD-BENA-PCB               PIC X.                                    
019900 01  ETD-WDB3-PCB               PIC X.                                    
020000 01  ETD-WDK9-PCB               PIC X.                                    
020100 01  ETD-WDA5-PCB               PIC X.                                    
020200 01  ETD-WDA6J-PCB              PIC X.                                    
020300 01  ETD-WDD9-PCB               PIC X.                                    
020400 01  ETD-WDL6-PCB               PIC X.                                    
020500 01  ETD-WDB6-PCB               PIC X.                                    
020600 01  ETD-WDF2-PCB               PIC X.                                    
020700 01  ETD-WDF2A-PCB              PIC X.                                    
020800                                                                          
020900 01  ETD-KREG-GMTA-PCB          PIC X.                                    
021000 01  ETD-KREG-GMTB-PCB          PIC X.                                    
021100 01  ETD-KREG-GMTC-PCB          PIC X.                                    
021200 01  ETD-KREG-BETC-PCB          PIC X.                                    
021300                                                                          
021400 01  ETD-KVAN-WDB2-PCB          PIC X.                                    
021500 01  ETD-KVAN-WDC1-PCB          PIC X.                                    
021600                                                                          
021700 01  ETD-AREG-WDK6-PCB          PIC X.                                    
021800 01  ETD-AREG-WDK7-PCB          PIC X.                                    
021900                                                                          
022000 01  ETD-SPAR-WDF8-PCB          PIC X.                                    
022100 01  ETD-SPAR-WDF8A-PCB         PIC X.                                    
022200 01  ETD-SPAR-WDK6-PCB          PIC X.                                    
022300                                                                          
022400 01  ETD-RANS-XXKM-PCB          PIC X.                                    
022500 01  ETD-RANS-ARTM-PCB          PIC X.                                    
022600 01  ETD-RANS-ARTS-PCB          PIC X.                                    
022700                                                                          
022800 PROCEDURE DIVISION  USING SLDO-WDF1-PCB                                  
022900                           SLDO-WDF2-PCB SLDO-WDF2A-PCB                   
023000                           SLDO-WDK7-PCB                                  
023100                           SLDO-WDK6-PCB SLDO-XXKJ-PCB                    
023200                           SLDO-WDD7-PCB SLDO-BENA-PCB                    
023300                           SLDO-WDB3-PCB SLDO-WDR6-PCB                    
023400                                                                          
023500                           SLDO-KREG-GMTA-PCB SLDO-KREG-GMTB-PCB          
023600                           SLDO-KREG-GMTC-PCB SLDO-KREG-BETC-PCB          
023700                                                                          
023800                           SLDO-KVAN-WDB2-PCB SLDO-KVAN-WDC1-PCB          
023900                                                                          
024000                           SLDO-AREG-WDK6-PCB SLDO-AREG-WDK7-PCB          
024100                                                                          
024200                           SLDO-DLEV-LEVF-PCB SLDO-DLEV-LEVG-PCB          
024300                           SLDO-DLEV-LEVA-PCB SLDO-DLEV-ARTS-PCB          
024400                           SLDO-DLEV-WDB6-PCB SLDO-DLEV-FILA-PCB          
024500                                                                          
024600                           SLDO-SPAR-WDF8-PCB SLDO-SPAR-WDF8A-PCB         
024700                           SLDO-SPAR-WDK6-PCB                             
024800                                                                          
024900                           SLDO-SDCA-ARTS-PCB SLDO-SDCA-WDB6-PCB          
025000                           SLDO-SDCA-WDK9-PCB SLDO-SDCA-WDR6-PCB          
025100                           SLDO-SDCA-WDK6-PCB SLDO-SDCA-WDQ4B-PCB         
025200                           SLDO-SDCA-WDQ2-PCB SLDO-SDCA-WDQ4-PCB          
025300                           SLDO-SDCA-WDB6-2-PCB                           
025300                           SLDO-SDCA-WDK6-2-PCB                           
025300                           SLDO-SDCA-WDK7-2-PCB                           
025300                           SLDO-SDCA-WDK7-3-PCB                           
025300                                                                          
025400                           SLDO-NDCA-USEA-PCB SLDO-NDCA-WDK7-PCB          
025500                           SLDO-NDCA-WDL6-PCB SLDO-NDCA-WDB6-PCB          
025600                                                                          
025700                           SLDO-RANS-XXKM-PCB SLDO-RANS-ARTM-PCB          
025800                           SLDO-RANS-ARTS-PCB                             
025900                                                                          
026000                           SLDO-CDCA-ARTM-PCB SLDO-CDCA-INLB-PCB          
026100                           SLDO-CDCA-WDB2-PCB SLDO-CDCA-WDC1-PCB          
026200                                                                          
026300                           SLDO-CLDC-WDB6-PCB                             
026400                                                                          
026500                           SLDO-ETA-ARTC-PCB  SLDO-ETA-WDK7-PCB           
026600                           SLDO-ETA-inlc-PCB  SLDO-ETA-LEVA-PCB           
026700                           SLDO-ETA-WDB6-PCB  SLDO-ETA-WDD9-PCB           
026800                                                                          
                                 SLDO-XDCA-USEA-PCB                             
040193                           SLDO-XDCA-WDB6-PCB SLDO-XDCA-WDK6-PCB          
040194                           SLDO-XDCA-WDK7-PCB SLDO-XDCA-WDK9-PCB          
040195                           SLDO-XDCA-WDL6-PCB SLDO-XDCA-WDQ4B-PCB         
040196                           SLDO-XDCA-WDQ2-PCB SLDO-XDCA-WDQ4-PCB          
040197                           SLDO-XDCA-WDR6-PCB                             
040197                           SLDO-XDCA-WDB6-2-PCB                           
040197                           SLDO-XDCA-WDK6-2-PCB                           
040197                           SLDO-XDCA-WDK7-2-PCB                           
040197                           SLDO-XDCA-WDK7-3-PCB                           
040192                                                                          
026900                           ETD-WDF1-PCB ETD-WDK6-PCB                      
027000                                                                          
027100                           ETD-WDK7-PCB ETD-XXKJ-PCB                      
027200                           ETD-WDD7-PCB ETD-BENA-PCB                      
027300                           ETD-WDB3-PCB ETD-WDK9-PCB ETD-WDA5-PCB         
027400                           ETD-WDA6J-PCB ETD-WDD9-PCB                     
027500                           ETD-WDL6-PCB ETD-WDB6-PCB                      
027600                           ETD-WDF2-PCB ETD-WDF2A-PCB                     
027700                                                                          
027800                           ETD-KREG-GMTA-PCB ETD-KREG-GMTB-PCB            
027900                           ETD-KREG-GMTC-PCB ETD-KREG-BETC-PCB            
028000                                                                          
028100                           ETD-KVAN-WDB2-PCB ETD-KVAN-WDC1-PCB            
028200                                                                          
028300                           ETD-AREG-WDK6-PCB ETD-AREG-WDK7-PCB            
028400                                                                          
028500                           ETD-SPAR-WDF8-PCB ETD-SPAR-WDF8A-PCB           
028600                           ETD-SPAR-WDK6-PCB                              
028700                                                                          
028800                           ETD-RANS-XXKM-PCB ETD-RANS-ARTM-PCB            
028900                           ETD-RANS-ARTS-PCB.                             
029000                                                                          
029100     ENTRY 'DLITCBL' USING SLDO-WDF1-PCB                                  
029200                           SLDO-WDF2-PCB SLDO-WDF2A-PCB                   
029300                           SLDO-WDK7-PCB                                  
029400                           SLDO-WDK6-PCB SLDO-XXKJ-PCB                    
029500                           SLDO-WDD7-PCB SLDO-BENA-PCB                    
029600                           SLDO-WDB3-PCB SLDO-WDR6-PCB                    
029700                                                                          
029800                           SLDO-KREG-GMTA-PCB SLDO-KREG-GMTB-PCB          
029900                           SLDO-KREG-GMTC-PCB SLDO-KREG-BETC-PCB          
030000                                                                          
030100                           SLDO-KVAN-WDB2-PCB SLDO-KVAN-WDC1-PCB          
030200                                                                          
030300                           SLDO-AREG-WDK6-PCB SLDO-AREG-WDK7-PCB          
030400                                                                          
030500                           SLDO-DLEV-LEVF-PCB SLDO-DLEV-LEVG-PCB          
030600                           SLDO-DLEV-LEVA-PCB SLDO-DLEV-ARTS-PCB          
030700                           SLDO-DLEV-WDB6-PCB SLDO-DLEV-FILA-PCB          
030800                                                                          
030900                           SLDO-SPAR-WDF8-PCB SLDO-SPAR-WDF8A-PCB         
031000                           SLDO-SPAR-WDK6-PCB                             
031100                                                                          
031200                           SLDO-SDCA-ARTS-PCB SLDO-SDCA-WDB6-PCB          
031300                           SLDO-SDCA-WDK9-PCB SLDO-SDCA-WDR6-PCB          
031400                           SLDO-SDCA-WDK6-PCB SLDO-SDCA-WDQ4B-PCB         
031500                           SLDO-SDCA-WDQ2-PCB SLDO-SDCA-WDQ4-PCB          
031600                           SLDO-SDCA-WDB6-2-PCB                           
031600                           SLDO-SDCA-WDK6-2-PCB                           
031600                           SLDO-SDCA-WDK7-2-PCB                           
031600                           SLDO-SDCA-WDK7-3-PCB                           
031600                                                                          
031700                           SLDO-NDCA-USEA-PCB SLDO-NDCA-WDK7-PCB          
031800                           SLDO-NDCA-WDL6-PCB SLDO-NDCA-WDB6-PCB          
031900                                                                          
032000                           SLDO-RANS-XXKM-PCB SLDO-RANS-ARTM-PCB          
032100                           SLDO-RANS-ARTS-PCB                             
032200                                                                          
032300                           SLDO-CDCA-ARTM-PCB SLDO-CDCA-INLB-PCB          
032400                           SLDO-CDCA-WDB2-PCB SLDO-CDCA-WDC1-PCB          
032500                                                                          
032600                           SLDO-CLDC-WDB6-PCB                             
032700                                                                          
032800                           SLDO-ETA-ARTC-PCB  SLDO-ETA-WDK7-PCB           
032900                           SLDO-ETA-inlc-PCB  SLDO-ETA-LEVA-PCB           
033000                           SLDO-ETA-WDB6-PCB  SLDO-ETA-WDD9-PCB           
033100                                                                          
                                 SLDO-XDCA-USEA-PCB                             
042296                           SLDO-XDCA-WDB6-PCB SLDO-XDCA-WDK6-PCB          
042297                           SLDO-XDCA-WDK7-PCB SLDO-XDCA-WDK9-PCB          
042298                           SLDO-XDCA-WDL6-PCB SLDO-XDCA-WDQ4B-PCB         
042299                           SLDO-XDCA-WDQ2-PCB SLDO-XDCA-WDQ4-PCB          
042300                           SLDO-XDCA-WDR6-PCB                             
042295                           SLDO-XDCA-WDB6-2-PCB                           
042295                           SLDO-XDCA-WDK6-2-PCB                           
042295                           SLDO-XDCA-WDK7-2-PCB                           
042295                           SLDO-XDCA-WDK7-3-PCB                           
042295                                                                          
033200                           ETD-WDF1-PCB ETD-WDK6-PCB                      
033300                           ETD-WDK7-PCB ETD-XXKJ-PCB                      
033400                           ETD-WDD7-PCB ETD-BENA-PCB                      
033500                           ETD-WDB3-PCB ETD-WDK9-PCB ETD-WDA5-PCB         
033600                           ETD-WDA6J-PCB ETD-WDD9-PCB                     
033700                           ETD-WDL6-PCB ETD-WDB6-PCB                      
033800                           ETD-WDF2-PCB ETD-WDF2A-PCB                     
033900                                                                          
034000                           ETD-KREG-GMTA-PCB ETD-KREG-GMTB-PCB            
034100                           ETD-KREG-GMTC-PCB ETD-KREG-BETC-PCB            
034200                                                                          
034300                           ETD-KVAN-WDB2-PCB ETD-KVAN-WDC1-PCB            
034400                                                                          
034500                           ETD-AREG-WDK6-PCB ETD-AREG-WDK7-PCB            
034600                                                                          
034700                           ETD-SPAR-WDF8-PCB ETD-SPAR-WDF8A-PCB           
034800                           ETD-SPAR-WDK6-PCB                              
034900                                                                          
035000                           ETD-RANS-XXKM-PCB ETD-RANS-ARTM-PCB            
035100                           ETD-RANS-ARTS-PCB.                             
035200                                                                          
035300     PERFORM A-INIT                                                       
035400                                                                          
035500     PERFORM S01-READ-W91091                                              
035600                                                                          
035700     PERFORM UNTIL END-OF-W91091                                          
035800                                                                          
035900       PERFORM B-PREPARE-INPUT                                            
036000                                                                          
036100       PERFORM C-GET-SLDO                                                 
036200                                                                          
036300       PERFORM D-GET-ETD                                                  
036400                                                                          
036500       PERFORM S01-READ-W91091                                            
036600                                                                          
036700     END-PERFORM                                                          
036800                                                                          
036900     PERFORM Z-FINIT                                                      
037000                                                                          
037100     MOVE ZERO TO RETURN-CODE                                             
037200     GOBACK                                                               
037300     .                                                                    
037400     EJECT                                                                
037500 A-INIT SECTION.                                                          
037600                                                                          
037700     OPEN INPUT W91091                                                    
037800                                                                          
037900     OPEN OUTPUT SLDO                                                     
038000                 ETD                                                      
038100     SKIP2                                                                
038200     ACCEPT TODAYS-DATE  FROM DATE                                        
038300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
038400     .                                                                    
038500     EJECT                                                                
038600                                                                          
038700 B-PREPARE-INPUT SECTION.                                                 
038800     MOVE 'B-PREPARE-    ' TO CURRENT-SECTION                             
038900                                                                          
039000     INITIALIZE SLDO-W911SLDO ETD-W911ETD                                 
039100                                                                          
039200     MOVE IN-IDARTNR       TO SLDO-IDARTNR-IN                             
039300                              ETD-IDARTNR-IN                              
039400                                                                          
039500     MOVE IN-IDDISTR       TO SLDO-IDDISTR-IN                             
039600                              ETD-IDDISTR-IN                              
039700                                                                          
039800     MOVE IN-IDKUNDNR      TO SLDO-IDKUNDNR-IN                            
039900                              ETD-IDKUNDNR-IN                             
040000                                                                          
040100     MOVE 1                TO SLDO-KDORDKL-IN                             
040200                              ETD-KDORDKL-IN                              
040300                                                                          
040400     MOVE IN-KVBEART       TO SLDO-KVBEART-IN                             
040500                              ETD-KVBEART-IN                              
040600*To exclude insertion of log into WDR6 DB.                                
040600*Since this is a one time pgm , logging is not required                   
           MOVE 'XX'             TO SLDO-IDDC                                   
      *                                                                         
040700     MOVE ZERO             TO SLDO-KVAVBART                               
040800                              SLDO-TIREGDAT                               
040900                              SLDO-TIDISPIN                               
041000                              SLDO-KDORDBEK                               
041100                              ETD-KVAVBART                                
041200                              ETD-TIREGDAT                                
041300                              ETD-TIDISPIN                                
041400                              ETD-KDORDBEK                                
041500                                                                          
041600     MOVE SPACE            TO SLDO-FLTPO1                                 
041800                              SLDO-IDMFSMED                               
                                    ETD-IDDC                                    
041900                              ETD-IDMFSMED                                
042000                              ETD-FLTPO1                                  
042100                                                                          
042200     .                                                                    
042300 C-GET-SLDO SECTION.                                                      
042400     MOVE 'C-GET-SLDO    ' TO CURRENT-SECTION                             
042500                                                                          
042600     INITIALIZE SLDO-UT-AREA                                              
042601                                                                          
042602     CALL W911SLDO USING SLDO-W911SLDO SLDO-WDF1-PCB                      
042603                         SLDO-WDF2-PCB SLDO-WDF2A-PCB                     
042604                         SLDO-WDK7-PCB                                    
042605                         SLDO-WDK6-PCB SLDO-XXKJ-PCB                      
042606                         SLDO-WDD7-PCB SLDO-BENA-PCB                      
042607                         SLDO-WDB3-PCB SLDO-WDR6-PCB                      
042608                                                                          
042609                         SLDO-KREG-GMTA-PCB SLDO-KREG-GMTB-PCB            
042610                         SLDO-KREG-GMTC-PCB SLDO-KREG-BETC-PCB            
042620                                                                          
042630                         SLDO-KVAN-WDB2-PCB SLDO-KVAN-WDC1-PCB            
042640                                                                          
042650                         SLDO-AREG-WDK6-PCB SLDO-AREG-WDK7-PCB            
042660                                                                          
042670                         SLDO-DLEV-LEVF-PCB SLDO-DLEV-LEVG-PCB            
042680                         SLDO-DLEV-LEVA-PCB SLDO-DLEV-ARTS-PCB            
042690                         SLDO-DLEV-WDB6-PCB SLDO-DLEV-FILA-PCB            
042700                                                                          
042800                         SLDO-SPAR-WDF8-PCB SLDO-SPAR-WDF8A-PCB           
042900                         SLDO-SPAR-WDK6-PCB                               
043000                                                                          
043100                         SLDO-SDCA-ARTS-PCB SLDO-SDCA-WDB6-PCB            
043200                         SLDO-SDCA-WDK9-PCB SLDO-SDCA-WDR6-PCB            
043300                         SLDO-SDCA-WDK6-PCB SLDO-SDCA-WDQ4B-PCB           
043400                         SLDO-SDCA-WDQ2-PCB SLDO-SDCA-WDQ4-PCB            
043500                         SLDO-SDCA-WDB6-2-PCB                             
043500                         SLDO-SDCA-WDK6-2-PCB                             
043500                         SLDO-SDCA-WDK7-2-PCB                             
043500                         SLDO-SDCA-WDK7-3-PCB                             
043500                                                                          
043600                         SLDO-NDCA-USEA-PCB SLDO-NDCA-WDK7-PCB            
043700                         SLDO-NDCA-WDL6-PCB SLDO-NDCA-WDB6-PCB            
043800                                                                          
043900                         SLDO-RANS-XXKM-PCB SLDO-RANS-ARTM-PCB            
044000                         SLDO-RANS-ARTS-PCB                               
044100                                                                          
044200                         SLDO-CDCA-ARTM-PCB SLDO-CDCA-INLB-PCB            
044300                         SLDO-CDCA-WDB2-PCB SLDO-CDCA-WDC1-PCB            
044400                                                                          
044500                         SLDO-CLDC-WDB6-PCB                               
                                                                                
056020                         SLDO-ETA-ARTC-PCB  SLDO-ETA-WDK7-PCB             
056040                         SLDO-ETA-inlc-PCB  SLDO-ETA-LEVA-PCB             
056060                         SLDO-ETA-WDB6-PCB  SLDO-ETA-WDD9-PCB             
056070                                                                          
                               SLDO-XDCA-USEA-PCB                               
056120                         SLDO-XDCA-WDB6-PCB SLDO-XDCA-WDK6-PCB            
056121                         SLDO-XDCA-WDK7-PCB SLDO-XDCA-WDK9-PCB            
056122                         SLDO-XDCA-WDL6-PCB SLDO-XDCA-WDQ4B-PCB           
056123                         SLDO-XDCA-WDQ2-PCB SLDO-XDCA-WDQ4-PCB            
056124                         SLDO-XDCA-WDR6-PCB                               
056124                         SLDO-XDCA-WDB6-2-PCB                             
056124                         SLDO-XDCA-WDK6-2-PCB                             
056124                         SLDO-XDCA-WDK7-2-PCB                             
056124                         SLDO-XDCA-WDK7-3-PCB                             
056125                                                                          
044510*To initialize the working storage in subpgm for next call                
044600     CANCEL W911SLDO                                                      
044610*                                                                         
044700     MOVE SLDO-KVAVBART TO WS-KVAVBART                                    
044800     INSPECT WS-KVAVBART-X REPLACING LEADING ZERO BY SPACE                
044900     MOVE WS-KVAVBART-X TO SLDO-UT-KVAVBART                               
045000     IF SLDO-TIDISPIN > 0                                                 
045100        MOVE SLDO-TIDISPIN   TO WS-TIAAMMDD                               
045200        MOVE WS-TIAAMMDD-X   TO SLDO-UT-TIDISPIN                          
045300     END-IF                                                               
045400     IF SLDO-TIKLAR > 0                                                   
045500        MOVE SLDO-TIKLAR     TO WS-TIAAMMDD                               
045600        MOVE WS-TIAAMMDD-X   TO SLDO-UT-TIKLAR                            
045700     END-IF                                                               
045800                                                                          
045900     MOVE SLDO-IDARTNR-IN    TO SLDO-UT-IDARTNR                           
046000     MOVE SLDO-IDDISTR-IN    TO SLDO-UT-IDDISTR                           
046100     MOVE SLDO-IDKUNDNR-IN   TO SLDO-UT-IDKUNDNR                          
046200     MOVE SLDO-KVBEART-IN    TO SLDO-UT-KVBEART                           
046300     MOVE SLDO-IDDC          TO SLDO-UT-IDDC                              
046400     MOVE SLDO-KDORDBEK      TO SLDO-UT-KDORDBEK                          
046500     MOVE SLDO-TEORDBEK-ENG  TO SLDO-UT-TEORDBEK                          
046600                                                                          
046700     IF SLDO-TEORDBEK NOT = 'ARTIKEL OKÄND'                               
046800*RECORD WILL BE WRITTEN ONLY WHEN THE PART NUMBER IS FOUND                
046900        PERFORM S11-WRITE-SLDO                                            
047000     END-IF                                                               
047100*                                                                         
047200     .                                                                    
047300                                                                          
047400 D-GET-ETD SECTION.                                                       
047500     MOVE 'D-GET-ETD     ' TO CURRENT-SECTION                             
047600                                                                          
047700     INITIALIZE ETD-UT-AREA                                               
047800                                                                          
047900     CALL W911ETD USING ETD-W911ETD  ETD-WDF1-PCB ETD-WDK6-PCB            
048000                        ETD-WDK7-PCB ETD-XXKJ-PCB                         
048100                        ETD-WDD7-PCB ETD-BENA-PCB                         
048200                        ETD-WDB3-PCB ETD-WDK9-PCB ETD-WDA5-PCB            
048300                        ETD-WDA6J-PCB ETD-WDD9-PCB                        
048400                        ETD-WDL6-PCB ETD-WDB6-PCB                         
048500                        ETD-WDF2-PCB ETD-WDF2A-PCB                        
048600                                                                          
048700                        ETD-KREG-GMTA-PCB ETD-KREG-GMTB-PCB               
048800                        ETD-KREG-GMTC-PCB ETD-KREG-BETC-PCB               
048900                                                                          
049000                        ETD-KVAN-WDB2-PCB ETD-KVAN-WDC1-PCB               
049100                                                                          
049200                        ETD-AREG-WDK6-PCB ETD-AREG-WDK7-PCB               
049300                                                                          
049400                        ETD-SPAR-WDF8-PCB ETD-SPAR-WDF8A-PCB              
049500                        ETD-SPAR-WDK6-PCB                                 
049600                        ETD-RANS-XXKM-PCB ETD-RANS-ARTM-PCB               
049700                        ETD-RANS-ARTS-PCB                                 
049800                                                                          
049801                                                                          
049802*To initialize the working storage in subpgm for next call                
049810     CANCEL W911ETD                                                       
049820*                                                                         
049900     MOVE ETD-KVAVBART TO WS-KVAVBART                                     
050000     INSPECT WS-KVAVBART-X REPLACING LEADING ZERO BY SPACE                
050100     MOVE WS-KVAVBART-X TO ETD-UT-KVAVBART                                
050200     IF ETD-TIDISPIN > 0                                                  
050300        MOVE ETD-TIDISPIN   TO WS-TIAAMMDD                                
050400        MOVE WS-TIAAMMDD-X   TO ETD-UT-TIDISPIN                           
050500     END-IF                                                               
050600     IF ETD-TIKLAR > 0                                                    
050700        MOVE ETD-TIKLAR     TO WS-TIAAMMDD                                
050800        MOVE WS-TIAAMMDD-X   TO ETD-UT-TIKLAR                             
050900     END-IF                                                               
051000                                                                          
051100     MOVE ETD-IDARTNR-IN    TO ETD-UT-IDARTNR                             
051200     MOVE ETD-IDDISTR-IN    TO ETD-UT-IDDISTR                             
051300     MOVE ETD-IDKUNDNR-IN   TO ETD-UT-IDKUNDNR                            
051400     MOVE ETD-KVBEART-IN    TO ETD-UT-KVBEART                             
051500     MOVE ETD-IDDC          TO ETD-UT-IDDC                                
051600     MOVE ETD-KDORDBEK      TO ETD-UT-KDORDBEK                            
051700     MOVE ETD-TEORDBEK      TO ETD-UT-TEORDBEK                            
051800                                                                          
051801                                                                          
051802     IF ETD-TEORDBEK NOT = 'ARTIKEL OKÄND'                                
051803*RECORD WILL BE WRITTEN ONLY WHEN THE PART NUMBER IS FOUND                
051804        PERFORM S12-WRITE-ETD                                             
051805     END-IF                                                               
051806*                                                                         
051807     .                                                                    
051808 Z-FINIT SECTION.                                                         
051809     CLOSE W91091                                                         
051810           SLDO                                                           
051820           ETD                                                            
051830     SKIP2                                                                
051840     MOVE 'S' TO POSTSUM-OPKOD                                            
051850     CALL POSTSUM USING POSTSUM-PARM                                      
051860     .                                                                    
053900     EJECT                                                                
054000 S01-READ-W91091  SECTION.                                                
054100     SKIP2                                                                
054200     READ W91091 INTO IN-AREA                                             
054300     AT END                                                               
054400        SET END-OF-W91091 TO TRUE                                         
054500                                                                          
054600     NOT AT END                                                           
054700        MOVE 'W91091' TO POSTSUM-FDNAMN                                   
054800        MOVE 'W91091D1' TO POSTSUM-DDNAMN2                                
054900        CALL POSTSUM USING POSTSUM-PARM                                   
055000     END-READ                                                             
055100     .                                                                    
055200     EJECT                                                                
055300 S11-WRITE-SLDO SECTION.                                                  
055400     MOVE 'S11-WRITE     ' TO CURRENT-SECTION                             
055500                                                                          
055600     WRITE UT-SLDO-POST FROM SLDO-UT-AREA                                 
055700                                                                          
055800     MOVE 'SLDO' TO POSTSUM-FDNAMN                                        
055900     MOVE 'W91091D2' TO POSTSUM-DDNAMN2                                   
056000     CALL POSTSUM USING POSTSUM-PARM                                      
056100     .                                                                    
056200     EJECT                                                                
056300 S12-WRITE-ETD SECTION.                                                   
056400     MOVE 'S12-WRITE     ' TO CURRENT-SECTION                             
056500                                                                          
056600     WRITE UT-ETD-POST  FROM ETD-UT-AREA                                  
056700                                                                          
056800     MOVE 'ETD' TO POSTSUM-FDNAMN                                         
056900     MOVE 'W91091D3' TO POSTSUM-DDNAMN2                                   
057000     CALL POSTSUM USING POSTSUM-PARM                                      
057100     .                                                                    
057200     EJECT                                                                
057300 S99-ABEND SECTION.                                                       
057400                                                                          
057500     SKIP2                                                                
057600     MOVE 'S' TO POSTSUM-OPKOD                                            
057700     CALL POSTSUM USING POSTSUM-PARM                                      
057800     CALL ABEND USING RKOD-ABEND                                          
057900     .                                                                    
