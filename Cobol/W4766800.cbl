000100 PROCESS DYNAM                                                            
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W4766800.                                                
000500 AUTHOR.         MOGREN STINA.                                            
000600 DATE-WRITTEN.   02/04/22.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*    FUNKTION:                                                            
001000*        INVOICE INFO COD/IMP                                             
001100*                                                                         
001200*        PROGRAMMET STARTAS EFTER MPP 4634 (SAVE-IT)                      
001300*        VIA SOP-RUTIN W476S5                                             
001400*        RADPOSTER FRÅN BILLIT ÄR 'PARAMETER' IN                          
001500*        SKAPA POST/ER PER RAD PÅ W47668 FRÅN WDR4                        
001600*                                                                         
001700*        FIL INVOICE INFO COD/IMP  W47668 SKAPAS                          
001800*        FIL INVOICE INFO POLAND   W47670 SKAPAS                          
001900*        FIL SWISS CUSTOM INFO     W47656 SKAPAS                          
002000*        FIL REFILL INFO           W47664 SKAPAS                          
002100*        FIL RETURGODS INLEV       W47674 SKAPAS                          
002200*        FIL SATSER TILL INLEV     W4765H SKAPAS                          
002300*                                                                         
002400*        FIL FÖR UPPDAT WDK6,WDK7,WDL9,WDM7/8, WDR8                       
002500*                                  W47665 SKAPAS                          
002600*                                                                         
002700*        PROGRAMMET LÄSER/UPPDAT  R4  FAKTURAINFO                         
002800*                   LÄSER         E4, E6                                  
002900*                   LÄSER         B1, B2, K6, K7                          
003000*                   LÄSER         Q2, G2, R2, Q3                          
003100*                                                                         
003200*    ABENDKODER:                                                          
003300*        U0016 -  . . . .                                                 
003400*        U1000 -  . . . .                                                 
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     SKIP2                                                                
003800 INPUT-OUTPUT SECTION.                                                    
003900                                                                          
004000 FILE-CONTROL.                                                            
004100     SKIP2                                                                
004200*          --- FIL MED RAD-POSTER , W461RIK-RIP                           
004300     SELECT W47668                     ASSIGN TO W47668D1.                
004400     SKIP2                                                                
004500     SELECT W47670                     ASSIGN TO W47668D3.                
004600     EJECT                                                                
004700     SELECT W47656                     ASSIGN TO W47668D4.                
004800     EJECT                                                                
004900     SELECT W47664                     ASSIGN TO W47668D5.                
005000     EJECT                                                                
005100     SELECT W47674                     ASSIGN TO W47668D6.                
005200     EJECT                                                                
005300     SELECT W47665                     ASSIGN TO W47668D7.                
005400     EJECT                                                                
005500     SELECT W4765H                     ASSIGN TO W47668D8.                
005600     EJECT                                                                
005700     SELECT W47664R                    ASSIGN TO W47668D9.                
005800     EJECT                                                                
005900     SELECT W47664S                    ASSIGN TO W47668DA.                
006000     EJECT                                                                
006100 DATA DIVISION.                                                           
006200     SKIP2                                                                
006300 FILE SECTION.                                                            
006400     SKIP2                                                                
006500 FD  W47668                                                               
006600     RECORDING       F                                                    
006700     BLOCK CONTAINS  0.                                                   
006800                                                                          
006900*01  POST -COPY W4766601  -PRE  UT-  -L.                                  
007000     EJECT                                                                
007100                                                                          
007200 FD  W47670                                                               
007300     RECORDING       F                                                    
007400     BLOCK CONTAINS  0.                                                   
007500                                                                          
007600*01  POST -COPY W4767001  -PRE  UT3-  -L.                                 
007700     EJECT                                                                
007800 FD  W47656                                                               
007900     RECORDING       F                                                    
008000     BLOCK CONTAINS  0.                                                   
008100                                                                          
008200*01  POST -COPY W4754A01  -PRE  UT4-  -L.                                 
008300     EJECT                                                                
008400 FD  W47664                                                               
008500     RECORDING       V                                                    
008600     BLOCK CONTAINS  0.                                                   
008700                                                                          
008800*01  POST -COPY W4766401  -PRE  UT5-  -L.                                 
008900     EJECT                                                                
009000 FD  W47674                                                               
009100     RECORDING       F                                                    
009200     BLOCK CONTAINS  0.                                                   
009300                                                                          
009400*01  POST -COPY W6110501  -PRE  UT6-  -L.                                 
009500     EJECT                                                                
009600 FD  W47665                                                               
009700     RECORDING       F                                                    
009800     BLOCK CONTAINS  0.                                                   
009900                                                                          
010000*01  POST -COPY W4766501  -PRE  UT7-  -L.                                 
010100     EJECT                                                                
010200 FD  W4765H                                                               
010300     LABEL RECORD   STANDARD                                              
010400     RECORDING      F                                                     
010500     BLOCK CONTAINS 0.                                                    
010600                                                                          
010700*01  SAT-POST     -COPY W476SAT      -L.                                  
010800     EJECT                                                                
010900 FD  W47664R                                                              
011000     RECORDING       V                                                    
011100     BLOCK CONTAINS  0.                                                   
011200                                                                          
011300*01  POST -COPY W4766401  -PRE  UT9-  -L.                                 
011400     EJECT                                                                
011500 FD  W47664S                                                              
011600     RECORDING       V                                                    
011700     BLOCK CONTAINS  0.                                                   
011800                                                                          
011900*01  POST -COPY W4766401  -PRE  UTA-  -L.                                 
012000     EJECT                                                                
012100 WORKING-STORAGE SECTION.                                                 
012200                                                                          
012300 77  IDPGM                       PIC X(8)    VALUE 'W4766800'.            
012400 77  JA                          PIC X       VALUE 'J'.                   
012500 77  NEJ                         PIC X       VALUE 'N'.                   
012600                                                                          
012700 77  FIL68-SW                    PIC X       VALUE 'N'.                   
012800     88  FIL68                               VALUE 'J'.                   
012900 77  FIL66-SW                    PIC X       VALUE 'N'.                   
013000     88  FIL66                               VALUE 'J'.                   
013100 77  FIL70-SW                    PIC X       VALUE 'N'.                   
013200     88  FIL70                               VALUE 'J'.                   
013300 77  FIL56-SW                    PIC X       VALUE 'N'.                   
013400     88  FIL56                               VALUE 'J'.                   
013500 77  FIL64-SW                    PIC X       VALUE 'N'.                   
013600     88  FIL64                               VALUE 'J'.                   
013700 77  FIL64R-SW                   PIC X       VALUE 'N'.                   
013800     88  FIL64R                              VALUE 'J'.                   
013900 77  FIL64S-SW                   PIC X       VALUE 'N'.                   
014000     88  FIL64S                              VALUE 'J'.                   
014100 77  FIL74-SW                    PIC X       VALUE 'N'.                   
014200     88  FIL74                               VALUE 'J'.                   
014300 77  FIL5H-SW                    PIC X       VALUE 'N'.                   
014400     88  FIL5H                               VALUE 'J'.                   
014500                                                                          
014600 77  W-RIM-SW                    PIC X       VALUE 'N'.                   
014700     88  RIM-POST                            VALUE 'J'.                   
014800                                                                          
014900 77  W-RIM-US                    PIC X       VALUE 'N'.                   
015000     88  RIM-US                              VALUE 'J'.                   
015100                                                                          
015200 77  FIRST-POST-SW               PIC X.                                   
015300     88  FIRST-POST                          VALUE 'J'.                   
015400     88  EJ-FIRST-POST                       VALUE 'N'.                   
015500                                                                          
015600 77  TILLAGG-SW                  PIC X       VALUE 'N'.                   
015700     88  TILLAGG-JA                          VALUE 'J'.                   
015800     88  TILLAGG-NEJ                         VALUE 'N'.                   
015900                                                                          
016000                                                                          
016100 77  W-ANT                       PIC S9(3)   VALUE ZERO COMP-3.           
016200 77  W-POST                      PIC S9(1)   VALUE ZERO COMP-3.           
016300 77  W-POST-R                    PIC S9(1)   VALUE ZERO COMP-3.           
016400 77  W-POST-S                    PIC S9(1)   VALUE ZERO COMP-3.           
016500 01  W-RAKNARE                   PIC S9(5)   VALUE ZERO COMP-3.           
016600 77  IX                          PIC S9(5)   VALUE ZERO COMP-3.           
016700                                                                          
016800 01  ERRTEXT.                                                             
016900     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
017000     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
017100                                                                          
017200*    --- ARBETSFÄLT FÖR BERÄKNING AV DAT./TID                             
017300 77  W-TIAAMMDD                  PIC 9(6)    VALUE ZERO.                  
017400 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
017500                                                                          
017600 77  WS-IDMARKBO                 PIC X       VALUE SPACE.                 
017700 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
017800 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
017900                                                                          
018000 01  SPAR-DAFAKT                 PIC 9(8)    VALUE ZERO.                  
018100 01  FILLER                      REDEFINES SPAR-DAFAKT.                   
018200   03  SPAR-SEKEL                PIC 9(2).                                
018300   03  SPAR-AAMMDD               PIC 9(6).                                
018400 01  SPAR-TIFAKTID               PIC S9(7)   VALUE ZERO COMP-3.           
018500 01  SPAR-IDKOLLI                PIC S9(5)   VALUE ZERO COMP-3.           
018600 01  SPAR-IDARTNR                PIC S9(9)   VALUE ZERO COMP-3.           
018700*                                                                         
018800 01  KONSTANTER.                                                          
018900     03  GEN-IDSTATNR            PIC S9(9) COMP-3 VALUE 87089997.         
019000                                                                          
019100 01  W-VKORDBTO-ORDER-LB        PIC S9(8)V9(1) COMP-3 VALUE ZERO.         
019200 01  W-VKORDBTO-ORDER           PIC S9(8)V9(1) COMP-3 VALUE ZERO.         
019300 01  SPAR-PRFRAKT-LOC            PIC 9(7)V9(2)  VALUE ZERO.               
019400                                                                          
019500 01  XX-RIO2.                                                             
019600   03  XX-PRARTNTO         PIC S9(11)V9(2)  COMP-3 VALUE ZERO.            
019700   03  XX-PRARTBTO         PIC S9(11)V9(2)  COMP-3 VALUE ZERO.            
019800   03  X4-PRARTNTO         PIC S9(7)V9(2)  COMP-3 VALUE ZERO.             
019900   03  X4-PRARTBTO         PIC S9(7)V9(2)  COMP-3 VALUE ZERO.             
020000                                                                          
020100 01  WS-BEL                     PIC S9(11)V9(2) COMP-3 VALUE ZERO.        
020200 01  WS-BELJPY                  PIC S9(11)      COMP-3 VALUE ZERO.        
020300                                                                          
020400 01  WRAD-KDSOFT                PIC S9          VALUE ZERO COMP-3.        
020500                                                                          
020600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
020700 01  FILLER REDEFINES DAGENS-DATUM.                                       
020800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
020900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
021000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
021100     EJECT                                                                
021200                                                                          
021300 01  WS-IDDC-SEND                PIC X(2)    VALUE SPACE.                 
021400 01  WS-IDDC-REC                 PIC X(2)    VALUE SPACE.                 
021500 01  WS-IDCOM                    PIC S9(9)   VALUE ZERO COMP-3.           
021600 01  WS-PRAVCOST-OLD             PIC S9(7)V9(2) COMP-3.                   
021700 01 DB2-LASNING.                                                          
021800     03 FILLER                   PIC X(16)   VALUE                        
021900                                             'WS-DB2-SEKTION'.            
022000     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
022100                                                                          
022200                                                                          
022300 01 NYCKLAR-TP4TRAN.                                                      
022400     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
022500                                                                          
022600                                                                          
022700                                                                          
022800*01  -COPY   WWDCKONS                                                     
022900*01  -COPY   WWDCLAND                                                     
023000*01  -COPY   WWDC99                                                       
023100                                                                          
023200 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
023300*01  FILLER        -COPY WWDIST07   -RED  TEST-IDDISTR.                   
023400                                                                          
023500*01  FILLER        -COPY WWDIST19   -RED  TEST-IDDISTR.                   
023600                                                                          
023700*01  FILLER        -COPY WWDIST34   -RED  TEST-IDDISTR.                   
023800                                                                          
023900*01  FILLER        -COPY WWDIST35   -RED  TEST-IDDISTR.                   
024000                                                                          
024100*01  FILLER        -COPY WWDIST79   -RED  TEST-IDDISTR.                   
024200                                                                          
024300*01  FILLER        -COPY WWDIST92   -RED  TEST-IDDISTR.                   
024400                                                                          
024500*01  FILLER        -COPY WWDIS130   -RED  TEST-IDDISTR.                   
024600     SKIP2                                                                
024700                                                                          
024800 01  DYNAMISKA-SUBPROGRAM.                                                
024900*                                                                         
025000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
025100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
025200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
025300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
025400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
025500     03  W460DIS1                PIC X(8)    VALUE 'W460DIS1'.            
025600     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
025700     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
025800     03  W335COST                PIC X(8)    VALUE 'W335COST'.            
025900     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
026000     SKIP2                                                                
026100 01  PARM-AREA                   PIC X(80)   VALUE SPACE.                 
026200 01  FILLER                      REDEFINES PARM-AREA.                     
026300     03  WS-IDSHIPM              PIC 9(7).                                
026400                                                                          
026500 01  WS-DISP                     PIC 9(12)   VALUE ZERO.                  
026600 01  W-KEY-KDTULLVE              PIC S9(1)   VALUE ZERO COMP-3.           
026700                                                                          
026800                                                                          
026900 01  WRO-IDKUNDRF                PIC X(10) VALUE '00000     '.            
027000                                                                          
027100*    --- PARAMETRAR TILL ABEND                                            
027200                                                                          
027300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
027400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
027500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
027600     SKIP2                                                                
027700 01  FELTEXT.                                                             
027800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
027900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
028000 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
028100                                                                          
028200 01  WS-PRKURS               PIC S9(6)V9(5)  VALUE ZERO COMP-3.           
028300                                                                          
028400 01  WS2-NYCKLAR.                                                         
028500     03  WS2-IDFAKT              PIC S9(7)  VALUE ZERO  COMP-3.           
028600     03  WS2-IDDISTR             PIC S9(5)  VALUE ZERO  COMP-3.           
028700     03  WS2-IDKUNDNR            PIC S9(7)  VALUE ZERO  COMP-3.           
028800     03  WS2-IDORDNR7            PIC S9(7)  VALUE ZERO  COMP-3.           
028900     03  WS2-IDPRODNR            PIC S9(7)  VALUE ZERO  COMP-3.           
029000     03  WS2-IDKOLLI             PIC S9(5)  VALUE ZERO  COMP-3.           
029100                                                                          
029200*    --- PARAMETRAR TILL W930VAL                                          
029300*01  -COPY W930VAL                                                        
029400                                                                          
029500*    --- PARAMETRAR TILL W411EXCH                                         
029600*01  -COPY W411EXCH                                                       
029700                                                                          
029800*    --- PARAMETRAR TILL W335COST                                         
029900 01 FILLER                       PIC X(8)    VALUE 'W335COST'.            
030000*   -COPY W335COST                                                        
030100     EJECT                                                                
030200*    --- PARAMETRAR TILL W510CURR                                         
030300 01 FILLER                       PIC X(8)    VALUE 'W510CURR'.            
030400*   -COPY W510CURR                                                        
030500     EJECT                                                                
030600                                                                          
030700*    --- PARAMETRAR TILL SUBPROGRAM W460DIS1                              
030800*01  -COPY  W460DIS1                                                      
030900                                                                          
031000*    --- PARAMETRAR TILL SUBPROGRAM W335PRIS                              
031100*01  -COPY  W335PRIS                                                      
031200     EJECT                                                                
031300                                                                          
031400*    --- PARAMETRAR TILL POSTSUM                                          
031500*                                                                         
031600*01  -COPY W0005   -PRE  POSTSUM-                                         
031700     EJECT                                                                
031800*01  -COPY WDATAREA                                                       
031900*        PARAMETRAR TILL WWOMVAND                                         
032000*    -COPY WWOMVAND                                                       
032100     EJECT                                                                
032200 01  UT-AREA-START               PIC X(16)   VALUE                        
032300                                 'UT-AREA-START  '.                       
032400     SKIP2                                                                
032500                                                                          
032600*01   -COPY W4766601                                                      
032700*********  AREOR  FÖR  FIL68                                              
032800 01  FILLER                      PIC X(16) VALUE 'W461RIK'.               
032900*01  -COPY W461RIK1                                                       
033000*                                                                         
033100 01  FILLER                      PIC X(16) VALUE 'W461RIL'.               
033200*01  -COPY W461RILN                                                       
033300*                                                                         
033400 01  FILLER                      PIC X(16) VALUE 'W461RIM'.               
033500*01  -COPY W461RIM2                                                       
033600*                                                                         
033700 01  FILLER                      PIC X(16) VALUE 'W461RIN'.               
033800*01  -COPY W461RINN                                                       
033900*                                                                         
034000 01  FILLER                      PIC X(16) VALUE 'W461RIO'.               
034100*01  -COPY W461RIO2                                                       
034200*                                                                         
034300 01  FILLER                      PIC X(16) VALUE 'W461RIP'.               
034400*01  -COPY W461RIPN                                                       
034500                                                                          
034600*********  AREOR  FÖR  FIL70                                              
034700 01  UT3-AREA-START              PIC X(16)   VALUE                        
034800                                 'UT3-AREA-START  '.                      
034900                                                                          
035000*01   -COPY W4767001         -PRE PL-                                     
035100*********  AREOR  FÖR  FIL56                                              
035200 01  UT4-AREA-START              PIC X(16)   VALUE                        
035300                                 'UT4-AREA-START  '.                      
035400 01  UT4-AREA.                                                            
035500*    03  -COPY W4754A01      -PRE UT4-                                    
035600*                                                                         
035700*********  AREOR  FÖR  FIL64                                              
035800 01  UT5-AREA-START              PIC X(16)   VALUE                        
035900                                 'UT5-AREA-START  '.                      
036000 01  UT5-AREA.                                                            
036100*    03  -COPY W4766401                                                   
036200*01  -COPY W4766402                                                       
036300*01  -COPY W4766403                                                       
036400*                                                                         
036500*********  AREOR  FÖR  FIL74                                              
036600 01  UT6-AREA-START              PIC X(16)   VALUE                        
036700                                 'UT6-AREA-START  '.                      
036800 01  UT6-AREA.                                                            
036900*    03  -COPY W6110501 -PRE UT6-                                         
037000*                                                                         
037100*********  AREOR  FÖR  FIL65                                              
037200 01  UT7-AREA-START              PIC X(16)   VALUE                        
037300                                 'UT7-AREA-START  '.                      
037400 01  UT7-AREA.                                                            
037500*    03  -COPY W4766501 -PRE UT7-                                         
037600                                                                          
037700********** AREOR FÖR FIL5H                                                
037800                                                                          
037900 01  FILLER                      PIC X(16) VALUE 'UTAREA SATSER'.         
038000 01  SAT-UTAREA.                                                          
038100*  03  -COPY W476SAT      -PRE SAT-                                       
038200     EJECT                                                                
038300*********                                                                 
038400*********  AREOR  FÖR  FIL64R - KONV. VIPSMARKADER                        
038500 01  UT9-AREA-START              PIC X(16)   VALUE                        
038600                                 'UT9-AREA-START  '.                      
038700 01  UT9-AREA.                                                            
038800*    03  -COPY W4766401  -PRE R-                                          
038900*01  -COPY W4766402 -PRE R-                                               
039000*01  -COPY W4766403 -PRE R-                                               
039100*                                                                         
039200*********                                                                 
039300*********  AREOR  FÖR  FIL64S - KONV. VIPSMARKADER (NR. 2)                
039400 01  UTA-AREA-START              PIC X(16)   VALUE                        
039500                                 'UTA-AREA-START '.                       
039600 01  UTA-AREA.                                                            
039700*    03  -COPY W4766401  -PRE S-                                          
039800*01  -COPY W4766402 -PRE S-                                               
039900*01  -COPY W4766403 -PRE S-                                               
040000*                                                                         
040100 01  FILLER                      PIC X(16) VALUE 'BILL-W4760001'.         
040200                                                                          
040300 01  IN-AREA.                                                             
040400   03  -COPY W4760001                                                     
040500                                                                          
040600     EJECT                                                                
040700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
040800*                                                                         
040900     EJECT                                                                
041000                                                                          
041100 01  TEST-ARTIKEL                PIC 9(9)    COMP-3.                      
041200*01  FILLER  -COPY WWART04       -RED  TEST-ARTIKEL.                      
041300*01  FILLER  -COPY  WWBYT19      -RED  TEST-ARTIKEL.                      
041400     EJECT                                                                
041500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
041600     SKIP3                                                                
041700 01  NYCKLAR-TILL-DLI.                                                    
041800     03  W-WDGXKEY-MIN-X.                                                 
041900         05  W-IDFAKT-MIN        PIC S9(7)    VALUE ZERO COMP-3.          
042000         05  W-IDDISTR-MIN       PIC S9(5)    VALUE ZERO COMP-3.          
042100         05  W-IDKUNDNR-MIN      PIC S9(7)    VALUE ZERO COMP-3.          
042200         05  W-IDORDER-MIN       PIC S9(7)    VALUE ZERO COMP-3.          
042300         05  W-IDPRODNR-MIN      PIC S9(7)    VALUE ZERO COMP-3.          
042400         05  W-IDPURAD-MIN       PIC S9(5)    VALUE ZERO COMP-3.          
042500                                                                          
042600     03  W-WDGXKEY-MAX-X.                                                 
042700         05  W-IDFAKT-MAX        PIC S9(7)    VALUE ZERO COMP-3.          
042800         05  W-IDDISTR-MAX       PIC S9(5)    VALUE ZERO COMP-3.          
042900         05  W-IDKUNDNR-MAX      PIC S9(7)    VALUE ZERO COMP-3.          
043000         05  W-IDORDER-MAX       PIC S9(7)    VALUE ZERO COMP-3.          
043100         05  W-IDPRODNR-MAX      PIC S9(7)    VALUE ZERO COMP-3.          
043200         05  W-IDPURAD-MAX       PIC S9(5)    VALUE ZERO COMP-3.          
043300                                                                          
043400     03  W-IDSHIPM-X.                                                     
043500         05  W-IDSHIPM           PIC  9(7)   VALUE ZERO.                  
043600     03  W-WDE111KY-X.                                                    
043700         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
043800         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
043900     03  W-WDE121KY-X.                                                    
044000         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
044100         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
044200     03  W-IDPURAD-X.                                                     
044300         05  W-IDPURAD           PIC S9(5)   VALUE ZERO COMP-3.           
044400                                                                          
044500     03  W-WDE4BSEQ-X.                                                    
044600         05  W-IDPRODNR-E4       PIC S9(7)   VALUE ZERO COMP-3.           
044700         05  W-IDPURAD-E4        PIC S9(5)   VALUE ZERO COMP-3.           
044800     03  W-WDE421KY-X.                                                    
044900         05  W-IDPRODNR-E421     PIC S9(7)   VALUE ZERO COMP-3.           
045000         05  W-IDKOLLI-E421      PIC S9(5)   VALUE ZERO COMP-3.           
045100     03  W-WDE4ESEQ-X.                                                    
045200         05  W-IDPRODNR-ESEQ     PIC S9(7)   VALUE ZERO COMP-3.           
045300                                                                          
045400     03  W-IDPRODNR-E6-X.                                                 
045500         05  W-IDPRODNR-E6       PIC S9(7)   VALUE ZERO COMP-3.           
045600     03  W-IDKOLLI-E6-X.                                                  
045700         05  W-IDKOLLI-E6        PIC S9(5)   VALUE ZERO COMP-3.           
045800                                                                          
045900     03  W-IDORDER-X.                                                     
046000         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
046100     03  W-IDARTNR-X.                                                     
046200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
046300     03  W-IDARTNR-CORE-X.                                                
046400         05  W-IDARTNR-CORE      PIC S9(9)   VALUE ZERO COMP-3.           
046500     03   W-KDSEGKEY-X.                                                   
046600         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
046700     03   W-IDDC-X.                                                       
046800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
046900     03   W-IDLAND-X.                                                     
047000         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
047100     03   W-IDPARTNR-X.                                                   
047200         05  W-IDPARTNR          PIC X(9)    VALUE SPACE.                 
047300                                                                          
047400     03  W-IDGMT-X.                                                       
047500         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO COMP-3.           
047600         05  W-IDKUNDNR-B2       PIC S9(7)   VALUE ZERO COMP-3.           
047700     03  W-IDGMT-MIN-X.                                                   
047800         05  W-IDDISTR-MIN-B2    PIC S9(5)   VALUE ZERO COMP-3.           
047900         05  W-IDKUNDNR-MIN-B2   PIC S9(7)   VALUE ZERO COMP-3.           
048000     03  W-IDGMT-MAX-X.                                                   
048100         05  W-IDDISTR-MAX-B2    PIC S9(5)   VALUE ZERO COMP-3.           
048200         05  W-IDKUNDNR-MAX-B2   PIC S9(7)   VALUE ZERO COMP-3.           
048300                                                                          
048400     03  W-WDGXKEY-4491-X.                                                
048500         05  W-IDHTYP-4491       PIC X(4)    VALUE '4491'.                
048600         05  W-IDDC-4491         PIC X(2)    VALUE SPACE.                 
048700         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
048800                                                                          
048900     03  W-DALASTN-X.                                                     
049000         05  W-DALASTN           PIC  9(8)   VALUE ZERO.                  
049100     03  W-IDKOLLI-4-X.                                                   
049200         05  W-IDKOLLI-4         PIC S9(5)   VALUE ZERO COMP-3.           
049300     03  W-IDGMTREF-X.                                                    
049400         05  W-IDDISTR-4         PIC S9(5)   VALUE ZERO COMP-3.           
049500         05  W-IDKUNDNR-4        PIC S9(7)   VALUE ZERO COMP-3.           
049600         05  W-IDKUNDRF.                                                  
049700             07  W-IDORDNR7-4    PIC 9(7)    VALUE ZERO.                  
049800             07  FILLER          PIC X(3)    VALUE SPACE.                 
049900     03  W-IDLBBET-X.                                                     
050000         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
050100                                                                          
050200     03  W-WDGXKEY-4507-X.                                                
050300         05  W-IDHTYP-4507       PIC X(4)    VALUE '4507'.                
050400         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
050500     03  W-IDFAKT-4508-X.                                                 
050600         05  W-IDFAKT-4508       PIC S9(7)   VALUE ZERO COMP-3.           
050700     03  W-IDFAKT-4508-MIN-X.                                             
050800         05  W-IDFAKT-4508-MIN   PIC S9(7)   VALUE ZERO COMP-3.           
050900     03  W-IDFAKT-4508-MAX-X.                                             
051000         05  W-IDFAKT-4508-MAX   PIC S9(7)   VALUE ZERO COMP-3.           
051100     03  W-FLKLAR-X.                                                      
051200         05  W-FLKLAR            PIC X(1)    VALUE 'J'.                   
051300     03  W-KY4510-4510-X.                                                 
051400         05  W-IDPRODNR-4510     PIC S9(7)   VALUE ZERO COMP-3.           
051500         05  W-IDKOLLI-4510      PIC S9(5)   VALUE ZERO COMP-3.           
051600         05  W-IDPURAD-4510      PIC S9(5)   VALUE ZERO COMP-3.           
051700    03  W-IDDC-B6-X.                                                      
051800        05  W-IDDC-B6            PIC X(2)    VALUE SPACE.                 
051900*                                                                         
052000    03   W-WDQ301-KEY-X.                                                  
052100        05   W-WDQ301-IDORDER    PIC S9(7)   VALUE ZERO  COMP-3.          
052200        05   W-WDQ301-IDDC       PIC X(2).                                
052300        05   W-WDQ301-IDPRODNR   PIC S9(7)   VALUE ZERO  COMP-3.          
052400        05   W-WDQ301-IDPLKLST   PIC S9(3)   VALUE ZERO  COMP-3.          
052500     SKIP2                                                                
052600*    --- STATUS-KOD FRÅN IMS                                              
053000 01  STATUS-WS                   PIC XX.                                  
053100     88  SEGMENT-FINNS                       VALUE '  '.                  
053200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
053300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
053400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
053500     88  IMS-NOT-OK                          VALUE 'XD'.                  
053600     SKIP2                                                                
053700 01  GODK-STATUSKODER.                                                    
053800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
053900     SKIP3                                                                
054000 01  SSA1                        PIC X(96).                               
054100 01  SSA2                        PIC X(96).                               
054200 01  SSA3                        PIC X(96).                               
054300 01  SSA4                        PIC X(96).                               
054400     EJECT                                                                
054500*                            DB2 FUNKTIONSKODER                           
054600 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
054700       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
054800                                                                          
054900 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
055000 01  DB2-WS.                                                              
055100     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
055200         88  CURSOR-OK                       VALUE 000.                   
055300         88  RADER-FINNS                     VALUE 000.                   
055400         88  RADER-SAKNAS                    VALUE 100.                   
055500         88  ATKOMST-FEL                     VALUE 904.                   
055600     03  GODK-SQLCODEKODER.                                               
055700         05  GODK-SQLCODE OCCURS 5                                        
055800             INDEXED BY SQLCODE-IX PIC 9(3).                              
055900 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
056000     EJECT                                                                
056100*    --- IMS FUNKTIONSKODER                                               
056200*01  -COPY W0003                                                          
056300     EJECT                                                                
056400*    ---  DLI INPUT-OUTPUT AREA                                           
056500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE401'.         
056600 01  DLI-IO-WDE401.                                                       
056700*    03  -COPY WDE401                                                     
056800     EJECT                                                                
056900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE411'.         
057000 01  DLI-IO-WDE411.                                                       
057100*    03  -COPY WDE411                                                     
057200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE421'.         
057300 01  DLI-IO-WDE421.                                                       
057400*    03  -COPY WDE421                                                     
057500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE601'.         
057600 01  DLI-IO-WDE601.                                                       
057700*    03  -COPY WDE601                                                     
057800     EJECT                                                                
057900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE611'.         
058000 01  DLI-IO-WDE611.                                                       
058100*    03  -COPY WDE611                                                     
058200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE111'.         
058300 01  DLI-IO-WDE111.                                                       
058400*    03  -COPY WDE111                                                     
058500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE131'.         
058600 01  DLI-IO-WDE131.                                                       
058700*    03  -COPY WDE131                                                     
058800     EJECT                                                                
058900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE141'.         
059000 01  DLI-IO-WDE141.                                                       
059100*    03  -COPY WDE141                                                     
059200     EJECT                                                                
059300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE122'.         
059400 01  DLI-IO-WDE122.                                                       
059500*    03  -COPY WDE122                                                     
059600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK601'.         
059700 01  DLI-IO-WDK601.                                                       
059800*    03  -COPY WDK601                                                     
059900     SKIP2                                                                
060000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK611'.         
060100 01  DLI-IO-WDK611.                                                       
060200*    03  -COPY WDK611                                                     
060300     SKIP2                                                                
060400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
060500 01  DLI-IO-WDK711.                                                       
060600*    03  -COPY WDK711                                                     
060700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
060800 01  DLI-IO-WDK711-CORE.                                                  
060900*    03  -COPY WDK711    -PRE CORE-                                       
061000                                                                          
061100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK712'.         
061200 01  DLI-IO-WDK712.                                                       
061300*    03  -COPY WDK712                                                     
061400                                                                          
061500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDQ201'.         
061600 01  DLI-IO-WDQ201.                                                       
061700*    03  -COPY WDQ201                                                     
061800                                                                          
061900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB101'.         
062000 01  DLI-IO-WDB101.                                                       
062100*    03  -COPY WDB101                                                     
062200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB201'.         
062300 01  DLI-IO-WDB201.                                                       
062400*    03  -COPY WDB201                                                     
062500     EJECT                                                                
062600 01  FILLER                      PIC X(16)   VALUE 'WDGX-4494  '.         
062700 01  DLI-IO-4494.                                                         
062800     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
062900     03  WL449112 REDEFINES IO-AREA.                                      
063000*        05  -COPY WDGX4494                                               
063100 01  FILLER                      PIC X(16)   VALUE 'WDGX-4508  '.         
063200 01  DLI-IO-4508.                                                         
063300*    03  -COPY WDGX4508                                                   
063400 01  FILLER                      PIC X(16)   VALUE 'WDGX-4510  '.         
063500 01  DLI-IO-4510.                                                         
063600*    03  -COPY WDGX4510                                                   
063700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDB601'.        
063800 01  DLI-IO-AREA-B601.                                                    
063900*    03  -COPY WDB601                                                     
064000 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDQ301'.        
064100 01  DLI-IO-WDQ301.                                                       
064200*    03 -COPY WDQ301                                                      
064300     SKIP2                                                                
064400                                                                          
064500 01  IO-PCB                      PIC X.                                   
064600     EJECT                                                                
064700 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
064800                                                                          
064900*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
065000     EJECT                                                                
065100     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
065200     EJECT                                                                
065300 LINKAGE SECTION.                                                         
065400                                                                          
065500*01  -COPY W0009   -PRE MSG-                                              
065600                                                                          
065700                                                                          
065800*01  -COPY W0008  -PRE WDE4-                                              
065900     05  FILLER                  PIC X.                                   
066000*01  -COPY W0008  -PRE WDE4X-                                             
066100     05  FILLER                  PIC X.                                   
066200*01  -COPY W0008  -PRE WDE6-                                              
066300     05  FILLER                  PIC X.                                   
066400*01  -COPY W0008  -PRE WDB1-                                              
066500     05  FILLER                  PIC X.                                   
066600*01  -COPY W0008  -PRE WDB2-                                              
066700     05  FILLER                  PIC X.                                   
066800*01  -COPY W0008  -PRE WDG2-                                              
066900     05  FILLER                  PIC X.                                   
067000*01  -COPY W0008  -PRE WDK6-                                              
067100     05  FILLER                  PIC X.                                   
067200*01  -COPY W0008  -PRE WDK7-                                              
067300     05  FILLER                  PIC X.                                   
067400*01  -COPY W0008  -PRE 4494-                                              
067500     05  FILLER                  PIC X.                                   
067600*01  -COPY W0008  -PRE 4507-                                              
067700     05  FILLER                  PIC X.                                   
067800*01  -COPY W0008  -PRE WDQ2-                                              
067900     05  FILLER                  PIC X.                                   
068000*01  -COPY W0008  -PRE WDB6-                                              
068100     05  FILLER                  PIC X.                                   
068200 01  PRIS-WDK6-PCB               PIC X.                                   
068300 01  PRIS-WDK7-PCB               PIC X.                                   
068400 01  PRIS-WDB2-PCB               PIC X.                                   
068500 01  PRIS-WDB1-PCB               PIC X.                                   
068600 01  PRIS-WDC1-PCB               PIC X.                                   
068700 01  PRIS-WDC2-PCB               PIC X.                                   
068800 01  COST-WDK6-PCB               PIC X.                                   
068900 01  COST-WDK7-PCB               PIC X.                                   
069000 01  COST-WDF1-PCB               PIC X.                                   
069100 01  COST-9305-PCB               PIC X.                                   
069200 01  COST-WDK72-PCB              PIC X.                                   
069300 01  COST-WDB6-PCB               PIC X.                                   
069400 01  PRIS-COST-WDK6-PCB          PIC X.                                   
069500 01  PRIS-COST-WDK7-PCB          PIC X.                                   
069600 01  PRIS-COST-WDF1-PCB          PIC X.                                   
069700 01  PRIS-COST-9305-PCB          PIC X.                                   
069800 01  PRIS-COST-WDK72-PCB         PIC X.                                   
069900 01  PRIS-COST-WDB6-PCB          PIC X.                                   
070000*01  -COPY W0008  -PRE WDE1-                                              
070100     05  FILLER                  PIC X.                                   
070200*01  -COPY W0008  -PRE WDQ3-                                              
070300     05  FILLER                  PIC X.                                   
070400     EJECT                                                                
070500 PROCEDURE DIVISION  USING MSG-PCB                                        
070600                           WDE4-PCB                                       
070700                           WDE4X-PCB                                      
070800                           WDE6-PCB                                       
070900                           WDB1-PCB                                       
071000                           WDB2-PCB                                       
071100                           WDG2-PCB                                       
071200                           WDK6-PCB                                       
071300                           WDK7-PCB                                       
071400                           4494-PCB                                       
071500                           4507-PCB                                       
071600                           WDQ2-PCB                                       
071700                           WDB6-PCB                                       
071800                           PRIS-WDK6-PCB  PRIS-WDK7-PCB                   
071900                           PRIS-WDB2-PCB                                  
072000                           PRIS-WDB1-PCB                                  
072100                           PRIS-WDC1-PCB  PRIS-WDC2-PCB                   
072200                           COST-WDK6-PCB                                  
072300                           COST-WDK7-PCB                                  
072400                           COST-WDF1-PCB                                  
072500                           COST-9305-PCB                                  
072600                           COST-WDK72-PCB COST-WDB6-PCB                   
072700                           PRIS-COST-WDK6-PCB                             
072800                           PRIS-COST-WDK7-PCB                             
072900                           PRIS-COST-WDF1-PCB                             
073000                           PRIS-COST-9305-PCB                             
073100                           PRIS-COST-WDK72-PCB                            
073200                           PRIS-COST-WDB6-PCB                             
073300                           WDE1-PCB                                       
073400                           WDQ3-PCB.                                      
073500 MAIN SECTION.                                                            
073600     ENTRY 'DLITCBL' USING MSG-PCB                                        
073700                           WDE4-PCB                                       
073800                           WDE4X-PCB                                      
073900                           WDE6-PCB                                       
074000                           WDB1-PCB                                       
074100                           WDB2-PCB                                       
074200                           WDG2-PCB                                       
074300                           WDK6-PCB                                       
074400                           WDK7-PCB                                       
074500                           4494-PCB                                       
074600                           4507-PCB                                       
074700                           WDQ2-PCB                                       
074800                           WDB6-PCB                                       
074900                           PRIS-WDK6-PCB  PRIS-WDK7-PCB                   
075000                           PRIS-WDB2-PCB                                  
075100                           PRIS-WDB1-PCB                                  
075200                           PRIS-WDC1-PCB  PRIS-WDC2-PCB                   
075300                           COST-WDK6-PCB                                  
075400                           COST-WDK7-PCB                                  
075500                           COST-WDF1-PCB                                  
075600                           COST-9305-PCB                                  
075700                           COST-WDK72-PCB COST-WDB6-PCB                   
075800                           PRIS-COST-WDK6-PCB                             
075900                           PRIS-COST-WDK7-PCB                             
076000                           PRIS-COST-WDF1-PCB                             
076100                           PRIS-COST-9305-PCB                             
076200                           PRIS-COST-WDK72-PCB                            
076300                           PRIS-COST-WDB6-PCB                             
076400                           WDE1-PCB                                       
076500                           WDQ3-PCB.                                      
076600                                                                          
076700                                                                          
076800     PERFORM A-INIT                                                       
076900                                                                          
077000     PERFORM IMS-GU-WDGX4507                                              
077100                                                                          
077200     PERFORM IMS-GHNP-WDGX4508                                            
077300     IF SEGMENT-FINNS                                                     
077400      MOVE 4508-IDFAKT         TO W-IDFAKT-4508                           
077500      PERFORM IMS-GHNP-WDGX4510                                           
077600      IF 4510-IDPURAD < ZERO                                              
077700        MOVE ZERO   TO 4510-IDPURAD                                       
077800      END-IF                                                              
077900                                                                          
078000      PERFORM UNTIL (NOT SEGMENT-FINNS AND NOT SEGMENT-SLUT)              
078100                                                                          
078200       PERFORM S20-INFIL-TILL-BILL                                        
078300       PERFORM B-VILKA-UTFILER                                            
078400       PERFORM C-LAES-GRUNDDATA                                           
078500                                                                          
078600       PERFORM L-SKRIV-ARTIKEL-TULL                                       
078700                                                                          
078800       IF FIL68                                                           
078900          PERFORM F-SKAPA-POSTER                                          
079000       END-IF                                                             
079100       IF FIL70                                                           
079200          PERFORM H-SKAPA-POSTER                                          
079300       END-IF                                                             
079400       IF FIL56                                                           
079500          PERFORM I-SKAPA-POSTER                                          
079600       END-IF                                                             
079700                                                                          
079800       IF FIL64                                                           
079900         IF DIST35-JP-NDC-RETURNS                                         
080000           PERFORM N-SKAPA-POSTER                                         
080100         ELSE                                                             
080200           PERFORM J-SKAPA-POSTER                                         
080300         END-IF                                                           
080400       END-IF                                                             
080500                                                                          
080600       IF FIL74                                                           
080700          PERFORM K-SKAPA-POSTER                                          
080800       END-IF                                                             
080900       IF FIL5H                                                           
081000          PERFORM M-SKRIV-SATS-W4765H                                     
081100       END-IF                                                             
081200       IF FIL64R                                                          
081300          PERFORM R-SKAPA-POSTER                                          
081400       END-IF                                                             
081500       IF FIL64S                                                          
081600          PERFORM S-SKAPA-POSTER                                          
081700       END-IF                                                             
081800                                                                          
081900                                                                          
082000       PERFORM IMS-GHNP-WDGX4510                                          
082100       IF 4510-IDPURAD < ZERO                                             
082200         MOVE ZERO   TO 4510-IDPURAD                                      
082300       END-IF                                                             
082400       MOVE 4510-IDPRODNR       TO W-IDPRODNR-4510                        
082500       MOVE 4510-IDKOLLI        TO W-IDKOLLI-4510                         
082600       MOVE 4510-IDPURAD        TO W-IDPURAD-4510                         
082700       IF NOT SEGMENT-FINNS                                               
082800         PERFORM IMS-GHNP-WDGX4508-KVAL                                   
082900         PERFORM IMS-DLET-WDGX4508-4510                                   
083000                                                                          
083100         PERFORM IMS-GHNP-WDGX4508                                        
083200         IF SEGMENT-FINNS                                                 
083300           MOVE 4508-IDFAKT       TO W-IDFAKT-4508                        
083400           PERFORM IMS-GHNP-WDGX4510                                      
083500           IF 4510-IDPURAD < ZERO                                         
083600             MOVE ZERO   TO 4510-IDPURAD                                  
083700           END-IF                                                         
083800           MOVE 4510-IDPRODNR     TO W-IDPRODNR-4510                      
083900           MOVE 4510-IDKOLLI      TO W-IDKOLLI-4510                       
084000           MOVE 4510-IDPURAD      TO W-IDPURAD-4510                       
084100         END-IF                                                           
084200       END-IF                                                             
084300                                                                          
084400      END-PERFORM                                                         
084500     END-IF                                                               
084600                                                                          
084700     PERFORM Z-FINIT                                                      
084800                                                                          
084900     MOVE ZERO TO RETURN-CODE                                             
085000     GOBACK                                                               
085100     .                                                                    
085200     EJECT                                                                
085300 A-INIT SECTION.                                                          
085400     MOVE 'A-INIT'              TO WS-SEKTION                             
085500                                                                          
085600     OPEN OUTPUT W47668                                                   
085700                 W47670                                                   
085800                 W47656                                                   
085900                 W47664                                                   
086000                 W47664R                                                  
086100                 W47664S                                                  
086200                 W47674                                                   
086300                 W47665                                                   
086400                 W4765H                                                   
086500                                                                          
086600                                                                          
086700     ACCEPT DAGENS-DATUM         FROM DATE                                
086800     ACCEPT WS-TTMMSSTH          FROM TIME                                
086900     ACCEPT W-TIAAMMDD           FROM DATE                                
087000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
087100                                                                          
087200     MOVE SPACE                  TO BET-IDPARTNR                          
087300                                    001-IDPTYP                            
087400                                    R-001-IDPTYP                          
087500                                    S-001-IDPTYP                          
087600                                    RIM-IDTRPBOT                          
087700     MOVE ZERO                   TO OHUV-IDORDER                          
087800                                    GMT-IDDISTR                           
087900                                    GMT-IDKUNDNR                          
088000                                    001-IDFAKT                            
088100                                    001-IDSHIPM                           
088200                                    002-IDFAKT                            
088300                                    002-IDDISTR                           
088400                                    002-IDKUNDNR                          
088500                                    002-IDKOLLI                           
088600                                    003-IDFAKT                            
088700                                    R-001-IDFAKT                          
088800                                    R-001-IDSHIPM                         
088900                                    R-002-IDFAKT                          
089000                                    R-002-IDDISTR                         
089100                                    R-002-IDKUNDNR                        
089200                                    R-002-IDKOLLI                         
089300                                    R-003-IDFAKT                          
089400                                    S-001-IDFAKT                          
089500                                    S-001-IDSHIPM                         
089600                                    S-002-IDFAKT                          
089700                                    S-002-IDDISTR                         
089800                                    S-002-IDKUNDNR                        
089900                                    S-002-IDKOLLI                         
090000                                    S-003-IDFAKT                          
090100                                    KORD-IDORDER                          
090200                                    KORD-IDORDER                          
090300                                    RIM-IDORDNR                           
090400                                    RIM-IDTRPBON                          
090500                                                                          
090600     MOVE JA                     TO FIRST-POST-SW                         
090700                                                                          
090800     PERFORM S21-NOLLA-RIL                                                
090900                                                                          
091000     MOVE +1                     TO W-IDFAKT-4508-MIN                     
091100     MOVE +9999999               TO W-IDFAKT-4508-MAX                     
091200                                                                          
091300     MOVE LOW-VALUE              TO W-IDGMT-MIN-X                         
091400     MOVE HIGH-VALUE             TO W-IDGMT-MAX-X                         
091500     .                                                                    
091600     EJECT                                                                
091700 B-VILKA-UTFILER  SECTION.                                                
091800     MOVE 'B-VILKA-UTFILER'     TO WS-SEKTION                             
091900                                                                          
092000     MOVE NEJ                    TO FIL68-SW                              
092100                                    FIL66-SW                              
092200                                    FIL70-SW                              
092300                                    FIL56-SW                              
092400                                    FIL64-SW                              
092500                                    FIL64R-SW                             
092600                                    FIL64S-SW                             
092700                                    FIL74-SW                              
092800                                    FIL5H-SW                              
092900     MOVE BILL-IDDISTR           TO TEST-IDDISTR                          
093000                                    W-IDDISTR-B2                          
093100                                    W-IDDISTR-MIN-B2                      
093200                                    W-IDDISTR-MAX-B2                      
093300                                    DIS1-IDDISTR                          
093400     MOVE BILL-IDKUNDNR          TO W-IDKUNDNR-B2                         
093500     IF BILL-IDKUNDNR = 0 AND BILL-IDPURAD = 0                            
093600      IF GMT-IDDISTR  NOT = BILL-IDDISTR  OR                              
093700        GMT-IDKUNDNR NOT = BILL-IDKUNDNR                                  
093800        PERFORM IMS-GU-WDB201A                                            
093900                                                                          
094000       CALL W460DIS1 USING DIS1-W460DIS1                                  
094100      END-IF                                                              
094200     ELSE                                                                 
094300      IF GMT-IDDISTR  NOT = BILL-IDDISTR  OR                              
094400        GMT-IDKUNDNR NOT = BILL-IDKUNDNR                                  
094500        PERFORM IMS-GU-WDB201                                             
094600                                                                          
094700        CALL W460DIS1 USING DIS1-W460DIS1                                 
094800                                                                          
094900      END-IF                                                              
095000     END-IF                                                               
095100     IF BILL-IDDC NOT = DCS-IDDC                                          
095200        MOVE BILL-IDDC           TO W-IDDC-B6                             
095300        PERFORM IMS-GU-WDB601                                             
095400     END-IF                                                               
095500                                                                          
095600     IF 4510-FLCOD = JA                                                   
095700*                                                                         
095800*---   *INVOICE-INFO COD-IMP                                              
095900*       -VANLIGT FLÖDE (UTAN STUDS)  --> KDFAKSTA-EXP = 0                 
096000*                                                                         
096100*       -VOR-STUDS (DC11 TILL CN/IN) --> KDFAKSTA-EXP = 2                 
096200*        * OBS : -INGA VIPS TRANSAR EFTER FÖRSTA FAKTURAN                 
096300*                 DC11->DC71/67 (KDFAKSTA-EXP=1)                          
096400*                -UTAN BARA EFTER ANDRA FAKTURAN                          
096500*                 DCXX->DEALER (KDFAKSTA-EXP=2)                           
096600*---                                                                      
096700*                                                                         
096800       IF 4510-KDFAKSTA-EXP = 0 OR 2                                      
096900         MOVE JA                 TO FIL68-SW                              
097000       END-IF                                                             
097100     END-IF                                                               
097200                                                                          
097300     IF GMT-FLNC = JA OR DIS1-KDSVAR = JA                                 
097400           OR DIS130-NOAC                                                 
097500*          POSTER SKA SKRIVAS TILL VIPS                                   
097600       CONTINUE                                                           
097700     ELSE                                                                 
097800       MOVE NEJ                  TO FIL66-SW                              
097900       MOVE NEJ                  TO FIL68-SW                              
098000     END-IF                                                               
098100                                                                          
098200     IF (BILL-IDDISTR = 2878 )                                            
098300*       FAKTURAINFO TILL POLEN                                            
098400       MOVE JA                   TO FIL70-SW                              
098500     END-IF                                                               
098600*                                                                         
098700     IF DCS-SDC AND DCS-HOLLAND AND                                       
098800       (BILL-IDDISTR = 2078 OR 2070)                                      
098900*        FRÅN DC21  TILL  SCHWEIZ                                         
099000*        TRANS FÖR FAKTURAINFO TILL SCHWEIZ                               
099100       MOVE JA                   TO FIL56-SW                              
099200       IF WS-TTMMSSTH  >  15000000                                        
099300*         INGET MATERIAL SÄNDES EFTER 15:00                               
099400         MOVE NEJ                TO FIL56-SW                              
099500       END-IF                                                             
099600     END-IF                                                               
099700                                                                          
099800*    FIL64  TEST I C-LAES-GRUND                                           
099900                                                                          
100000*SKRIVER FIL W47674 FÖR RETURINFO TILL INLEVERANS.                        
100100*                                                                         
100200     IF DIST35-RETUR                                                      
100300       IF NOT DIST35-CDC-NL-RETUR AND NOT DIST35-JP-NDC-RETURNS           
100400        MOVE JA              TO FIL74-SW                                  
100500       END-IF                                                             
100600     END-IF                                                               
100700                                                                          
100800***********************                                                   
100900                                                                          
101000     MOVE NEJ                    TO  TILLAGG-SW                           
101100                                                                          
101200     IF  BILL-BEART  = 'FREIGHT' OR 'INSURANCE' OR                        
101300                       'LEGAL' OR                                         
101400                       'PACKING & HANDLING' OR                            
101500                       'SERVICE FEE' OR                                   
101600                       'REDUCTION'                                        
101700       MOVE JA                   TO TILLAGG-SW                            
101800     END-IF                                                               
101900                                                                          
102000     IF FIL74 AND TILLAGG-JA                                              
102100       MOVE NEJ                 TO FIL74-SW                               
102200     END-IF                                                               
102300                                                                          
102400*      TRANS FÖR SATS TILL INLEVERANS                                     
102500*      IF KORD-KDFAKTYP =  'N', KOLLAS SENARE                             
102600     IF DIST19-SATS                                                       
102700       MOVE JA                TO FIL5H-SW                                 
102800     END-IF                                                               
102900     .                                                                    
103000     EJECT                                                                
103100 C-LAES-GRUNDDATA SECTION.                                                
103200     MOVE 'C-LAES-GRUNDDATA'     TO WS-SEKTION                            
103300                                                                          
103400     MOVE BILL-IDSHIPM           TO W-IDSHIPM                             
103500     MOVE BILL-IDPRODNR          TO W-IDPRODNR                            
103600                                    W-IDPRODNR-E4                         
103700                                    W-IDPRODNR-E6                         
103800                                    W-IDPRODNR-E421                       
103900                                    W-IDPRODNR-ESEQ                       
104000     MOVE BILL-IDKOLLI           TO W-IDKOLLI-E6                          
104100                                    W-IDKOLLI                             
104200                                    W-IDKOLLI-E421                        
104300     MOVE BILL-IDDISTR           TO W-IDDISTR                             
104400     MOVE BILL-IDKUNDNR          TO W-IDKUNDNR                            
104500     MOVE BILL-IDPURAD           TO W-IDPURAD                             
104600                                    W-IDPURAD-E4                          
104700     IF BILL-IDPURAD = ZERO AND BILL-IDKOLLI = ZERO                       
104800       PERFORM IMS-GU-WDE401-ESEQ                                         
104900       PERFORM IMS-GNP-WDE411-ESEQ                                        
105000       PERFORM XX-FIXE411                                                 
105100       IF SEGMENT-FINNS                                                   
105200         PERFORM IMS-GU-WDE601                                            
105300         MOVE ZERO               TO KOLLI-VKORDBTO-KOLLI                  
105400       END-IF                                                             
105500     ELSE                                                                 
105600       PERFORM IMS-GU-WDE411-BSEQ                                         
105700       IF SEGMENT-FINNS                                                   
105800         PERFORM XX-FIXE411                                               
105900         PERFORM IMS-GNP-WDE401                                           
106000         IF SEGMENT-FINNS                                                 
106100           PERFORM IMS-GNP-WDE421                                         
106200           IF SEGMENT-FINNS                                               
106300            PERFORM IMS-GU-WDE601                                         
106400            IF SEGMENT-FINNS                                              
106500             PERFORM IMS-GNP-WDE611                                       
106600            END-IF                                                        
106700           END-IF                                                         
106800         END-IF                                                           
106900       END-IF                                                             
107000     END-IF                                                               
107100                                                                          
107200     IF KORD-KDFAKTYP = 'R' OR 'G' OR 'K'                                 
107300*         POSTER SKA SKRIVAS TILL VIPS /    FAKTURATYP                    
107400       CONTINUE                                                           
107500     ELSE                                                                 
107600       MOVE NEJ                  TO FIL66-SW                              
107700       MOVE NEJ                  TO FIL68-SW                              
107800     END-IF                                                               
107900                                                                          
108000     IF KORD-KDFAKTYP = 'N'                                               
108100*         POSTER SKA SKRIVAS TILL INLEV                                   
108200       CONTINUE                                                           
108300     ELSE                                                                 
108400       MOVE NEJ                  TO FIL5H-SW                              
108500     END-IF                                                               
108600                                                                          
108700******************************************************************        
108800*                                                                         
108900* KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                              
109000* GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                              
109100*                                                                         
109200******************************************************************        
109300                                                                          
109400     MOVE KORD-IDDISTR    TO W-TP4TRAN-IDDISTR                            
109500                                                                          
109600     PERFORM DB2-SELECT-TP4TRAN                                           
109700                                                                          
109800     IF ((DIST35-REFILL                                                   
109900          AND NOT(DIST35-REFILL-NA         OR                             
110000                 DIST35-CDC-NONVCC-REFILL  OR                             
110100                 DIST35-VCC-NONVCC-REFILL))                               
110200                  OR                                                      
110300                  DIST35-PACIFIC-TRANSFER OR                              
110400                  DIST35-REFILL-INOM-JP   OR                              
110500                  DIST35-REFILL-INOM-CN   OR                              
110600                  DIST35-CN-NDC-RETURNS   OR                              
110700                  RADER-FINNS)                                            
110800          AND                                                             
110900          KORD-KDFAKTYP = 'K'           OR                                
111000         (DIST35-REFILL-NA              OR                                
111100          DIST35-NA-TRANSFER            OR                                
111200          DIST35-NA-NDC-RETURNS         OR                                
111300          DIST35-REFILL-INOM-NA         OR                                
111400          DIST35-CDC-NONVCC-REFILL      OR                                
111500          DIST35-VCC-NONVCC-REFILL      OR                                
111600          DIST35-NONVCC-REFILL          OR                                
111700          DIST35-NONVCC-NONVCC-TRANSFER OR                                
111800          DIST35-REFILL-NA-JAP          OR                                
111900          DIST35-NONVCC-VCC-TRANSFER    OR                                
112000          DIST35-VCC-NONVCC-TRANSFER)                                     
112100          AND                                                             
112200          VORD-KDFAKTYP    NOT = 'P'                                      
112300       IF KORD-FLOVRLEV NOT = JA                                          
112400         MOVE JA                    TO FIL64-SW                           
112500         IF TILLAGG-JA                                                    
112600           MOVE NEJ                 TO FIL64-SW                           
112700         END-IF                                                           
112800       END-IF                                                             
112900     ELSE                                                                 
113000       IF DIST35-JP-NDC-RETURNS                                           
113100         MOVE JA                   TO FIL64-SW                            
113200         IF TILLAGG-JA                                                    
113300           MOVE NEJ                TO FIL64-SW                            
113400         END-IF                                                           
113500       END-IF                                                             
113600     END-IF                                                               
113700*                                                                         
113800*** FIX FÖR ATT SPARA FAKT.FÖR VIPSMARKADER FÖR FRAMTIDA KONV.            
113900*   KOREA   -DISTR 6121 (2019)                                            
114000*   TURKIET -DISTR 5810 (2021-2022)                                       
114100*   MALAYSIA-DISTR 5619 (2022)                                            
114200*   THAILAND-DISTR 6251 (2022-2023)                                       
114300*   TAIWAN  -DISTR 6200 (2023) - SAMTIDIGT MED THAILAND                   
114400*   MEXICO  -DISTR 6589 (2024)                                            
114500*   BRAZIL  -DISTR 7050 (2024) - SAMTIDIGT MED MEXICO                     
114510*   S.AFRICA-DISTR 3160 (2025) - SAMTIDIGT MED BRAZIL                     
114600*                                                                         
114700     IF KORD-IDDISTR = 3160                                               
114800       IF KORD-FLOVRLEV NOT = JA                                          
114900         MOVE JA                   TO FIL64R-SW                           
115000         IF TILLAGG-JA                                                    
115100           MOVE NEJ                TO FIL64R-SW                           
115200         END-IF                                                           
115300       END-IF                                                             
115400     END-IF                                                               
115500*                                                                         
115600     IF KORD-IDDISTR = 7050                                               
115700       IF KORD-FLOVRLEV NOT = JA                                          
115800         MOVE JA                   TO FIL64S-SW                           
115900         IF TILLAGG-JA                                                    
116000           MOVE NEJ                TO FIL64S-SW                           
116100         END-IF                                                           
116200       END-IF                                                             
116300     END-IF                                                               
116400*                                                                         
116500     .                                                                    
116600     EJECT                                                                
116700 F-SKAPA-POSTER SECTION.                                                  
116800     MOVE 'F-SKAPA-POSTER'         TO WS-SEKTION                          
116900      MOVE BILL-IDDISTR            TO TEST-IDDISTR                        
117000                                                                          
117100     IF BILL-IDDC NOT = DCS-IDDC                                          
117200        MOVE BILL-IDDC           TO W-IDDC-B6                             
117300        PERFORM IMS-GU-WDB601                                             
117400     END-IF                                                               
117500                                                                          
117600     IF BILL-IDFAKT NOT = WS2-IDFAKT                                      
117700        AND WS2-IDFAKT NOT = ZERO                                         
117800        PERFORM S22-SKRIV-RIL                                             
117900        PERFORM S21-NOLLA-RIL                                             
118000     END-IF                                                               
118100      IF BILL-IDFAKT   = WS2-IDFAKT   AND                                 
118200         BILL-IDDISTR  = WS2-IDDISTR  AND                                 
118300         BILL-IDKUNDNR = WS2-IDKUNDNR AND                                 
118400         BILL-IDORDNR7 = WS2-IDORDNR7                                     
118500         IF BILL-IDPRODNR = WS2-IDPRODNR AND                              
118600            BILL-IDKOLLI  = WS2-IDKOLLI                                   
118700            CONTINUE                                                      
118800         ELSE                                                             
118900            ADD KOLLI-VKORDBTO-KOLLI  TO W-VKORDBTO-ORDER                 
119000         END-IF                                                           
119100      ELSE                                                                
119200       IF RIM-IDORDNR NOT = ZERO                                          
119300        PERFORM FC-SKRIV-RIM-POST                                         
119400        MOVE ZERO                     TO W-VKORDBTO-ORDER                 
119500                                         SPAR-PRFRAKT-LOC                 
119600        MOVE NEJ                      TO W-RIM-SW                         
119700                                         W-RIM-US                         
119800       END-IF                                                             
119900      END-IF                                                              
120000     IF BILL-IDPURAD = ZERO AND BILL-IDKOLLI = ZERO                       
120100       PERFORM S10-SPARA-ID                                               
120200       PERFORM FB-FAKTURA-TILLAGG                                         
120300     ELSE                                                                 
120400      IF GMT-IDDISTR  NOT = BILL-IDDISTR  OR                              
120500        GMT-IDKUNDNR NOT = BILL-IDKUNDNR                                  
120600        PERFORM IMS-GU-WDB201                                             
120700                                                                          
120800      END-IF                                                              
120900       IF BET-IDPARTNR NOT = BILL-IDPARTNR                                
121000         MOVE BILL-IDDISTR       TO W-IDDISTR                             
121100         MOVE BILL-IDKUNDNR      TO W-IDKUNDNR                            
121200         MOVE BILL-IDPARTNR      TO W-IDPARTNR                            
121300         PERFORM IMS-GU-WDB101                                            
121400         IF SEGMENT-SAKNAS                                                
121500            MOVE SPACE           TO BET-KDBETVIL                          
121600                                    BET-IDMARKBO                          
121700         END-IF                                                           
121800         IF BET-IDFTG = 53                                                
121900           PERFORM IMS-GN-WDB101                                          
122000         END-IF                                                           
122100         MOVE BET-IDMARKBO       TO WS-IDMARKBO                           
122200       END-IF                                                             
122300       IF KORD-IDORDER NOT = OHUV-IDORDER                                 
122400         MOVE KORD-IDORDER       TO W-IDORDER                             
122500         PERFORM IMS-GU-WDQ201                                            
122600         IF SEGMENT-SAKNAS                                                
122700           MOVE SPACE            TO OHUV-FLOVRLEV                         
122800         END-IF                                                           
122900       END-IF                                                             
123000*       INFO W461RIK - RIP UPPDATERAS PÅ UTFIL                            
123100                                                                          
123200*              FAKTURAHUVUD TILL IMPORTÖR                                 
123300      MOVE 'RIK'                TO RIK-IDPTYP                             
123400      MOVE BILL-IDDC            TO RIK-IDDC                               
123500      MOVE BILL-IDDISTR         TO RIK-IDDISTR                            
123600      MOVE VORD-KDFAKTYP        TO RIK-KDFAKTYP                           
123700      MOVE BILL-IDFAKT          TO RIK-IDFAKT                             
123800      MOVE BILL-DAFINDOC        TO RIK-TIFAKT                             
123900      MOVE SPACE                TO RIK-IDFRASED                           
124000      IF DIST92-ITALY-SEP                                                 
124100         MOVE BILL-IDSHIPM      TO RIK-IDFRASED(1:7)                      
124200      END-IF                                                              
124300      IF DIST79-DEALER-PRICE                                              
124400        MOVE BILL-KDVALISO-FAKT TO RIK-KDVALISO                           
124500      ELSE                                                                
124600        MOVE SPACE              TO RIK-KDVALISO                           
124700      END-IF                                                              
124800      PERFORM S09-BYT-KDVALISO-TILL-KDVALUTA                              
124900      IF WS-PRKURS = ZERO                                                 
125000      OR CURR-KDVALISO-ROW NOT = BILL-KDVALISO-BET                        
125100         MOVE BILL-KDVALISO-BET  TO CURR-KDVALISO-ROW                     
125200         MOVE BILL-DAFINDOC(3:2) TO W-DATE-AAMM(1:2)                      
125300         MOVE BILL-DAFINDOC(5:2) TO W-DATE-AAMM(3:2)                      
125400         IF W-DATE-AAMM(3:2) = ZERO                                       
125500           MOVE DAGENS-DATUM-AAR     TO W-DATE-AAMM(1:2)                  
125600           MOVE DAGENS-DATUM-MAANAD  TO W-DATE-AAMM(3:2)                  
125700         END-IF                                                           
125800         PERFORM S01-HAMTA-VALUTAKURS                                     
125900      END-IF                                                              
126000       MOVE WS-PRKURS            TO RIK-PRKURS                            
126100                                    EXCH-PRKURS                           
126200       MOVE BILL-SUNTO-TOT       TO RIK-SUFKTBEL                          
126300       MOVE BILL-SUNTO-TOT       TO RIK-SUFKTUTL                          
126400       IF DIST79-DEALER-PRICE                                             
126500         MOVE BILL-SUNTO-TOT     TO RIK-SUFKTUTL                          
126600         MOVE 1.0                TO RIK-PRKURS                            
126700         IF BILL-KDVALISO-FAKT NOT = BILL-KDVALISO-BET                    
126800           MOVE BILL-PRKURS-FIKTIV TO RIK-PRKURS                          
126900                                      EXCH-PRKURS                         
127000           MOVE BILL-SUNTO-TOT   TO WS-BEL                                
127100           PERFORM S04-OMVANDLA-SEK-TILL-UTL                              
127200           MOVE WS-BEL           TO RIK-SUFKTUTL                          
127300         END-IF                                                           
127400       ELSE                                                               
127500**       IF WS-PRKURS NOT = 1.0                                           
127600         IF BILL-PRKURS-BET NOT = 1.0                                     
127700           MOVE BILL-SUNTO-TOT       TO WS-BEL                            
127800           PERFORM S04-OMVANDLA-SEK-TILL-UTL                              
127900           MOVE WS-BEL               TO RIK-SUFKTUTL                      
128000*FIX JAPAN                                                                
128100           IF BILL-KDVALISO-BET = 'JPY'                                   
128200             COMPUTE WS-BELJPY ROUNDED = WS-BEL * 1                       
128300             MOVE WS-BELJPY          TO RIK-SUFKTUTL                      
128400           END-IF                                                         
128500         END-IF                                                           
128600       END-IF                                                             
128700       IF OHUV-FLOVRLEV = JA                                              
128800         MOVE '01'               TO RIK-KDFAKNOT                          
128900       ELSE                                                               
129000         MOVE SPACE              TO RIK-KDFAKNOT                          
129100       END-IF                                                             
129200       MOVE SPACES               TO RJX-FILLER                            
129300*                                                                         
129400       MOVE 'RIK'                TO RJX-IDPTYP                            
129500       MOVE RIK-W461RIK1         TO RJX-FILLER                            
129600       MOVE BILL-IDFAKT          TO RJX-IDFAKT                            
129700       MOVE BILL-IDDISTR         TO RJX-IDDISTR                           
129800       MOVE ZERO                 TO RJX-IDKUNDNR                          
129900       MOVE ZERO                 TO RJX-IDORDER                           
130000       MOVE ZERO                 TO RJX-IDKUNDNR-S                        
130100       MOVE ZERO                 TO RJX-IDPRODNR                          
130200       MOVE ZERO                 TO RJX-IDKOLLI                           
130300       MOVE ZERO                 TO RJX-IDPURAD                           
130400       MOVE ZERO                 TO RJX-IDTRPBON                          
130500*                                                                         
130600       PERFORM S11-SKRIV-W47668                                           
130700       PERFORM S10-SPARA-ID                                               
130800                                                                          
130900*         EV MOMS                                                         
131000       MOVE BILL-SUVAT-FAKT        TO RIL-PRMOMS                          
131100                                                                          
131200       IF BILL-IDDC NOT = DCS-IDDC                                        
131300          MOVE BILL-IDDC           TO W-IDDC-B6                           
131400          PERFORM IMS-GU-WDB601                                           
131500       END-IF                                                             
131600*      IF (KORD-KDFAKTYP = 'R' OR 'K') AND                                
131700*        (SDC-NL                                                          
131800*        OR SDC-GB OR LDC-GB-2A OR SDC-IT OR SDC-ES OR SDC-AT)            
131900*        IF WS-PRKURS NOT = 1.0                                           
132000*          MOVE BILL-SUVAT-FAKT      TO WS-BEL                            
132100*          PERFORM S04-OMVANDLA-SEK-TILL-UTL                              
132200*          MOVE WS-BEL               TO RIL-PRMOMS                        
132300*        END-IF                                                           
132400*      END-IF                                                             
132500       IF DCS-SDC AND DCS-HOLLAND AND                                     
132600          DIST34-HOLLAND-SDC                                              
132700          MOVE ZERO                TO RIL-PRMOMS                          
132800       END-IF                                                             
132900       IF DCS-DDC AND DCS-GERMANY AND                                     
133000          DIST34-TYSKLAND-DDC                                             
133100          MOVE ZERO                TO RIL-PRMOMS                          
133200       END-IF                                                             
133300**     IF DDC-NO AND DIST34-NORGE-DDC                                     
133400**        MOVE ZERO                TO RIL-PRMOMS                          
133500**     END-IF                                                             
133600*                                                                         
133700       IF DCS-NDC-PF                                                      
133800         IF DCS-LAND-NON-VCC-OWNED                                        
133900           CONTINUE                                                       
134000         ELSE                                                             
134100           COMPUTE WS-BEL = BILL-SUVAT-FAKT / BILL-PRKURS-BET             
134200           IF BILL-KDVALISO-BET = 'JPY'                                   
134300             COMPUTE WS-BELJPY ROUNDED = WS-BEL * 1                       
134400             MOVE WS-BELJPY        TO RIL-PRMOMS                          
134500           END-IF                                                         
134600         END-IF                                                           
134700       END-IF                                                             
134800*              HUVUD  REFERENSER TILL IMPORTER                            
134900       MOVE 'RIM'                TO RIM-IDPTYP                            
135000       MOVE BILL-IDKUNDNR        TO RIM-IDKUNDNR                          
135100       MOVE ZERO                 TO RIM-IDORDNR                           
135200       MOVE BILL-IDORDNR7        TO RIM-IDORDNR                           
135300       MOVE BILL-IDPRODNR        TO RIM-IDPRODNR                          
135400       MOVE KORD-TIORDREG        TO RIM-TIORDREG                          
135500       MOVE KORD-KDORDKL         TO RIM-KDORDKL                           
135600       MOVE SPACE                TO RIM-BEVARREF                          
135700                                    RIM-BEKUNDRF                          
135800                                    RIM-KDREFNOT                          
135900       IF KORD-IDORDER NOT = OHUV-IDORDER                                 
136000         MOVE KORD-IDORDER         TO W-IDORDER                           
136100         PERFORM IMS-GU-WDQ201                                            
136200         IF SEGMENT-SAKNAS                                                
136300           MOVE SPACE            TO OHUV-BEVARREF                         
136400                                    OHUV-BEKUNDRF                         
136500           MOVE NEJ              TO OHUV-FLOVRLEV                         
136600         END-IF                                                           
136700                                                                          
136800       END-IF                                                             
136900       MOVE OHUV-BEKUNDRF        TO RIM-BEKUNDRF                          
137000       MOVE OHUV-BEVARREF        TO RIM-BEVARREF                          
137100       IF OHUV-FLOVRLEV = JA                                              
137200         MOVE '01'               TO RIM-KDREFNOT                          
137300       ELSE                                                               
137400         MOVE '00'               TO RIM-KDREFNOT                          
137500       END-IF                                                             
137600       MOVE OHUV-IDDEPT          TO RIM-IDDEPT                            
137700       MOVE OHUV-IDBILREG        TO RIM-IDBILREG                          
137800       MOVE OHUV-IDVIN           TO RIM-IDVIN                             
137900       MOVE OHUV-IDCISNR         TO RIM-IDCISNR                           
138000       MOVE VORD-KDFRAKT         TO RIM-KDFRAKT                           
138100                                                                          
138200       IF BILL-IDDC NOT = DCS-IDDC                                        
138300          MOVE BILL-IDDC           TO W-IDDC-B6                           
138400          PERFORM IMS-GU-WDB601                                           
138500       END-IF                                                             
138600       PERFORM S06-HAMTA-BOLLA-INFO                                       
138700       IF RIM-IDTRPBON = ZERO                                             
138800         MOVE 4494-IDTRPBOT      TO RIM-IDTRPBOT                          
138900         MOVE 4494-IDTRPBON      TO RIM-IDTRPBON                          
139000       END-IF                                                             
139100       MOVE ZERO                 TO RIM-PRFRAKT-LOC                       
139200       MOVE JA                   TO W-RIM-SW                              
139300       IF DCS-NDC-NA AND DCS-USA                                          
139400           AND (KORD-KDORDKL = 0 OR 1)                                    
139500           AND KOLLI-IDTRPTNR      = 901                                  
139600**         AND KOLLI-IDLBBET       = 'FEDXPO'                             
139700          MOVE JA                TO W-RIM-US                              
139800       END-IF                                                             
139900       IF DCS-NDC-NA                                                      
140000          PERFORM S50-HAMTA-USA-FRAKT                                     
140100       END-IF                                                             
140200*                                                                         
140300                                                                          
140400*             FAKTURA  -  KOLLI                                           
140500       MOVE 'RIN'                TO RIN-IDPTYP                            
140600       MOVE BILL-IDKUNDNR        TO RIN-IDKUNDNR                          
140700       MOVE ZERO                 TO RIN-IDORDNR                           
140800       MOVE BILL-IDORDNR7        TO RIN-IDORDNR                           
140900       MOVE BILL-IDPRODNR        TO RIN-IDPRODNR                          
141000       MOVE BILL-IDKOLLI         TO RIN-IDKOLLI                           
141100       MOVE KOLLI-VKORDBTO-KOLLI TO RIN-VKORDBTO-KOLLI                    
141200       ADD  KOLLI-VKORDBTO-KOLLI TO W-VKORDBTO-ORDER                      
141300       MOVE KOLLI-VLORDBTO-KOLLI TO RIN-VLORDBTO-KOLLI                    
141400       MOVE KOLLI-IDLBBET        TO RIN-IDLBBET                           
141500       MOVE KOLLI-KDEMBTYP       TO RIN-KDEMBTYP                          
141600*      MOVE KOLLI-IDSUPREF       TO RIN-IDFAKT-GNB                        
141700       MOVE SPACE                TO RIN-IDFAKT-GNB                        
141800       MOVE SPACE                TO RIN-FILLERX16                         
141900                                    RJX-FILLER                            
142000*                                                                         
142100       MOVE 'RIN'                TO RJX-IDPTYP                            
142200       MOVE RIN-W461RINN-CTX     TO RJX-FILLER                            
142300       MOVE BILL-IDFAKT          TO RJX-IDFAKT                            
142400       MOVE BILL-IDDISTR         TO RJX-IDDISTR                           
142500       MOVE BILL-IDKUNDNR        TO RJX-IDKUNDNR                          
142600       MOVE BILL-IDORDNR7        TO RJX-IDORDER                           
142700       MOVE ZERO                 TO RJX-IDKUNDNR-S                        
142800       MOVE BILL-IDPRODNR        TO RJX-IDPRODNR                          
142900       MOVE BILL-IDKOLLI         TO RJX-IDKOLLI                           
143000       MOVE ZERO                 TO RJX-IDPURAD                           
143100       MOVE ZERO                 TO RJX-IDTRPBON                          
143200*                                                                         
143300       PERFORM S11-SKRIV-W47668                                           
143400                                                                          
143500       IF BILL-BEART = 'FREIGHT'  OR  'INSURANCE' OR                      
143600         'PACKING & HANDLING' OR                                          
143700         'SERVICE FEE' OR                                                 
143800         'LEGAL' OR 'REDUCTION'                                           
143900         PERFORM FB-FAKTURA-TILLAGG                                       
144000       ELSE                                                               
144100         PERFORM FA-FAKTURA-RAD-INFO                                      
144200       END-IF                                                             
144300     END-IF                                                               
144400                                                                          
144500     .                                                                    
144600     EJECT                                                                
144700 FA-FAKTURA-RAD-INFO SECTION.                                             
144800     MOVE 'FA-FAKTURA-RAD-INFO'  TO WS-SEKTION                            
144900*                                                                         
145000       MOVE 'RIO'                TO RIO-IDPTYP                            
145100       MOVE ZERO                 TO RIO-IDORDNR                           
145200       MOVE BILL-IDORDNR7        TO RIO-IDORDNR                           
145300       MOVE ORAD-IDARTNR         TO RIO-IDARTNR                           
145400       MOVE ORAD-REKSIFFR        TO RIO-REKSIFFR                          
145500       MOVE ORAD-BERADREF        TO RIO-BERADREF                          
145600       MOVE ORAD-KVBEART         TO RIO-KVBEART                           
145700       MOVE KKOLLI-KVLEVART      TO RIO-KVLEVART                          
145800       IF ORAD-KVBEART = ZERO                                             
145900          MOVE ZERO              TO RIO-RESERVG                           
146000       ELSE                                                               
146100          COMPUTE RIO-RESERVG =  ( ORAD-KVLEVART /                        
146200                  ORAD-KVBEART ) * 100                                    
146300       END-IF                                                             
146400       IF ORAD-IDARTNR NOT = W-IDARTNR                                    
146500         MOVE ORAD-IDARTNR         TO W-IDARTNR                           
146600         PERFORM IMS-GU-WDK611                                            
146700       END-IF                                                             
146800*      MOVE CLAG-PRARTBTO-EXP    TO RIO-PRARTBTO-EXP                      
146900       MOVE CLAG-PRARTSTD        TO RIO-PRARTSTD                          
147000                                                                          
147100       SEARCH ALL DC-LAND                                                 
147200          AT END                                                          
147300             MOVE SPACE          TO W-IDLAND                              
147400          WHEN DCLAND-IDDC (DCLAND-IX) = DCS-IDDC                         
147500             MOVE DCLAND-IDLANDX2 (DCLAND-IX)                             
147600                                 TO W-IDLAND                              
147700       END-SEARCH                                                         
147800       PERFORM IMS-GU-WDK712                                              
147900       IF SEGMENT-FINNS                                                   
148000          MOVE LART-PRARTSJK     TO RIO-PRARTSJK                          
148100       ELSE                                                               
148200          MOVE CLAG-PRARTSJK     TO RIO-PRARTSJK                          
148300       END-IF                                                             
148400                                                                          
148500       PERFORM S07-W335PRIS                                               
148600       MOVE PRIS-PRARTBTO-MARK   TO RIO-PRARTBTO-EXP                      
148700       MOVE ORAD-PRARTNTO-LOC    TO RIO-PRARTNTO-LOC                      
148800       MOVE ORAD-PRARTBTO-LOC    TO RIO-PRARTBTO-LOC                      
148900       IF BILL-IDDC NOT = DCS-IDDC                                        
149000          MOVE BILL-IDDC           TO W-IDDC-B6                           
149100          PERFORM IMS-GU-WDB601                                           
149200       END-IF                                                             
149300       MOVE BILL-IDDC    TO WS-IDDC                                       
149400       MOVE BILL-IDDISTR TO TEST-IDDISTR                                  
149500       IF ((NDC-CN OR LDC-CN) AND DIS130-NOAC-KINA-C1) OR                 
149600           (NDC-IN            AND DIST07-INDIEN)       OR                 
149700           (NDC-KR            AND DIST07-KOREA)        OR                 
149800           (NDC-MY            AND DIST07-MALAYSIA)     OR                 
149900           (NDC-TR            AND DIST07-TURKEY)       OR                 
150000           (NDC-TH            AND DIST07-THAILAND)     OR                 
150100           (NDC-TW            AND DIST07-TAIWAN)       OR                 
150200           (NDC-MX            AND DIST07-MEXICO)       OR                 
150201           (NDC-BR            AND DIST07-BRAZIL)       OR                 
150210           (NDC-ZA            AND DIST07-S-AFRICA)                        
150300          PERFORM S08-W335PRIS                                            
150400          MOVE PRIS-PRARTNTO        TO RIO-PRARTNTO                       
150500       ELSE                                                               
150600          MOVE ORAD-PRARTNTO        TO RIO-PRARTNTO                       
150700       END-IF                                                             
150800       MOVE ORAD-KDVAT           TO RIO-KDVAT                             
150900       MOVE BILL-IDFKNGRP        TO RIO-IDFKNGRP                          
151000       MOVE ORAD-KDPRODSL        TO RIO-KDPRODSL                          
151100       MOVE ORAD-KDDSP           TO RIO-KDDSP                             
151200       MOVE CLAG-KDVVKL          TO RIO-KDVVKL                            
151300       MOVE ORAD-KDVRINFO        TO RIO-KDVRINFO                          
151400       MOVE ORAD-FLINVEST        TO RIO-FLINVEST                          
151500       MOVE ORAD-FLPRTILL        TO RIO-FLPRTILL                          
151600       IF ORAD-KDPRTYP = 'P'                                              
151700         MOVE 'M'                TO RIO-FLPRTILL                          
151800       END-IF                                                             
151900       MOVE ORAD-FLDIRLEV        TO RIO-FLDIRLEV                          
152000       IF ((DCS-CDC OR DCS-DDC) AND                                       
152100           (DIST07-USA-RETAILER OR DIST07-CAN-RETAILER))                  
152200           OR                                                             
152300          (DCS-NDC-NA)                                                    
152400          MOVE VORD-FLDIRLEV     TO RIO-FLDIRLEV                          
152500       END-IF                                                             
152600       MOVE BILL-KDARTRAB        TO RIO-KDRABATT                          
152700       MOVE ORAD-KDRAB           TO RIO-KDRAB                             
152800       MOVE BILL-IDDC            TO RIO-IDDC                              
152900       MOVE CLAG-KDPSLLOC        TO RIO-KDPSLLOC                          
153000       PERFORM S40-HAMTA-PRAVCOST                                         
153100       MOVE BILL-PRAVCOST        TO RIO-PRAVCOST                          
153200       MOVE BILL-PRAVCOST-CORE   TO RIO-PRAVCOST-CORE                     
153300       MOVE ORAD-IDBIL           TO RIO-IDBIL                             
153400*          (FAKT-BILLIT ..  PULS-PRISER)                                  
153500       MOVE ZERO                 TO RIO-PRARTNTO-LOC                      
153600       MOVE ZERO                 TO RIO-PRARTBTO-LOC                      
153700       MOVE ZERO                 TO RIO-RERAB                             
153800       MOVE SPACE                TO RIO-KDRAB                             
153900       MOVE SPACE                TO RIO-KDVAT                             
154000       MOVE ZERO                 TO RIO-PRARTSJK                          
154100       MOVE ZERO                 TO RIO-PRARTSTD                          
154200                                                                          
154300       IF ORAD-IDSYSTEM = 'SOFT'                                          
154400         MOVE ORAD-BEVOLREF      TO RIO-BEVOLREF                          
154500         MOVE ORAD-IDVIN         TO RIO-IDVIN                             
154600       ELSE                                                               
154700         MOVE SPACE              TO RIO-BEVOLREF                          
154800         MOVE SPACE              TO RIO-IDVIN                             
154900       END-IF                                                             
155000                                                                          
155100       IF DIST79-DEALER-PRICE                                             
155200         MOVE ORAD-PRARTNTO-LOC    TO RIO-PRARTNTO-LOC                    
155300         MOVE ORAD-PRARTBTO-LOC    TO RIO-PRARTBTO-LOC                    
155400         MOVE ORAD-RERAB           TO RIO-RERAB                           
155500         MOVE ORAD-KDRAB           TO RIO-KDRAB                           
155600         MOVE CLAG-PRARTSTD        TO RIO-PRARTSTD                        
155700*EÖ   -   NY SJÄLVKOST                                                    
155800         MOVE '11'                 TO COST-IDDC                           
155900         MOVE W-IDARTNR            TO COST-IDARTNR                        
156000         MOVE ZERO                 TO COST-PRARTBES-MON                   
156100                                      COST-PRARTBES-MONLOC                
156200                                      COST-PRARTSJK-MON                   
156300                                      COST-PRARTSJK-MONLOC                
156400         MOVE SPACE                TO COST-IDLEVNR                        
156500         MOVE BILL-DAFINDOC(5:2)   TO COST-TIMM                           
156600         IF COST-TIMM = ZERO                                              
156700          MOVE DAGENS-DATUM-MAANAD TO COST-TIMM                           
156800         END-IF                                                           
156900                                                                          
157000         CALL W335COST USING COST-W335COST COST-WDK6-PCB                  
157100                                           COST-WDK7-PCB                  
157200                                           COST-WDF1-PCB                  
157300                                           COST-9305-PCB                  
157400                                           COST-WDK72-PCB                 
157500                                           COST-WDB6-PCB                  
157600         MOVE COST-PRARTSJK-MON    TO RIO-PRARTSJK                        
157700*EÖ                                                                       
157800         MOVE ORAD-KDVAT           TO RIO-KDVAT                           
157900         IF RIO-PRARTNTO-LOC = ZERO                                       
158000           PERFORM XX-RIO                                                 
158100           MOVE XX-PRARTNTO        TO RIO-PRARTNTO-LOC                    
158200           IF RIO-PRARTBTO-LOC = ZERO                                     
158300            MOVE XX-PRARTBTO       TO RIO-PRARTBTO-LOC                    
158400           END-IF                                                         
158500         END-IF                                                           
158600       END-IF                                                             
158700*                                                                         
158800*      FETCH TRACKING NO. FOR NDC-MX                                      
158900       IF NDC-MX                                                          
159000*--      NYCKLAR TILL WDE1                                                
159100         MOVE BILL-IDSHIPM         TO W-IDSHIPM                           
159200         MOVE BILL-IDDISTR         TO W-IDDISTR                           
159300         MOVE BILL-IDKUNDNR        TO W-IDKUNDNR                          
159400         MOVE BILL-IDPURAD         TO W-IDPURAD                           
159500         MOVE BILL-IDPRODNR        TO W-IDPRODNR                          
159600         MOVE BILL-IDKOLLI         TO W-IDKOLLI                           
159700*                                                                         
159800         PERFORM IMS-GU-WDE131                                            
159900         PERFORM IMS-GNP-WDE141                                           
160000         IF SEGMENT-FINNS                                                 
160100           PERFORM UNTIL SEGMENT-SAKNAS                                   
160200             MOVE TLEV-IDTRACK(1:15)                                      
160300                                   TO RIO-IDTRACK                         
160400             MOVE TLEV-KVTRACK-LEV TO RIO-KVLEVART                        
160500             MOVE TLEV-DADATUM     TO RIO-DADATUM                         
160600*                                                                         
160700             MOVE 'RIO'            TO RJX-IDPTYP                          
160800             MOVE RIO-W461RIO2     TO RJX-FILLER                          
160900             MOVE BILL-IDFAKT      TO RJX-IDFAKT                          
161000             MOVE BILL-IDDISTR     TO RJX-IDDISTR                         
161100             MOVE BILL-IDKUNDNR    TO RJX-IDKUNDNR                        
161200             MOVE BILL-IDORDNR7    TO RJX-IDORDER                         
161300             MOVE ZERO             TO RJX-IDKUNDNR-S                      
161400             MOVE BILL-IDPRODNR    TO RJX-IDPRODNR                        
161500             MOVE BILL-IDKOLLI     TO RJX-IDKOLLI                         
161600             MOVE BILL-IDPURAD     TO RJX-IDPURAD                         
161700             MOVE ZERO             TO RJX-IDTRPBON                        
161800*                                                                         
161900             PERFORM S11-SKRIV-W47668                                     
162000             PERFORM IMS-GNP-WDE141                                       
162100           END-PERFORM                                                    
162200         ELSE                                                             
162300           MOVE SPACE              TO RIO-IDTRACK                         
162400           MOVE ZERO               TO RIO-DADATUM                         
162500           MOVE 'RIO'              TO RJX-IDPTYP                          
162600           MOVE RIO-W461RIO2       TO RJX-FILLER                          
162700           MOVE BILL-IDFAKT        TO RJX-IDFAKT                          
162800           MOVE BILL-IDDISTR       TO RJX-IDDISTR                         
162900           MOVE BILL-IDKUNDNR      TO RJX-IDKUNDNR                        
163000           MOVE BILL-IDORDNR7      TO RJX-IDORDER                         
163100           MOVE ZERO               TO RJX-IDKUNDNR-S                      
163200           MOVE BILL-IDPRODNR      TO RJX-IDPRODNR                        
163300           MOVE BILL-IDKOLLI       TO RJX-IDKOLLI                         
163400           MOVE BILL-IDPURAD       TO RJX-IDPURAD                         
163500           MOVE ZERO               TO RJX-IDTRPBON                        
163600*                                                                         
163700           PERFORM S11-SKRIV-W47668                                       
163800                                                                          
163900         END-IF                                                           
164000       ELSE                                                               
164100         MOVE SPACE                TO RIO-IDTRACK                         
164200         MOVE ZERO                 TO RIO-DADATUM                         
164300         MOVE 'RIO'                TO RJX-IDPTYP                          
164400         MOVE RIO-W461RIO2         TO RJX-FILLER                          
164500         MOVE BILL-IDFAKT          TO RJX-IDFAKT                          
164600         MOVE BILL-IDDISTR         TO RJX-IDDISTR                         
164700         MOVE BILL-IDKUNDNR        TO RJX-IDKUNDNR                        
164800         MOVE BILL-IDORDNR7        TO RJX-IDORDER                         
164900         MOVE ZERO                 TO RJX-IDKUNDNR-S                      
165000         MOVE BILL-IDPRODNR        TO RJX-IDPRODNR                        
165100         MOVE BILL-IDKOLLI         TO RJX-IDKOLLI                         
165200         MOVE BILL-IDPURAD         TO RJX-IDPURAD                         
165300         MOVE ZERO                 TO RJX-IDTRPBON                        
165400*                                                                         
165500         PERFORM S11-SKRIV-W47668                                         
165600       END-IF                                                             
165700*                                                                         
165800       IF ORAD-IDKUNDRF-RO  = SPACES OR                                   
165900         =  WRO-IDKUNDRF                                                  
166000         CONTINUE                                                         
166100       ELSE                                                               
166200         MOVE 'RIP'                TO RIP-IDPTYP                          
166300         MOVE ZERO                 TO RIP-IDRONR                          
166400         MOVE +0                   TO W-ANT                               
166500         INSPECT ORAD-IDKUNDRF-RO TALLYING W-ANT FOR CHARACTERS           
166600                 BEFORE INITIAL ' '                                       
166700         MOVE ORAD-IDKUNDRF-RO(1:W-ANT)  TO RIP-IDRONR                    
166800        IF RIP-IDRONR NOT = ZERO                                          
166900         MOVE KORD-TIORDREG        TO RIP-TIORDREG                        
167000         MOVE ORAD-BEVOLREF        TO RIP-BEVOLREF                        
167100         MOVE ORAD-KDORDKL         TO RIP-KDORDKL                         
167200         IF ORAD-TIRODAT = ZERO                                           
167300           MOVE JA                 TO RIP-FLIHOP                          
167400         ELSE                                                             
167500           MOVE NEJ                TO RIP-FLIHOP                          
167600         END-IF                                                           
167700         MOVE SPACE                TO RIP-FILLERX52                       
167800*                                                                         
167900         MOVE 'RIP'                TO RJX-IDPTYP                          
168000         MOVE RIP-W461RIPN-CTX     TO RJX-FILLER                          
168100         MOVE BILL-IDFAKT          TO RJX-IDFAKT                          
168200         MOVE BILL-IDDISTR         TO RJX-IDDISTR                         
168300         MOVE BILL-IDKUNDNR        TO RJX-IDKUNDNR                        
168400         MOVE BILL-IDORDNR7        TO RJX-IDORDER                         
168500         MOVE ZERO                 TO RJX-IDKUNDNR-S                      
168600         MOVE BILL-IDPRODNR        TO RJX-IDPRODNR                        
168700         MOVE BILL-IDKOLLI         TO RJX-IDKOLLI                         
168800         MOVE BILL-IDPURAD         TO RJX-IDPURAD                         
168900         MOVE ZERO                 TO RJX-IDTRPBON                        
169000*                                                                         
169100         PERFORM S11-SKRIV-W47668                                         
169200        END-IF                                                            
169300       END-IF                                                             
169400     .                                                                    
169500     EJECT                                                                
169600 XX-RIO  SECTION.                                                         
169700     COMPUTE XX-PRARTNTO = BILL-SUNTO-LINE /                              
169800                           KKOLLI-KVLEVART                                
169900     COMPUTE XX-PRARTBTO = BILL-SUBTO-LINE /                              
170000                           KKOLLI-KVLEVART                                
170100     .                                                                    
170200     EJECT                                                                
170300 FB-FAKTURA-TILLAGG  SECTION.                                             
170400     MOVE 'FB-FAKTURA-TILLAGG'    TO WS-SEKTION                           
170500                                                                          
170600     MOVE BILL-IDDISTR            TO TEST-IDDISTR                         
170700     IF DIST35-CDC-AU-REFILL OR DIST07-AUSTRALIEN                         
170800*      SKALL EJ HA T/A I VIPS-FILEN                                       
170900       CONTINUE                                                           
171000     ELSE                                                                 
171100      IF BILL-BEART = 'FREIGHT'                                           
171200       MOVE BILL-SUNTO-LINE       TO RIL-PRFRAKT                          
171300                                     SPAR-PRFRAKT-LOC                     
171400      END-IF                                                              
171500      IF BILL-BEART = 'INSURANCE'                                         
171600       MOVE BILL-SUNTO-LINE       TO RIL-PRFOERS                          
171700      END-IF                                                              
171800      IF BILL-BEART = 'PACKING & HANDLING'                                
171900       MOVE BILL-SUNTO-LINE       TO RIL-PREMBHNT                         
172000      END-IF                                                              
172100      IF BILL-BEART = 'SERVICE FEE'                                       
172200       MOVE BILL-SUNTO-LINE       TO RIL-PREMBHNT                         
172300      END-IF                                                              
172400      IF BILL-BEART = 'LEGAL'                                             
172500       MOVE BILL-SUNTO-LINE       TO RIL-PRLEGKST                         
172600      END-IF                                                              
172700      IF BILL-BEART = 'REDUCTION'                                         
172800       MOVE BILL-SUNTO-LINE       TO RIL-PRAVDRAG                         
172900      END-IF                                                              
173000     END-IF                                                               
173100                                                                          
173200*                                                                         
173300     .                                                                    
173400     EJECT                                                                
173500 FC-SKRIV-RIM-POST  SECTION.                                              
173600     MOVE 'FC-SKRIV-RIM-POST'     TO WS-SEKTION                           
173700                                                                          
173800     IF WS2-IDDISTR NOT = ZERO                                            
173900       IF    RIM-US                                                       
174000         IF SPAR-PRFRAKT-LOC > ZERO                                       
174100           MOVE SPAR-PRFRAKT-LOC  TO RIM-PRFRAKT-LOC                      
174200         ELSE                                                             
174300           PERFORM FCA-BERAKNA-PRFRAKT-LOC                                
174400         END-IF                                                           
174500       ELSE                                                               
174600         MOVE SPAR-PRFRAKT-LOC    TO RIM-PRFRAKT-LOC                      
174700       END-IF                                                             
174800*                                                                         
174900       MOVE 'RIM'                TO RJX-IDPTYP                            
175000       MOVE RIM-W461RIM2-CTX     TO RJX-FILLER                            
175100       MOVE WS2-IDFAKT           TO RJX-IDFAKT                            
175200       MOVE WS2-IDDISTR          TO RJX-IDDISTR                           
175300       MOVE ZERO                 TO RJX-IDKUNDNR                          
175400       MOVE WS2-IDORDNR7         TO RJX-IDORDER                           
175500       MOVE WS2-IDKUNDNR         TO RJX-IDKUNDNR-S                        
175600       MOVE ZERO                 TO RJX-IDPRODNR                          
175700       MOVE ZERO                 TO RJX-IDKOLLI                           
175800       MOVE ZERO                 TO RJX-IDPURAD                           
175900       MOVE RIM-IDTRPBON         TO RJX-IDTRPBON                          
176000*                                                                         
176100       PERFORM S11-SKRIV-W47668                                           
176200       MOVE ZERO                 TO RIM-IDTRPBON                          
176300                                    RIM-IDKUNDNR                          
176400                                    RIM-IDORDNR                           
176500                                    RIM-IDPRODNR                          
176600                                    RIM-TIORDREG                          
176700                                    RIM-KDORDKL                           
176800                                    RIM-KDFRAKT                           
176900                                    RIM-PRFRAKT-LOC                       
177000                                    RIM-IDDEPT                            
177100       MOVE SPACE                TO RIM-BEKUNDRF                          
177200                                    RIM-BEVARREF                          
177300                                    RIM-KDREFNOT                          
177400                                    RIM-IDTRPBOT                          
177500                                    RIM-IDBILREG                          
177600                                    RIM-IDVIN                             
177700                                    RIM-IDCISNR                           
177800     END-IF                                                               
177900                                                                          
178000     MOVE NEJ                    TO W-RIM-SW                              
178100     .                                                                    
178200     EJECT                                                                
178300 FCA-BERAKNA-PRFRAKT-LOC  SECTION.                                        
178400     MOVE 'FCA-BERAKNA-PRFRAKT-LOC' TO WS-SEKTION                         
178500                                                                          
178600     MOVE ZERO                      TO RIM-PRFRAKT-LOC                    
178700     COMPUTE W-VKORDBTO-ORDER-LB ROUNDED =                                
178800             W-VKORDBTO-ORDER   *                                         
178900             CONV-KG-TO-LB                                                
179000                                                                          
179100     EVALUATE W-VKORDBTO-ORDER-LB                                         
179200       WHEN  0.1 THRU  2.9                                                
179300         MOVE 8.50                  TO RIM-PRFRAKT-LOC                    
179400       WHEN  3.0 THRU  5.9                                                
179500         MOVE 11.15                 TO RIM-PRFRAKT-LOC                    
179600       WHEN  6.0 THRU  9.9                                                
179700         MOVE 15.00                 TO RIM-PRFRAKT-LOC                    
179800       WHEN 10.0 THRU 15.9                                                
179900         MOVE 20.00                 TO RIM-PRFRAKT-LOC                    
180000       WHEN 16.0 THRU 20.9                                                
180100         MOVE 24.15                 TO RIM-PRFRAKT-LOC                    
180200       WHEN 21.0 THRU 25.9                                                
180300         MOVE 28.40                 TO RIM-PRFRAKT-LOC                    
180400       WHEN 26.0 THRU 30.9                                                
180500         MOVE 32.70                 TO RIM-PRFRAKT-LOC                    
180600       WHEN 31.0 THRU 40.9                                                
180700         MOVE 40.50                 TO RIM-PRFRAKT-LOC                    
180800       WHEN 41.0 THRU 50.9                                                
180900         MOVE 47.75                 TO RIM-PRFRAKT-LOC                    
181000       WHEN 51.0 THRU 60.9                                                
181100         MOVE 56.10                 TO RIM-PRFRAKT-LOC                    
181200       WHEN 61.0 THRU 70.9                                                
181300         MOVE 66.20                 TO RIM-PRFRAKT-LOC                    
181400       WHEN 71.0 THRU 80.9                                                
181500         MOVE 76.40                 TO RIM-PRFRAKT-LOC                    
181600       WHEN 81.0 THRU 90.9                                                
181700         MOVE 86.35                 TO RIM-PRFRAKT-LOC                    
181800       WHEN 91.0 THRU 99.9                                                
181900         MOVE 95.05                 TO RIM-PRFRAKT-LOC                    
182000       WHEN 100.0 THRU 110.9                                              
182100         MOVE 105.00                TO RIM-PRFRAKT-LOC                    
182200       WHEN 111.0 THRU 120.9                                              
182300         MOVE 114.50                TO RIM-PRFRAKT-LOC                    
182400       WHEN 121.0 THRU 130.9                                              
182500         MOVE 124.00                TO RIM-PRFRAKT-LOC                    
182600       WHEN 131.0 THRU 140.9                                              
182700         MOVE 133.60                TO RIM-PRFRAKT-LOC                    
182800       WHEN 141.0 THRU 150.9                                              
182900         MOVE 143.15                TO RIM-PRFRAKT-LOC                    
183000       WHEN OTHER                                                         
183100         MOVE SPAR-PRFRAKT-LOC      TO RIM-PRFRAKT-LOC                    
183200     END-EVALUATE                                                         
183300                                                                          
183400     .                                                                    
183500     EJECT                                                                
183600 H-SKAPA-POSTER  SECTION.                                                 
183700*    SKAPA POSTER FÖR INVOICE INFO POLEN                                  
183800     MOVE 'H-SKAPA-POSTER'      TO WS-SEKTION                             
183900                                                                          
184000     MOVE BILL-IDFAKT           TO PL-BILL-IDFAKT                         
184100     MOVE BILL-IDDISTR          TO PL-BILL-IDDISTR                        
184200     MOVE BILL-IDKUNDNR         TO PL-BILL-IDKUNDNR                       
184300     MOVE VORD-KDFAKTYP         TO PL-BILL-KDFAKTYP                       
184400     MOVE ORAD-IDARTNR          TO PL-BILL-IDARTNR                        
184500     MOVE BILL-BEART            TO PL-BILL-BEART                          
184600     MOVE BILL-IDKOLLI          TO PL-BILL-IDKOLLI                        
184700     MOVE KORD-IDKUNDRF         TO PL-BILL-IDKUNDRF                       
184800     MOVE ORAD-KDARTURS         TO PL-BILL-KDARTURS                       
184900     MOVE KKOLLI-KVLEVART       TO PL-BILL-KVLEVART                       
185000     MOVE ORAD-PRARTNTO         TO PL-BILL-PRARTNTO                       
185100     IF DIST79-DEALER-PRICE                                               
185200       MOVE ORAD-PRARTNTO-LOC   TO PL-BILL-PRARTNTO                       
185300       IF ORAD-PRARTNTO-LOC = ZERO                                        
185400         MOVE ORAD-PRARTNTO-LOCPREL TO PL-BILL-PRARTNTO                   
185500       END-IF                                                             
185600     ELSE                                                                 
185700       IF DIST79-ECOM-PRICE                                               
185800         MOVE ORAD-PRARTNTO-LOC TO PL-BILL-PRARTNTO                       
185900       END-IF                                                             
186000     END-IF                                                               
186100     MOVE BILL-KDVALISO-FAKT    TO PL-BILL-KDVALISO                       
186200     MOVE BILL-VKARTNTO         TO PL-BILL-VKARTNTO                       
186300     MOVE VORD-SUORDV           TO PL-BILL-SUORDV-FAKT                    
186400     IF DIST79-DEALER-PRICE                                               
186500       MOVE VORD-SUORDV-LOC     TO PL-BILL-SUORDV-FAKT                    
186600       IF VORD-SUORDV-LOCPREL NOT = ZERO                                  
186700         ADD VORD-SUORDV-LOCPREL TO PL-BILL-SUORDV-FAKT                   
186800       END-IF                                                             
186900     ELSE                                                                 
187000       IF DIST79-ECOM-PRICE                                               
187100         MOVE VORD-SUORDV-LOC   TO PL-BILL-SUORDV-FAKT                    
187200       END-IF                                                             
187300     END-IF                                                               
187400     MOVE BILL-DAFINDOC         TO PL-BILL-TIFAKT                         
187500     IF BILL-IDPURAD = ZERO                                               
187600       MOVE SPACE               TO PL-BILL-KDFAKTYP                       
187700                                   PL-BILL-IDKUNDRF                       
187800                                   PL-BILL-KDARTURS                       
187900       MOVE ZERO                TO PL-BILL-IDARTNR                        
188000                                   PL-BILL-KVLEVART                       
188100                                   PL-BILL-PRARTNTO                       
188200     END-IF                                                               
188300                                                                          
188400     PERFORM S13-SKRIV-W47670                                             
188500     .                                                                    
188600     EJECT                                                                
188700 I-SKAPA-POSTER  SECTION.                                                 
188800*    SKAPA POSTER FÖR SWISS CUSTOM INFO                                   
188900     MOVE 'I-SKAPA-POSTER'        TO WS-SEKTION                           
189000                                                                          
189100       MOVE BILL-IDFAKT           TO UT4-IDFAKT                           
189200       MOVE BILL-IDDISTR          TO UT4-IDDISTR                          
189300       MOVE BILL-IDKUNDNR         TO UT4-IDKUNDNR                         
189400       MOVE BILL-IDORDNR7         TO UT4-IDORDNR7                         
189500       MOVE ORAD-IDARTNR          TO UT4-IDARTNR                          
189600       MOVE ORAD-VKARTNTO         TO UT4-VKARTNTO                         
189700       MOVE KKOLLI-KVLEVART       TO UT4-KVLEVART                         
189800       MOVE ORAD-PRARTNTO         TO UT4-PRARTNTO                         
189900       IF  DIST79-DEALER-PRICE                                            
190000         MOVE ORAD-PRARTNTO-LOC   TO UT4-PRARTNTO                         
190100         IF UT4-PRARTNTO = ZERO                                           
190200           MOVE ORAD-PRARTNTO-LOCPREL TO UT4-PRARTNTO                     
190300         END-IF                                                           
190400       ELSE                                                               
190500         IF DIST79-ECOM-PRICE                                             
190600           MOVE ORAD-PRARTNTO-LOC TO UT4-PRARTNTO                         
190700         END-IF                                                           
190800       END-IF                                                             
190900       MOVE BILL-IDPRODNR         TO UT4-IDPRODNR                         
191000       MOVE BILL-DAFINDOC         TO UT4-TIFAKT                           
191100       MOVE BILL-IDKOLLI          TO UT4-IDKOLLI                          
191200       IF BILL-IDPURAD = ZERO                                             
191300         MOVE SPACE TO UT4-KDARTURS                                       
191400         MOVE ZERO  TO UT4-IDSTATNR                                       
191500       ELSE                                                               
191600         MOVE ORAD-KDARTURS         TO UT4-KDARTURS                       
191700         IF ORAD-IDARTNR NOT = W-IDARTNR                                  
191800           MOVE ORAD-IDARTNR        TO W-IDARTNR                          
191900           PERFORM IMS-GU-WDK611                                          
192000           IF SEGMENT-FINNS                                               
192100            MOVE CLAG-IDSTATNR(GMT-KDSTATNR) TO UT4-IDSTATNR              
192200           END-IF                                                         
192300         END-IF                                                           
192400         IF UT4-IDSTATNR = ZERO                                           
192500           MOVE GEN-IDSTATNR        TO UT4-IDSTATNR                       
192600         END-IF                                                           
192700       END-IF                                                             
192800       MOVE BILL-IDPRODNR           TO W-IDPRODNR-E6                      
192900       MOVE BILL-IDKOLLI            TO W-IDKOLLI-E6                       
193000       IF BILL-IDPURAD = ZERO                                             
193100         MOVE ZERO                  TO UT4-TIPACKN                        
193200                                       UT4-VKORDBTO-KOLLI                 
193300                                       UT4-VKORDNTO-KOLLI                 
193400       ELSE                                                               
193500         MOVE KOLLI-TIPACKN         TO UT4-TIPACKN                        
193600         MOVE KOLLI-VKORDBTO-KOLLI  TO UT4-VKORDBTO-KOLLI                 
193700         MOVE KOLLI-VKORDNTO-KOLLI  TO UT4-VKORDNTO-KOLLI                 
193800       END-IF                                                             
193900                                                                          
194000       MOVE ZERO                    TO UT4-TIPACKN                        
194100       MOVE ZERO                    TO UT4-IDPRODNR                       
194200                                                                          
194300       PERFORM S14-SKRIV-W47656                                           
194400                                                                          
194500     .                                                                    
194600     EJECT                                                                
194700 J-SKAPA-POSTER  SECTION.                                                 
194800                                                                          
194900*      REFILL-FIL                                                         
195000     MOVE 'J-SKAPA-POSTER'       TO WS-SEKTION                            
195100                                                                          
195200*    FÖR 'STUDS' REF.DISTR. SKALL DET INTE SKAPAS FIL W47664              
195300*    I FÖRSTA FLÖDET DÅ FAKTURAN ÄR MELLAN LEV.DC ->'STUDS'DC.            
195400*    (MHA DENNA FIL SKAPAR MAN 'R30' POSTER FÖR INLEVERANS)               
195500*    DENNA FIL SKALL SKAPAS EFTER ANDRA FAKT.('STUDS'DC'->MOT.DC)         
195600*    4510-IDDC     = FINANSIELLA SÄNDANDE DC'T                            
195700*    4510-IDDC-LEV = GODS LEVERERANDE DC'T                                
195800*    I FÖRSTA FLÖDET ÄR DET FINANSIELLA = LEV.DC --> SKAPAS EJ FIL        
195900*    I ANDRA FLÖDET ÄR DET FINANSIELLA = 'STUDS'DC'T --> SKAPA FIL        
196000*                                                                         
196100     IF (DIST35-NONVCC-NONVCC-REFILL   AND                                
196200         4510-IDDC = 4510-IDDC-LEV)                                       
196300           OR                                                             
196400        (DIST35-NONVCC-NONVCC-TRANSFER AND                                
196500         4510-IDDC = 4510-IDDC-LEV)                                       
196600       CONTINUE                                                           
196700     ELSE                                                                 
196800       IF 001-IDFAKT NOT = BILL-IDFAKT                                    
196900         IF 001-IDPTYP = '001'                                            
197000           MOVE 1                TO W-POST                                
197100           PERFORM S15-SKRIV-W47664                                       
197200           MOVE SPACE            TO 001-IDPTYP                            
197300         END-IF                                                           
197400         MOVE '001'              TO 001-IDPTYP                            
197500         MOVE BILL-IDFAKT        TO 001-IDFAKT                            
197600         MOVE BILL-IDDISTR       TO 001-IDDISTR                           
197700         MOVE ZERO               TO 001-IDKUNDNR                          
197800         MOVE BILL-IDDC          TO 001-IDDC                              
197900         MOVE SPACE              TO 001-IDKUNDRF                          
198000         MOVE ZERO               TO 001-IDKOLLI                           
198100         MOVE BILL-DAFINDOC(3:6) TO 001-TIFAKT                            
198200         MOVE ORAD-IDLEVNR       TO 001-IDLEVNR                           
198300         MOVE KORD-KDFRAKT       TO 001-KDFRAKT                           
198400         MOVE BILL-KDVALISO-BET  TO 001-KDVALISO                          
198500*        IF BILL-KDVALISO-BET NOT = W-KDVALISO                            
198600*          MOVE BILL-KDVALISO-BET TO W-KDVALISO                           
198700*          MOVE BILL-DAFINDOC(5:2) TO W-TIMM                              
198800*          IF W-TIMM = ZERO                                               
198900*            MOVE DAGENS-DATUM-MAANAD TO W-TIMM                           
199000*          END-IF                                                         
199100*          PERFORM S01-HAMTA-VALUTAKURS                                   
199200*        END-IF                                                           
199300         MOVE BILL-PRKURS-BET    TO 001-PRKURS                            
199400         IF DIST79-DEALER-PRICE OR                                        
199500            DIST79-ECOM-PRICE                                             
199600           IF BILL-KDVALISO-FAKT NOT = BILL-KDVALISO-BET                  
199700             MOVE BILL-PRKURS-FIKTIV TO 001-PRKURS                        
199800           END-IF                                                         
199900         END-IF                                                           
200000         MOVE KORD-KDORDKL       TO 001-KDORDKL-MAX                       
200100         MOVE BILL-IDSHIPM       TO 001-IDSHIPM                           
200200         MOVE 1                  TO W-POST                                
200300         IF DIST35-NONVCC-NONVCC-REFILL  OR                               
200400            DIST35-NONVCC-NONVCC-TRANSFER                                 
200500           MOVE 4510-IDDC-LEV    TO 001-IDDC-LEV                          
200600         ELSE                                                             
200700           MOVE SPACE            TO 001-IDDC-LEV                          
200800         END-IF                                                           
200900**       PERFORM S15-SKRIV-W47664                                         
201000       END-IF                                                             
201100                                                                          
201200       IF 002-IDFAKT = BILL-IDFAKT    AND                                 
201300          002-IDDISTR = BILL-IDDISTR  AND                                 
201400          002-IDKUNDNR = BILL-IDKUNDNR AND                                
201500          002-IDKOLLI = BILL-IDKOLLI  AND                                 
201600          002-IDKUNDRF = KORD-IDKUNDRF                                    
201700         CONTINUE                                                         
201800       ELSE                                                               
201900         MOVE '002'             TO 002-IDPTYP                             
202000         MOVE BILL-IDFAKT       TO 002-IDFAKT                             
202100         MOVE BILL-IDDISTR      TO 002-IDDISTR                            
202200         MOVE BILL-IDKUNDNR     TO 002-IDKUNDNR                           
202300         MOVE BILL-IDDC         TO 002-IDDC                               
202400         MOVE KORD-IDKUNDRF     TO 002-IDKUNDRF                           
202500         MOVE BILL-IDKOLLI      TO 002-IDKOLLI                            
202600         MOVE KOLLI-KDKOLLI     TO 002-KDKOLLI                            
202700         MOVE KOLLI-IDLBBET     TO 002-IDLBBET                            
202800         MOVE 2                 TO W-POST                                 
202900         PERFORM S15-SKRIV-W47664                                         
203000       END-IF                                                             
203100                                                                          
203200       IF KORD-KDORDKL > 001-KDORDKL-MAX                                  
203300          MOVE KORD-KDORDKL     TO 001-KDORDKL-MAX                        
203400       END-IF                                                             
203500                                                                          
203600       MOVE '003'               TO 003-IDPTYP                             
203700       MOVE BILL-IDFAKT         TO 003-IDFAKT                             
203800       MOVE BILL-IDDISTR        TO 003-IDDISTR                            
203900       MOVE BILL-IDKUNDNR       TO 003-IDKUNDNR                           
204000       MOVE BILL-IDDC           TO 003-IDDC                               
204100       MOVE KORD-IDKUNDRF       TO 003-IDKUNDRF                           
204200       MOVE BILL-IDKOLLI        TO 003-IDKOLLI                            
204300       MOVE ORAD-IDARTNR        TO 003-IDARTNR                            
204400       MOVE KKOLLI-KVLEVART     TO 003-KVLEVART                           
204500       MOVE ORAD-PRARTNTO       TO 003-PRARTNTO                           
204600       IF DIST79-DEALER-PRICE                                             
204700         MOVE ORAD-PRARTNTO-LOC TO 003-PRARTNTO                           
204800         IF 003-PRARTNTO = ZERO                                           
204900           MOVE ORAD-PRARTNTO-LOCPREL TO 003-PRARTNTO                     
205000         END-IF                                                           
205100         IF 003-PRARTNTO = ZERO                                           
205200           PERFORM XX-003                                                 
205300           MOVE XX-PRARTNTO     TO 003-PRARTNTO                           
205400         END-IF                                                           
205500       ELSE                                                               
205600         IF DIST79-ECOM-PRICE                                             
205700           MOVE ORAD-PRARTNTO-LOC TO 003-PRARTNTO                         
205800         END-IF                                                           
205900       END-IF                                                             
206000       MOVE BILL-PRAVCOST        TO 003-PRAVCOST                          
206100       IF DIST35-NONVCC-NONVCC-REFILL  OR                                 
206200          DIST35-NONVCC-NONVCC-TRANSFER                                   
206300         MOVE 4510-IDDC-LEV      TO 003-IDDC-LEV                          
206400       ELSE                                                               
206500         MOVE SPACE              TO 003-IDDC-LEV                          
206600       END-IF                                                             
206700       MOVE 3                   TO W-POST                                 
206800       PERFORM S15-SKRIV-W47664                                           
206900     END-IF                                                               
207000                                                                          
207100     .                                                                    
207200     EJECT                                                                
207300 XX-003 SECTION.                                                          
207400     COMPUTE XX-PRARTNTO = BILL-SUNTO-LINE /                              
207500                           KKOLLI-KVLEVART                                
207600     COMPUTE XX-PRARTBTO = BILL-SUBTO-LINE /                              
207700                           KKOLLI-KVLEVART                                
207800     .                                                                    
207900     EJECT                                                                
208000 K-SKAPA-POSTER  SECTION.                                                 
208100*       RETUR-POSTER                                                      
208200     MOVE 'K-SKAPA-POSTER'      TO WS-SEKTION                             
208300                                                                          
208400     IF KORD-IDORDER NOT = OHUV-IDORDER                                   
208500        MOVE KORD-IDORDER       TO W-IDORDER                              
208600        PERFORM IMS-GU-WDQ201                                             
208700        IF SEGMENT-SAKNAS                                                 
208800          MOVE SPACE            TO OHUV-FLOVRLEV                          
208900          MOVE SPACE            TO OHUV-BEVARREF                          
209000          MOVE ZERO             TO OHUV-KDTULLVE                          
209100        END-IF                                                            
209200     END-IF                                                               
209300***  IF BILL-IDPURAD NOT = ZERO AND OHUV-KDTULLVE = ZERO                  
209400     IF BILL-IDPURAD NOT = ZERO                                           
209500       MOVE ZERO                 TO UT6-IDLEVNR                           
209600       MOVE DCS-IDLEVNR-DC       TO UT6-IDLEVNR                           
209700                                                                          
209800******************************************************************        
209900*                                                                         
210000*  KOLLA OM KVALITETSTRANSFER (GER SVARET RADER-FINNS)                    
210100*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
210200*                                                                         
210300******************************************************************        
210400                                                                          
210500       MOVE KORD-IDDISTR      TO W-TP4TRAN-IDDISTR                        
210600                                                                          
210700       PERFORM DB2-SELECT-TP4TRAN                                         
210800                                                                          
210900       MOVE BILL-IDFAKT          TO UT6-IDFS                              
211000       MOVE BILL-DAFINDOC(3:6)   TO UT6-TIAVIDAT                          
211100       MOVE BILL-IDDISTR         TO TEST-IDDISTR                          
211200       IF DIST35-RETUR-Q OR                                               
211300         (RADER-FINNS AND                                                 
211400          TP4TRAN-KDARBTYP = 'QUAL')                                      
211500         MOVE 'QUALITY'          TO UT6-IDLBBET                           
211600       ELSE                                                               
211700         MOVE KOLLI-IDLBBET      TO UT6-IDLBBET                           
211800       END-IF                                                             
211900       MOVE ORAD-IDARTNR         TO UT6-IDARTNR                           
212000       MOVE KKOLLI-KVLEVART      TO UT6-KVLEVART                          
212100       MOVE BILL-IDKOLLI         TO UT6-IDOKOLLI                          
212200       IF FIRST-POST                                                      
212300         MOVE NEJ                TO UT6-FLDIVKLI                          
212400         MOVE NEJ                TO FIRST-POST-SW                         
212500       ELSE                                                               
212600         MOVE JA                 TO UT6-FLDIVKLI                          
212700       END-IF                                                             
212800                                                                          
212900       MOVE NEJ                  TO UT6-FLSATS                            
213000                                                                          
213100                                                                          
213200       IF OHUV-BEVARREF = 'DAMAGED   '                                    
213300         MOVE JA                 TO UT6-FLKVAFEL                          
213400       ELSE                                                               
213500         MOVE NEJ                TO UT6-FLKVAFEL                          
213600       END-IF                                                             
213700       MOVE BILL-IDSHIPM         TO UT6-IDSHIPM                           
213800       PERFORM S16-SKRIV-W47674                                           
213900     END-IF                                                               
214000     .                                                                    
214100     EJECT                                                                
214200 L-SKRIV-ARTIKEL-TULL  SECTION.                                           
214300*       TILL TULLREG. +  ARTIKELREG.                                      
214400     MOVE 'L-SKRIV-ARTIKEL-TULL'  TO WS-SEKTION                           
214500                                                                          
214600*    IF TILLAGG-JA                                                        
214700*      CONTINUE                                                           
214800*    ELSE                                                                 
214900       PERFORM LA-SKAPA-FIL65                                             
215000*    END-IF                                                               
215100     .                                                                    
215200     EJECT                                                                
215300 LA-SKAPA-FIL65    SECTION.                                               
215400     MOVE 'LA-SKAPA-FIL65'           TO WS-SEKTION                        
215500                                                                          
215600     IF TILLAGG-JA                                                        
215700       MOVE ZERO                     TO UT7-BILL-IDARTNR                  
215800                                        UT7-BILL-IDKOLLI                  
215900                                        UT7-BILL-IDTULL                   
216000                                        UT7-BILL-IDKUNDRF                 
216100                                        UT7-BILL-FLORDSPE                 
216200                                        UT7-BILL-FLOVRLEV                 
216300                                        UT7-BILL-KDFRAKT                  
216400                                        UT7-BILL-KDORDKL                  
216500                                        UT7-BILL-KDTULLVE                 
216600                                        UT7-BILL-IDUSER-OREG              
216700                                        UT7-BILL-IDKONTO                  
216800                                        UT7-BILL-KVBEART                  
216900                                        UT7-BILL-KVAVBART                 
217000                                        UT7-BILL-ADLAGOMR                 
217100                                        UT7-BILL-KVORDRAD                 
217200                                        UT7-BILL-TIFAKT                   
217300                                        UT7-BILL-TIREGDAT                 
217400                                        UT7-BILL-VKORDBTO-KOLLI           
217500                                        UT7-BILL-VKORDNTO-KOLLI           
217600                                        UT7-BILL-TISKEPPN                 
217700       MOVE SPACE                    TO UT7-BILL-KDFAKTYP                 
217800                                        UT7-BILL-FLLSBOK                  
217900                                        UT7-BILL-FLDIRLEV                 
218000                                        UT7-BILL-IDKST                    
218100                                        UT7-BILL-IDANALYS                 
218200                                        UT7-BILL-IDLEVNR                  
218300                                        UT7-BILL-KDVAT                    
218400                                        UT7-BILL-IDUSER-PACK              
218500                                        UT7-BILL-KDKOLLI                  
218600                                        UT7-BILL-IDBORD                   
218700     ELSE                                                                 
218800       IF KORD-IDORDER NOT = OHUV-IDORDER OR                              
218900          KORD-IDORDER = ZERO                                             
219000         MOVE KORD-IDORDER     TO W-IDORDER                               
219100         PERFORM IMS-GU-WDQ201                                            
219200         IF SEGMENT-SAKNAS                                                
219300           MOVE ZERO                 TO OHUV-KDTULLVE                     
219400         END-IF                                                           
219500       END-IF                                                             
219600       MOVE OHUV-KDTULLVE            TO UT7-BILL-KDTULLVE                 
219700       MOVE OHUV-IDUSER              TO UT7-BILL-IDUSER-OREG              
219800       MOVE ORAD-IDARTNR             TO UT7-BILL-IDARTNR                  
219900       MOVE ORAD-FLDIRLEV            TO UT7-BILL-FLDIRLEV                 
220000       MOVE ORAD-IDKONTO             TO UT7-BILL-IDKONTO                  
220100       MOVE ORAD-IDKST               TO UT7-BILL-IDKST                    
220200       MOVE ORAD-IDANALYS            TO UT7-BILL-IDANALYS                 
220300       MOVE ORAD-KVBEART             TO UT7-BILL-KVBEART                  
220400       MOVE ORAD-KVAVBART            TO UT7-BILL-KVAVBART                 
220500       MOVE ORAD-IDLEVNR             TO UT7-BILL-IDLEVNR                  
220600       MOVE ORAD-KDVAT               TO UT7-BILL-KDVAT                    
220700       MOVE ORAD-ADLAGOMR            TO UT7-BILL-ADLAGOMR                 
220800       MOVE KORD-IDKUNDRF            TO UT7-BILL-IDKUNDRF                 
220900       MOVE KORD-FLORDSPE            TO UT7-BILL-FLORDSPE                 
221000       MOVE KORD-FLOVRLEV            TO UT7-BILL-FLOVRLEV                 
221100       MOVE KORD-KDFAKTYP            TO UT7-BILL-KDFAKTYP                 
221200       MOVE KORD-KDFRAKT             TO UT7-BILL-KDFRAKT                  
221300       MOVE KORD-KDORDKL             TO UT7-BILL-KDORDKL                  
221400       MOVE KORD-FLLSBOK             TO UT7-BILL-FLLSBOK                  
221500       MOVE KORD-IDUSER              TO UT7-BILL-IDUSER-PACK              
221600       MOVE KOLLI-IDTULL             TO UT7-BILL-IDTULL                   
221700       MOVE KOLLI-KVORDRAD           TO UT7-BILL-KVORDRAD                 
221800       MOVE KOLLI-KDKOLLI            TO UT7-BILL-KDKOLLI                  
221900       MOVE KOLLI-TIFAKT             TO UT7-BILL-TIFAKT                   
222000       MOVE KOLLI-TIFAKT             TO UT7-BILL-TIREGDAT                 
222100       MOVE KOLLI-VKORDBTO-KOLLI     TO UT7-BILL-VKORDBTO-KOLLI           
222200       MOVE KOLLI-VKORDNTO-KOLLI     TO UT7-BILL-VKORDNTO-KOLLI           
222300     END-IF                                                               
222400                                                                          
222500     MOVE BILL-IDPRODNR              TO UT7-BILL-IDPRODNR                 
222600     MOVE BILL-IDKOLLI               TO UT7-BILL-IDKOLLI                  
222700     MOVE BILL-IDPURAD               TO UT7-BILL-IDPURAD                  
222800     MOVE BILL-IDSHIPM               TO UT7-BILL-IDSHIPM                  
222900     MOVE BILL-BEART                 TO UT7-BILL-BEART                    
223000     MOVE BILL-DAFINDOC              TO UT7-BILL-DAFINDOC                 
223100     MOVE BILL-IDDC                  TO UT7-BILL-IDDC                     
223200     MOVE BILL-IDDISTR               TO UT7-BILL-IDDISTR                  
223300     MOVE BILL-IDKUNDNR              TO UT7-BILL-IDKUNDNR                 
223400     MOVE BILL-IDORDNR7              TO UT7-BILL-IDORDNR7                 
223500     MOVE BILL-IDFAKT                TO UT7-BILL-IDFAKT                   
223600     MOVE BILL-IDPARTNR              TO UT7-BILL-IDPARTNR                 
223700     MOVE BILL-KDVALISO-FAKT         TO UT7-BILL-KDVALISO                 
223800                                                                          
223900     IF BILL-KVLEVART = ZERO                                              
224000       MOVE KKOLLI-KVLEVART          TO UT7-BILL-KVLEVART                 
224100     ELSE                                                                 
224200       MOVE BILL-KVLEVART            TO UT7-BILL-KVLEVART                 
224300     END-IF                                                               
224400                                                                          
224500     MOVE BILL-PRARTNTO              TO UT7-BILL-PRARTNTO                 
224600     MOVE BILL-PRARTNTO-LOC          TO UT7-BILL-PRARTNTO-LOC             
224700     IF TILLAGG-NEJ                                                       
224800       IF BILL-PRARTNTO = ZERO                                            
224900         MOVE ORAD-PRARTNTO          TO UT7-BILL-PRARTNTO                 
225000       END-IF                                                             
225100       IF BILL-PRARTNTO-LOC = ZERO                                        
225200         MOVE ORAD-PRARTNTO-LOC      TO UT7-BILL-PRARTNTO-LOC             
225300       END-IF                                                             
225400     END-IF                                                               
225500                                                                          
225600     MOVE BILL-IDDISTR               TO TEST-IDDISTR                      
225700                                                                          
225800     IF TILLAGG-NEJ                                                       
225900       IF DIST79-DEALER-PRICE OR                                          
226000          DIST79-ECOM-PRICE                                               
226100        IF UT7-BILL-PRARTNTO-LOC = ZERO                                   
226200         PERFORM XX-UT7                                                   
226300         MOVE XX-PRARTNTO            TO UT7-BILL-PRARTNTO-LOC             
226400        END-IF                                                            
226500       END-IF                                                             
226600     END-IF                                                               
226700*                                                                         
226800     MOVE BILL-VKARTNTO              TO UT7-BILL-VKARTNTO                 
226900**   VI FLYTTAR NETTOVIKT UTAN EMB TILL VIKT MED EMB PGA TULLEN           
227000     IF TILLAGG-JA                                                        
227100       MOVE ZERO                     TO UT7-BILL-VKART-NTO-KG             
227200     ELSE                                                                 
227300       MOVE ORAD-VKART-NTO-KG        TO UT7-BILL-VKART-NTO-KG             
227400     END-IF                                                               
227500*                                                                         
227600**   TA REDA PÅ OM DET ÄR STUDS-FLÖDE OCH OM JA SKALL UPPDAT SL           
227700     MOVE NEJ                        TO UT7-BILL-FLSTUDSUP                
227800     IF TILLAGG-NEJ                                                       
227900       MOVE JA                       TO UT7-BILL-FLSTUDSUP                
228000       IF VORD-IDDC-EXP > SPACE                                           
228100         IF VORD-IDDC-EXP = WC-CDC-SE                                     
228200**       EXPORT-REFILL OCH IMPORTÖRSFLÖDET FRÅN ETT NON VCC-NDC           
228300           IF BILL-IDFAKT  = KOLLI-IDFAKT                                 
228400             MOVE NEJ                TO UT7-BILL-FLSTUDSUP                
228500           END-IF                                                         
228600         ELSE                                                             
228700**       VOR FRÅN DC.11 TILL DEALERDISTR.I CN,IN,KR                       
228800           IF BILL-IDFAKT  = KOLLI-IDFAKT-EXP                             
228900             MOVE NEJ                TO UT7-BILL-FLSTUDSUP                
229000           END-IF                                                         
229100         END-IF                                                           
229200       END-IF                                                             
229300     END-IF                                                               
229400                                                                          
229500     MOVE BILL-TISKEPPN              TO UT7-BILL-TISKEPPN                 
229600     MOVE BILL-SUBTO-LINE            TO UT7-BILL-SUBTO-LINE               
229700     MOVE BILL-SUBTO-TOT             TO UT7-BILL-SUBTO-TOT                
229800     MOVE BILL-SUNTO-LINE            TO UT7-BILL-SUNTO-LINE               
229900     MOVE BILL-SUNTO-TOT             TO UT7-BILL-SUNTO-TOT                
230000     MOVE BILL-SUVAT-LINE            TO UT7-BILL-SUVAT-LINE               
230100     MOVE BILL-IDFKNGRP              TO UT7-BILL-IDFKNGRP                 
230200     MOVE BILL-KDARTRAB              TO UT7-BILL-KDARTRAB                 
230300     MOVE BILL-KDPRODSL              TO UT7-BILL-KDPRODSL                 
230400     MOVE BILL-PRAVCOST-BILLIT       TO UT7-BILL-PRAVCOST-BILLIT          
230500     MOVE BILL-KDVALISO-AVC          TO UT7-BILL-KDVALISO-AVC             
230600     MOVE BILL-KDVALISO-NTO          TO UT7-BILL-KDVALISO-NTO             
230700     MOVE BILL-IDDC-BILLIT           TO UT7-BILL-IDDC-BILLIT              
230800     IF TILLAGG-NEJ                                                       
230900       MOVE ORAD-PRAVCOST            TO UT7-BILL-PRAVCOST-BILLIT          
231000       MOVE ORAD-KDVALISO-EXP        TO UT7-BILL-KDVALISO-AVC             
231100     END-IF                                                               
231200     MOVE BILL-PRAVCOST-CORE         TO UT7-BILL-PRAVCOST-CORE            
231300     MOVE BILL-KDVALISO-BET          TO UT7-BILL-KDVALISO-BET             
231400     MOVE BILL-PRKURS-BET            TO UT7-BILL-PRKURS-BET               
231500     MOVE BILL-PRKURS-FAKT           TO UT7-BILL-PRKURS-FAKT              
231600     MOVE BILL-PRKURS-FIKTIV         TO UT7-BILL-PRKURS-FIKTIV            
231700                                                                          
231800     MOVE BILL-KDFAKSTA-EXP          TO UT7-BILL-KDFAKSTA-EXP             
231900     MOVE BILL-SUNTO-PART-LOC                                             
232000                                     TO UT7-BILL-SUNTO-PART-LOC           
232100*    MOVE BILL-SUVAT-BILLIT-TOT-PART-L                                    
232200*                            TO UT7-BILL-SUVAT-BILLIT-TOT-PART-L          
232300*    MOVE BILL-SUVAT-BILLIT-TOT-PART-R                                    
232400*                            TO UT7-BILL-SUVAT-BILLIT-TOT-PART-R          
232500     MOVE BILL-SUBTO-TOT-PART-LOC                                         
232600                             TO UT7-BILL-SUBTO-TOT-PART-LOC               
232700     MOVE BILL-SUNTO-TOT-LOC         TO UT7-BILL-SUNTO-TOT-LOC            
232800     MOVE BILL-SUVAT-BILLIT-TOT-LOC                                       
232900                             TO UT7-BILL-SUVAT-BILLIT-TOT-LOC             
233000     MOVE BILL-SUBTO-TOT-LOC         TO UT7-BILL-SUBTO-TOT-LOC            
233100     MOVE BILL-KDVALISO-LOC          TO UT7-BILL-KDVALISO-LOC             
233200     MOVE BILL-PRKURS-LOC            TO UT7-BILL-PRKURS-LOC               
233300     MOVE BILL-KDTECKEN-LOC          TO UT7-BILL-KDTECKEN-LOC             
233400     MOVE BILL-SUNTO-LOCC            TO UT7-BILL-SUNTO-LOCC               
233500     MOVE BILL-SUNTO-PART-RECALC     TO UT7-BILL-SUNTO-PART-RECALC        
233600     MOVE BILL-SUBTO-TOT-PART-RECALC TO                                   
233700                                    UT7-BILL-SUBTO-TOT-PART-RECALC        
233800     MOVE BILL-SUNTO-TOT-RECALC      TO UT7-BILL-SUNTO-TOT-RECALC         
233900     MOVE BILL-SUBTO-TOT-RECALC      TO UT7-BILL-SUBTO-TOT-RECALC         
234000     MOVE BILL-KDVALISO-RECALC       TO UT7-BILL-KDVALISO-RECALC          
234100     MOVE BILL-PRKURS-RECALC         TO UT7-BILL-PRKURS-RECALC            
234200     MOVE BILL-KDTECKEN-RECALC       TO UT7-BILL-KDTECKEN-RECALC          
234300     MOVE BILL-SUNTO-LOCC-RECALC     TO UT7-BILL-SUNTO-LOCC-RECALC        
234400     MOVE BILL-FLPCOO                TO UT7-BILL-FLPCOO                   
234500     MOVE BILL-KDARTURS              TO UT7-BILL-KDARTURS                 
234600     IF TILLAGG-NEJ                                                       
234700       IF BILL-KDARTURS = SPACE                                           
234800         MOVE ORAD-KDARTURS          TO UT7-BILL-KDARTURS                 
234900       END-IF                                                             
235000     END-IF                                                               
235100                                                                          
235200     IF TILLAGG-NEJ                                                       
235300       MOVE KORD-IDORDER             TO W-WDQ301-IDORDER                  
235400       MOVE KORD-IDDC                TO W-WDQ301-IDDC                     
235500       MOVE KORD-IDPRODNR            TO W-WDQ301-IDPRODNR                 
235600       MOVE KORD-IDPLKLST            TO W-WDQ301-IDPLKLST                 
235700                                                                          
235800       PERFORM IMS-GU-WDQ301                                              
235900                                                                          
236000       IF SEGMENT-FINNS                                                   
236100         MOVE ODEL-IDBORD            TO UT7-BILL-IDBORD                   
236200       ELSE                                                               
236300         MOVE SPACE                  TO UT7-BILL-IDBORD                   
236400       END-IF                                                             
236500     END-IF                                                               
236600                                                                          
236700     PERFORM S17-SKRIV-W47665                                             
236800     .                                                                    
236900     EJECT                                                                
237000 XX-UT7 SECTION.                                                          
237100     COMPUTE XX-PRARTNTO = BILL-SUNTO-LINE /                              
237200                           KKOLLI-KVLEVART                                
237300     COMPUTE XX-PRARTBTO = BILL-SUBTO-LINE /                              
237400                           KKOLLI-KVLEVART                                
237500     .                                                                    
237600     EJECT                                                                
237700 M-SKRIV-SATS-W4765H  SECTION.                                            
237800     MOVE 'M-SKRIV-SATS-W4765H'  TO WS-SEKTION                            
237900                                                                          
238000     IF BILL-IDKOLLI = ZERO AND BILL-IDPURAD = ZERO                       
238100       CONTINUE                                                           
238200     ELSE                                                                 
238300      IF ORAD-IDBIL > SPACE AND  ORAD-IDSYSTEM  = 'VDI '                  
238400       MOVE +1                  TO WRAD-KDSOFT                            
238500      ELSE                                                                
238600       IF ORAD-IDSYSTEM = 'SOFT'                                          
238700         MOVE +2                TO WRAD-KDSOFT                            
238800       ELSE                                                               
238900         MOVE ORAD-IDARTNR      TO TEST-ARTIKEL                           
239000         IF ART04-SOFTWARE                                                
239100           MOVE +3              TO WRAD-KDSOFT                            
239200         ELSE                                                             
239300           IF ORAD-IDSYSTEM = 'W371' OR 'W37A'                            
239400             MOVE +4            TO WRAD-KDSOFT                            
239500           ELSE                                                           
239600             MOVE +0            TO WRAD-KDSOFT                            
239700           END-IF                                                         
239800         END-IF                                                           
239900       END-IF                                                             
240000      END-IF                                                              
240100      IF WRAD-KDSOFT = +0                                                 
240200        MOVE BILL-IDFAKT         TO SAT-IDFAKT                            
240300        MOVE BILL-IDDISTR        TO SAT-IDDISTR                           
240400        MOVE WRAD-KDSOFT         TO SAT-KDSOFT                            
240500        MOVE KORD-KDORDKL        TO SAT-KDORDKL                           
240600        MOVE KORD-IDARTNR-SATS   TO SAT-IDARTNR                           
240700        MOVE KORD-KVBEART-SATS   TO SAT-KVBEART                           
240800        MOVE BILL-DAFINDOC       TO SAT-TIFAKT                            
240900        MOVE KORD-IDKUNDRF(1:5)  TO SAT-IDKUNDRF                          
241000        MOVE ORAD-IDKONTO        TO SAT-IDKONTO                           
241100        MOVE ORAD-IDANALYS       TO SAT-IDANALYS                          
241200        MOVE ORAD-IDKST          TO SAT-IDKST                             
241300                                                                          
241400        WRITE SAT-POST           FROM SAT-UTAREA                          
241500        MOVE SPACE               TO SAT-UTAREA                            
241600                                                                          
241700        MOVE 'W4765H'            TO POSTSUM-FDNAMN                        
241800        MOVE 'W4765HDG'          TO POSTSUM-DDNAMN2                       
241900        MOVE 'SAT'               TO POSTSUM-TRANSTYP                      
242000                                                                          
242100        CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                     
242200      END-IF                                                              
242300     END-IF                                                               
242400     .                                                                    
242500     EJECT                                                                
242600 XX-FIXE411  SECTION.                                                     
242700*    IF ORAD-PRARTNTO-LOC > ORAD-PRARTBTO-LOC                             
242800*      AND ORAD-PRARTBTO-LOC NOT = ZERO                                   
242900*      MOVE ORAD-PRARTNTO-LOC  TO X4-PRARTNTO                             
243000*      MOVE ORAD-PRARTBTO-LOC  TO X4-PRARTBTO                             
243100*      MOVE X4-PRARTBTO        TO ORAD-PRARTNTO-LOC                       
243200*      MOVE X4-PRARTNTO        TO ORAD-PRARTBTO-LOC                       
243300*    END-IF                                                               
243400**FIX  FÖR ATT TA HAND OM GROSS < NET PRISER ***                          
243500       IF DIST79-DEALER-PRICE                                             
243600         IF ORAD-PRARTNTO-LOC > ORAD-PRARTBTO-LOC                         
243700           MOVE ORAD-PRARTNTO-LOC TO ORAD-PRARTBTO-LOC                    
243800           MOVE ZERO              TO ORAD-RERAB                           
243900         END-IF                                                           
244000       END-IF                                                             
244100**END-FIX                                                                 
244200     .                                                                    
244300     EJECT                                                                
244400 N-SKAPA-POSTER  SECTION.                                                 
244500*     REFILL-FIL FÖR REREFILL DISTR - GÄLLER BARA INOM JP 23/2 '23        
244600     MOVE 'N-SKAPA-POSTER'       TO WS-SEKTION                            
244700                                                                          
244800     IF 001-IDFAKT NOT = BILL-IDFAKT                                      
244900       IF 001-IDPTYP = '001'                                              
245000         MOVE 1                TO W-POST                                  
245100         PERFORM S15-SKRIV-W47664                                         
245200         MOVE SPACE            TO 001-IDPTYP                              
245300       END-IF                                                             
245400       MOVE '001'              TO 001-IDPTYP                              
245500       MOVE BILL-IDFAKT        TO 001-IDFAKT                              
245600       MOVE BILL-IDDISTR       TO 001-IDDISTR                             
245700                                  TEST-IDDISTR                            
245800       MOVE ZERO               TO 001-IDKUNDNR                            
245900       MOVE BILL-IDDC          TO 001-IDDC                                
246000       MOVE SPACE              TO 001-IDKUNDRF                            
246100       MOVE ZERO               TO 001-IDKOLLI                             
246200       MOVE BILL-DAFINDOC(3:6) TO 001-TIFAKT                              
246300       MOVE DCS-IDLEVNR-DC     TO 001-IDLEVNR                             
246400       MOVE KORD-KDFRAKT       TO 001-KDFRAKT                             
246500       MOVE BILL-KDVALISO-BET  TO 001-KDVALISO                            
246600       MOVE BILL-PRKURS-BET    TO 001-PRKURS                              
246700       IF DIST79-DEALER-PRICE OR                                          
246800          DIST79-ECOM-PRICE                                               
246900         IF BILL-KDVALISO-FAKT NOT = BILL-KDVALISO-BET                    
247000           MOVE BILL-PRKURS-FIKTIV TO 001-PRKURS                          
247100         END-IF                                                           
247200       END-IF                                                             
247300       MOVE KORD-KDORDKL       TO 001-KDORDKL-MAX                         
247400       MOVE BILL-IDSHIPM       TO 001-IDSHIPM                             
247500       MOVE 1                  TO W-POST                                  
247600       MOVE SPACE              TO 001-IDDC-LEV                            
247700     END-IF                                                               
247800                                                                          
247900     IF 002-IDFAKT = BILL-IDFAKT    AND                                   
248000        002-IDDISTR = BILL-IDDISTR  AND                                   
248100        002-IDKUNDNR = BILL-IDKUNDNR AND                                  
248200        002-IDKOLLI = BILL-IDKOLLI  AND                                   
248300        002-IDKUNDRF = KORD-IDKUNDRF                                      
248400       CONTINUE                                                           
248500     ELSE                                                                 
248600       MOVE '002'             TO 002-IDPTYP                               
248700       MOVE BILL-IDFAKT       TO 002-IDFAKT                               
248800       MOVE BILL-IDDISTR      TO 002-IDDISTR                              
248900       MOVE BILL-IDKUNDNR     TO 002-IDKUNDNR                             
249000       MOVE BILL-IDDC         TO 002-IDDC                                 
249100       MOVE KORD-IDKUNDRF     TO 002-IDKUNDRF                             
249200       MOVE BILL-IDKOLLI      TO 002-IDKOLLI                              
249300       MOVE KOLLI-KDKOLLI     TO 002-KDKOLLI                              
249400       IF DIST35-RETUR-Q                                                  
249500         MOVE 'QUALITY'       TO 002-IDLBBET                              
249600       ELSE                                                               
249700         MOVE KOLLI-IDLBBET   TO 002-IDLBBET                              
249800       END-IF                                                             
249900       MOVE 2                 TO W-POST                                   
250000       PERFORM S15-SKRIV-W47664                                           
250100     END-IF                                                               
250200                                                                          
250300     IF KORD-KDORDKL > 001-KDORDKL-MAX                                    
250400       MOVE KORD-KDORDKL     TO 001-KDORDKL-MAX                           
250500     END-IF                                                               
250600                                                                          
250700     MOVE '003'               TO 003-IDPTYP                               
250800     MOVE BILL-IDFAKT         TO 003-IDFAKT                               
250900     MOVE BILL-IDDISTR        TO 003-IDDISTR                              
251000     MOVE BILL-IDKUNDNR       TO 003-IDKUNDNR                             
251100     MOVE BILL-IDDC           TO 003-IDDC                                 
251200     MOVE KORD-IDKUNDRF       TO 003-IDKUNDRF                             
251300     MOVE BILL-IDKOLLI        TO 003-IDKOLLI                              
251400     MOVE ORAD-IDARTNR        TO 003-IDARTNR                              
251500     MOVE KKOLLI-KVLEVART     TO 003-KVLEVART                             
251600     MOVE ORAD-PRARTNTO       TO 003-PRARTNTO                             
251700     IF DIST79-DEALER-PRICE                                               
251800       MOVE ORAD-PRARTNTO-LOC TO 003-PRARTNTO                             
251900       IF 003-PRARTNTO = ZERO                                             
252000         MOVE ORAD-PRARTNTO-LOCPREL TO 003-PRARTNTO                       
252100       END-IF                                                             
252200       IF 003-PRARTNTO = ZERO                                             
252300         PERFORM XX-003                                                   
252400         MOVE XX-PRARTNTO     TO 003-PRARTNTO                             
252500       END-IF                                                             
252600     ELSE                                                                 
252700       IF DIST79-ECOM-PRICE                                               
252800         MOVE ORAD-PRARTNTO-LOC TO 003-PRARTNTO                           
252900         IF 003-PRARTNTO = ZERO                                           
253000           PERFORM XX-003                                                 
253100           MOVE XX-PRARTNTO   TO 003-PRARTNTO                             
253200         END-IF                                                           
253300       END-IF                                                             
253400     END-IF                                                               
253500     MOVE BILL-PRAVCOST       TO 003-PRAVCOST                             
253600     MOVE SPACE               TO 003-IDDC-LEV                             
253700     MOVE 3                   TO W-POST                                   
253800     PERFORM S15-SKRIV-W47664                                             
253900                                                                          
254000     .                                                                    
254100     EJECT                                                                
254200 R-SKAPA-POSTER  SECTION.                                                 
254300*      REFILL-FIL FÖR KONVERTERING KOREA                                  
254400     MOVE 'R-SKAPA-POSTER'       TO WS-SEKTION                            
254500                                                                          
254600     IF R-001-IDFAKT NOT = BILL-IDFAKT                                    
254700       IF R-001-IDPTYP = '001'                                            
254800         MOVE 1                  TO W-POST-R                              
254900         PERFORM S15X-SKRIV-W47664R                                       
255000         MOVE SPACE              TO R-001-IDPTYP                          
255100       END-IF                                                             
255200       MOVE '001'                TO R-001-IDPTYP                          
255300       MOVE BILL-IDFAKT          TO R-001-IDFAKT                          
255400       MOVE BILL-IDDISTR         TO R-001-IDDISTR                         
255500       MOVE ZERO                 TO R-001-IDKUNDNR                        
255600       MOVE BILL-IDDC            TO R-001-IDDC                            
255700       MOVE SPACE                TO R-001-IDKUNDRF                        
255800       MOVE ZERO                 TO R-001-IDKOLLI                         
255900       MOVE BILL-DAFINDOC(3:6)   TO R-001-TIFAKT                          
256000       MOVE ORAD-IDLEVNR         TO R-001-IDLEVNR                         
256100       MOVE KORD-KDFRAKT         TO R-001-KDFRAKT                         
256200       MOVE BILL-KDVALISO-BET    TO R-001-KDVALISO                        
256300       MOVE BILL-PRKURS-BET      TO R-001-PRKURS                          
256400       IF DIST79-DEALER-PRICE OR                                          
256500          DIST79-ECOM-PRICE                                               
256600         IF BILL-KDVALISO-FAKT NOT = BILL-KDVALISO-BET                    
256700           MOVE BILL-PRKURS-FIKTIV TO R-001-PRKURS                        
256800         END-IF                                                           
256900       END-IF                                                             
257000       MOVE KORD-KDORDKL         TO R-001-KDORDKL-MAX                     
257100       MOVE BILL-IDSHIPM         TO R-001-IDSHIPM                         
257200       MOVE 1                    TO W-POST-R                              
257300       MOVE SPACE                TO R-001-IDDC-LEV                        
257400     END-IF                                                               
257500                                                                          
257600     IF R-002-IDFAKT   = BILL-IDFAKT   AND                                
257700        R-002-IDDISTR  = BILL-IDDISTR  AND                                
257800        R-002-IDKUNDNR = BILL-IDKUNDNR AND                                
257900        R-002-IDKOLLI  = BILL-IDKOLLI  AND                                
258000        R-002-IDKUNDRF = KORD-IDKUNDRF                                    
258100       CONTINUE                                                           
258200     ELSE                                                                 
258300       MOVE '002'               TO R-002-IDPTYP                           
258400       MOVE BILL-IDFAKT         TO R-002-IDFAKT                           
258500       MOVE BILL-IDDISTR        TO R-002-IDDISTR                          
258600       MOVE BILL-IDKUNDNR       TO R-002-IDKUNDNR                         
258700       MOVE BILL-IDDC           TO R-002-IDDC                             
258800       MOVE KORD-IDKUNDRF       TO R-002-IDKUNDRF                         
258900       MOVE BILL-IDKOLLI        TO R-002-IDKOLLI                          
259000       MOVE KOLLI-KDKOLLI       TO R-002-KDKOLLI                          
259100       MOVE KOLLI-IDLBBET       TO R-002-IDLBBET                          
259200       MOVE 2                   TO W-POST-R                               
259300       PERFORM S15X-SKRIV-W47664R                                         
259400     END-IF                                                               
259500                                                                          
259600     IF KORD-KDORDKL > R-001-KDORDKL-MAX                                  
259700        MOVE KORD-KDORDKL       TO R-001-KDORDKL-MAX                      
259800     END-IF                                                               
259900                                                                          
260000     MOVE '003'                 TO R-003-IDPTYP                           
260100     MOVE BILL-IDFAKT           TO R-003-IDFAKT                           
260200     MOVE BILL-IDDISTR          TO R-003-IDDISTR                          
260300     MOVE BILL-IDKUNDNR         TO R-003-IDKUNDNR                         
260400     MOVE BILL-IDDC             TO R-003-IDDC                             
260500     MOVE KORD-IDKUNDRF         TO R-003-IDKUNDRF                         
260600     MOVE BILL-IDKOLLI          TO R-003-IDKOLLI                          
260700     MOVE ORAD-IDARTNR          TO R-003-IDARTNR                          
260800     MOVE KKOLLI-KVLEVART       TO R-003-KVLEVART                         
260900     MOVE ORAD-PRARTNTO         TO R-003-PRARTNTO                         
261000     IF DIST79-DEALER-PRICE                                               
261100       MOVE ORAD-PRARTNTO-LOC TO R-003-PRARTNTO                           
261200       IF R-003-PRARTNTO = ZERO                                           
261300         MOVE ORAD-PRARTNTO-LOCPREL TO R-003-PRARTNTO                     
261400       END-IF                                                             
261500       IF R-003-PRARTNTO = ZERO                                           
261600         PERFORM XX-003                                                   
261700         MOVE XX-PRARTNTO       TO R-003-PRARTNTO                         
261800       END-IF                                                             
261900     ELSE                                                                 
262000       IF DIST79-ECOM-PRICE                                               
262100         MOVE ORAD-PRARTNTO-LOC TO R-003-PRARTNTO                         
262200       END-IF                                                             
262300     END-IF                                                               
262400     MOVE BILL-PRAVCOST         TO R-003-PRAVCOST                         
262500     MOVE SPACE                  TO R-003-IDDC-LEV                        
262600     MOVE 3                     TO W-POST-R                               
262700     PERFORM S15X-SKRIV-W47664R                                           
262800                                                                          
262900     .                                                                    
263000     EJECT                                                                
263100 S-SKAPA-POSTER  SECTION.                                                 
263200*      REFILL-FIL FÖR KONVERTERING KOREA                                  
263300     MOVE 'S-SKAPA-POSTER'       TO WS-SEKTION                            
263400                                                                          
263500     IF S-001-IDFAKT NOT = BILL-IDFAKT                                    
263600       IF S-001-IDPTYP = '001'                                            
263700         MOVE 1                  TO W-POST-S                              
263800         PERFORM S15Y-SKRIV-W47664S                                       
263900         MOVE SPACE              TO S-001-IDPTYP                          
264000       END-IF                                                             
264100       MOVE '001'                TO S-001-IDPTYP                          
264200       MOVE BILL-IDFAKT          TO S-001-IDFAKT                          
264300       MOVE BILL-IDDISTR         TO S-001-IDDISTR                         
264400       MOVE ZERO                 TO S-001-IDKUNDNR                        
264500       MOVE BILL-IDDC            TO S-001-IDDC                            
264600       MOVE SPACE                TO S-001-IDKUNDRF                        
264700       MOVE ZERO                 TO S-001-IDKOLLI                         
264800       MOVE BILL-DAFINDOC(3:6)   TO S-001-TIFAKT                          
264900       MOVE ORAD-IDLEVNR         TO S-001-IDLEVNR                         
265000       MOVE KORD-KDFRAKT         TO S-001-KDFRAKT                         
265100       MOVE BILL-KDVALISO-BET    TO S-001-KDVALISO                        
265200       MOVE BILL-PRKURS-BET      TO S-001-PRKURS                          
265300       IF DIST79-DEALER-PRICE OR                                          
265400          DIST79-ECOM-PRICE                                               
265500         IF BILL-KDVALISO-FAKT NOT = BILL-KDVALISO-BET                    
265600           MOVE BILL-PRKURS-FIKTIV TO S-001-PRKURS                        
265700         END-IF                                                           
265800       END-IF                                                             
265900       MOVE KORD-KDORDKL         TO S-001-KDORDKL-MAX                     
266000       MOVE BILL-IDSHIPM         TO S-001-IDSHIPM                         
266100       MOVE 1                    TO W-POST-S                              
266200       MOVE SPACE                TO S-001-IDDC-LEV                        
266300     END-IF                                                               
266400                                                                          
266500     IF S-002-IDFAKT   = BILL-IDFAKT   AND                                
266600        S-002-IDDISTR  = BILL-IDDISTR  AND                                
266700        S-002-IDKUNDNR = BILL-IDKUNDNR AND                                
266800        S-002-IDKOLLI  = BILL-IDKOLLI  AND                                
266900        S-002-IDKUNDRF = KORD-IDKUNDRF                                    
267000       CONTINUE                                                           
267100     ELSE                                                                 
267200       MOVE '002'               TO S-002-IDPTYP                           
267300       MOVE BILL-IDFAKT         TO S-002-IDFAKT                           
267400       MOVE BILL-IDDISTR        TO S-002-IDDISTR                          
267500       MOVE BILL-IDKUNDNR       TO S-002-IDKUNDNR                         
267600       MOVE BILL-IDDC           TO S-002-IDDC                             
267700       MOVE KORD-IDKUNDRF       TO S-002-IDKUNDRF                         
267800       MOVE BILL-IDKOLLI        TO S-002-IDKOLLI                          
267900       MOVE KOLLI-KDKOLLI       TO S-002-KDKOLLI                          
268000       MOVE KOLLI-IDLBBET       TO S-002-IDLBBET                          
268100       MOVE 2                   TO W-POST-S                               
268200       PERFORM S15Y-SKRIV-W47664S                                         
268300     END-IF                                                               
268400                                                                          
268500     IF KORD-KDORDKL > S-001-KDORDKL-MAX                                  
268600        MOVE KORD-KDORDKL       TO S-001-KDORDKL-MAX                      
268700     END-IF                                                               
268800                                                                          
268900     MOVE '003'                 TO S-003-IDPTYP                           
269000     MOVE BILL-IDFAKT           TO S-003-IDFAKT                           
269100     MOVE BILL-IDDISTR          TO S-003-IDDISTR                          
269200     MOVE BILL-IDKUNDNR         TO S-003-IDKUNDNR                         
269300     MOVE BILL-IDDC             TO S-003-IDDC                             
269400     MOVE KORD-IDKUNDRF         TO S-003-IDKUNDRF                         
269500     MOVE BILL-IDKOLLI          TO S-003-IDKOLLI                          
269600     MOVE ORAD-IDARTNR          TO S-003-IDARTNR                          
269700     MOVE KKOLLI-KVLEVART       TO S-003-KVLEVART                         
269800     MOVE ORAD-PRARTNTO         TO S-003-PRARTNTO                         
269900     IF DIST79-DEALER-PRICE                                               
270000       MOVE ORAD-PRARTNTO-LOC TO S-003-PRARTNTO                           
270100       IF S-003-PRARTNTO = ZERO                                           
270200         MOVE ORAD-PRARTNTO-LOCPREL TO S-003-PRARTNTO                     
270300       END-IF                                                             
270400       IF S-003-PRARTNTO = ZERO                                           
270500         PERFORM XX-003                                                   
270600         MOVE XX-PRARTNTO       TO S-003-PRARTNTO                         
270700       END-IF                                                             
270800     ELSE                                                                 
270900       IF DIST79-ECOM-PRICE                                               
271000         MOVE ORAD-PRARTNTO-LOC TO S-003-PRARTNTO                         
271100       END-IF                                                             
271200     END-IF                                                               
271300     MOVE BILL-PRAVCOST         TO S-003-PRAVCOST                         
271400     MOVE SPACE                 TO S-003-IDDC-LEV                         
271500     MOVE 3                     TO W-POST-S                               
271600     PERFORM S15Y-SKRIV-W47664S                                           
271700                                                                          
271800     .                                                                    
271900     EJECT                                                                
272000 Z-FINIT SECTION.                                                         
272100     MOVE 'Z-FINIT'   TO WS-SEKTION                                       
272200                                                                          
272300     IF WS2-IDFAKT NOT = ZERO                                             
272400       PERFORM S22-SKRIV-RIL                                              
272500     END-IF                                                               
272600                                                                          
272700     IF RIM-POST                                                          
272800       PERFORM FC-SKRIV-RIM-POST                                          
272900     END-IF                                                               
273000                                                                          
273100     IF 001-IDPTYP = '001'                                                
273200       MOVE 1                    TO W-POST                                
273300       PERFORM S15-SKRIV-W47664                                           
273400       MOVE SPACE                TO 001-IDPTYP                            
273500     END-IF                                                               
273600                                                                          
273700     IF R-001-IDPTYP = '001'                                              
273800       MOVE 1                    TO W-POST-R                              
273900       PERFORM S15X-SKRIV-W47664R                                         
274000       MOVE SPACE                TO R-001-IDPTYP                          
274100     END-IF                                                               
274200                                                                          
274300     IF S-001-IDPTYP = '001'                                              
274400       MOVE 1                    TO W-POST-S                              
274500       PERFORM S15Y-SKRIV-W47664S                                         
274600       MOVE SPACE                TO S-001-IDPTYP                          
274700     END-IF                                                               
274800                                                                          
274900       CLOSE W47668                                                       
275000             W47670                                                       
275100             W47656                                                       
275200             W47664                                                       
275300             W47664R                                                      
275400             W47664S                                                      
275500             W47674                                                       
275600             W47665                                                       
275700             W4765H                                                       
275800     SKIP2                                                                
275900     MOVE 'S' TO POSTSUM-OPKOD                                            
276000     CALL POSTSUM USING POSTSUM-PARM                                      
276100     .                                                                    
276200     EJECT                                                                
276300 S01-HAMTA-VALUTAKURS SECTION.                                            
276400                                                                          
276500     MOVE W-DATE-AAMM        TO CURR-TIAAMM                               
276600     MOVE WS-KDVALISO-HUV    TO CURR-KDVALISO-HUV                         
276700     MOVE 'M'                TO CURR-KDVALTYP                             
276800                                                                          
276900     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
277000     IF CURR-KDSVAR = ' '                                                 
277100        MOVE CURR-PRKURS-NEW TO WS-PRKURS                                 
277200     ELSE                                                                 
277300        MOVE ZERO            TO WS-PRKURS                                 
277400     END-IF                                                               
277500     .                                                                    
277600     EJECT                                                                
277700 S04-OMVANDLA-SEK-TILL-UTL  SECTION.                                      
277800     MOVE 'S04-OMVANDLA-SEK-TILL-UTL' TO WS-SEKTION                       
277900**EO   +2 KDCALL = SEK TILL UTL  VALUTA                                   
278000     MOVE +2                 TO EXCH-KDCALL                               
278100     MOVE +0                 TO EXCH-PRARTNTO-IN                          
278200     MOVE WS-BEL             TO EXCH-SUORDV-IN                            
278300                                                                          
278400     CALL W411EXCH USING EXCH-W411EXCH                                    
278500                                                                          
278600     MOVE EXCH-SUORDV-UT     TO WS-BEL                                    
278700     .                                                                    
278800     EJECT                                                                
278900 S06-HAMTA-BOLLA-INFO   SECTION.                                          
279000     MOVE 'S06-HAMTA-BOLLA-INFO' TO WS-SEKTION                            
279100*                                                                         
279200     IF DCS-SDC AND DCS-ITALY                                             
279300                                                                          
279400       MOVE BILL-IDDC              TO W-IDDC-4491                         
279500       PERFORM IMS-GU-WDGX4491                                            
279600       MOVE VORD-TILASTN-SK        TO W-DALASTN(2:7)                      
279700       MOVE 20                     TO W-DALASTN(1:2)                      
279800       MOVE KOLLI-IDLBBET          TO W-IDLBBET                           
279900       MOVE BILL-IDKOLLI           TO W-IDKOLLI-4                         
280000       MOVE BILL-IDDISTR           TO W-IDDISTR-4                         
280100       MOVE BILL-IDKUNDNR          TO W-IDKUNDNR-4                        
280200       MOVE BILL-IDORDNR7          TO W-IDORDNR7-4                        
280300       PERFORM IMS-GNP-WDGX4494                                           
280400       IF SEGMENT-SAKNAS                                                  
280500         MOVE ZERO                 TO 4494-IDTRPBON                       
280600         MOVE SPACE                TO 4494-IDTRPBOT                       
280700       END-IF                                                             
280800     ELSE                                                                 
280900       MOVE ZERO                   TO 4494-IDTRPBON                       
281000       MOVE SPACE                  TO 4494-IDTRPBOT                       
281100     END-IF                                                               
281200     .                                                                    
281300     EJECT                                                                
281400 S07-W335PRIS  SECTION.                                                   
281500     MOVE 'S07-W335PRIS'       TO WS-SEKTION                              
281600     MOVE 1                    TO PRIS-KDCALL                             
281700     MOVE IDPGM                TO PRIS-IDPGM                              
281800     MOVE ORAD-IDARTNR         TO PRIS-IDARTNR                            
281900     MOVE BILL-IDDISTR         TO PRIS-IDDISTR                            
282000     MOVE BILL-IDKUNDNR        TO PRIS-IDKUNDNR                           
282100     MOVE BILL-IDDC            TO PRIS-IDDC                               
282200     MOVE ORAD-KDORDKL         TO PRIS-KDORDKL                            
282300     MOVE ORAD-KVBEART         TO PRIS-KVBEART                            
282400     MOVE ORAD-FLINVEST        TO PRIS-FLINVEST                           
282500     CALL W335PRIS USING PRIS-W335PRIS PRIS-WDK6-PCB PRIS-WDK7-PCB        
282600                         PRIS-WDB2-PCB PRIS-WDB1-PCB                      
282700                         PRIS-WDC1-PCB PRIS-WDC2-PCB                      
282800                         PRIS-COST-WDK6-PCB PRIS-COST-WDK7-PCB            
282900                         PRIS-COST-WDF1-PCB PRIS-COST-9305-PCB            
283000                         PRIS-COST-WDK72-PCB                              
283100                         PRIS-COST-WDB6-PCB                               
283200     IF PRIS-KDSVAR = '2'                                                 
283300        MOVE 'DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'                
283400                               TO FELTEXT                                 
283500        CALL ABEND USING RKOD-ABEND                                       
283600     END-IF                                                               
283700     .                                                                    
283800     EJECT                                                                
283900 S08-W335PRIS  SECTION.                                                   
284000     MOVE 'S08-W335PRIS'       TO WS-SEKTION                              
284100     MOVE 1                    TO PRIS-KDCALL                             
284200     MOVE IDPGM                TO PRIS-IDPGM                              
284300     MOVE ORAD-IDARTNR         TO PRIS-IDARTNR                            
284400     MOVE DCS-IDDISTR-REFILL   TO PRIS-IDDISTR                            
284500     MOVE 0                    TO PRIS-IDKUNDNR                           
284600     MOVE WC-CDC-SE            TO PRIS-IDDC                               
284700*--  DETTA DC GÄLLER BARA FÖR CN OCH IN (REFILLANDE DC = 11)              
284800     MOVE 4                    TO PRIS-KDORDKL                            
284900     MOVE 1                    TO PRIS-KVBEART                            
285000     MOVE SPACE                TO PRIS-FLINVEST                           
285100     CALL W335PRIS USING PRIS-W335PRIS PRIS-WDK6-PCB PRIS-WDK7-PCB        
285200                         PRIS-WDB2-PCB PRIS-WDB1-PCB                      
285300                         PRIS-WDC1-PCB PRIS-WDC2-PCB                      
285400                         PRIS-COST-WDK6-PCB PRIS-COST-WDK7-PCB            
285500                         PRIS-COST-WDF1-PCB PRIS-COST-9305-PCB            
285600                         PRIS-COST-WDK72-PCB                              
285700                         PRIS-COST-WDB6-PCB                               
285800     IF PRIS-KDSVAR = '2'                                                 
285900        MOVE 'DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'                
286000                               TO FELTEXT                                 
286100        CALL ABEND USING RKOD-ABEND                                       
286200     END-IF                                                               
286300     .                                                                    
286400     EJECT                                                                
286500 S09-BYT-KDVALISO-TILL-KDVALUTA SECTION.                                  
286600     MOVE 'S09-BYT-KDVALISO'        TO WS-SEKTION                         
286700     MOVE ZERO                      TO RIK-KDVALUTA                       
286800     MOVE +1                        TO TAB-IX                             
286900     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
287000      IF DIST79-DEALER-PRICE                                              
287100       IF BILL-KDVALISO-FAKT = TAB-KDVALISO (TAB-IX)                      
287200         MOVE TAB-KDVALUTA (TAB-IX) TO RIK-KDVALUTA                       
287300         MOVE TAB-IX-MAX            TO TAB-IX                             
287400       END-IF                                                             
287500      ELSE                                                                
287600       IF BILL-KDVALISO-BET  = TAB-KDVALISO (TAB-IX)                      
287700         MOVE TAB-KDVALUTA (TAB-IX) TO RIK-KDVALUTA                       
287800         MOVE TAB-IX-MAX            TO TAB-IX                             
287900       END-IF                                                             
288000      END-IF                                                              
288100      ADD +1 TO TAB-IX                                                    
288200     END-PERFORM                                                          
288300     .                                                                    
288400     EJECT                                                                
288500 S10-SPARA-ID   SECTION.                                                  
288600*    DISPLAY 'S10'                                                        
288700     MOVE BILL-IDFAKT              TO WS2-IDFAKT                          
288800     MOVE BILL-IDDISTR             TO WS2-IDDISTR                         
288900     MOVE BILL-IDKUNDNR            TO WS2-IDKUNDNR                        
289000     MOVE BILL-IDORDNR7            TO WS2-IDORDNR7                        
289100     MOVE BILL-IDPRODNR            TO WS2-IDPRODNR                        
289200     MOVE BILL-IDKOLLI             TO WS2-IDKOLLI                         
289300     .                                                                    
289400     EJECT                                                                
289500 S11-SKRIV-W47668 SECTION.                                                
289600*    DISPLAY 'S11'                                                        
289700     WRITE UT-POST FROM RJX-W4766601                                      
289800                                                                          
289900     MOVE RJX-IDPTYP      TO POSTSUM-TRANSTYP                             
290000     MOVE 'W47668'        TO POSTSUM-FDNAMN                               
290100     MOVE 'W47668D1'      TO POSTSUM-DDNAMN2                              
290200     CALL POSTSUM USING POSTSUM-PARM                                      
290300     .                                                                    
290400     EJECT                                                                
290500 S13-SKRIV-W47670 SECTION.                                                
290600*    DISPLAY 'S13'                                                        
290700     WRITE UT3-POST FROM PL-BILL-W4767001                                 
290800                                                                          
290900     MOVE SPACE           TO POSTSUM-TRANSTYP                             
291000     MOVE 'W47670'        TO POSTSUM-FDNAMN                               
291100     MOVE 'W47668D3'      TO POSTSUM-DDNAMN2                              
291200     CALL POSTSUM USING POSTSUM-PARM                                      
291300     .                                                                    
291400     EJECT                                                                
291500 S14-SKRIV-W47656 SECTION.                                                
291600*    DISPLAY 'S14'                                                        
291700     WRITE UT4-POST FROM UT4-AREA                                         
291800                                                                          
291900     MOVE SPACE           TO POSTSUM-TRANSTYP                             
292000     MOVE 'W47656'        TO POSTSUM-FDNAMN                               
292100     MOVE 'W47668D4'      TO POSTSUM-DDNAMN2                              
292200     CALL POSTSUM USING POSTSUM-PARM                                      
292300     .                                                                    
292400     EJECT                                                                
292500 S15-SKRIV-W47664 SECTION.                                                
292600*    DISPLAY 'S15'                                                        
292700     EVALUATE W-POST                                                      
292800       WHEN 1                                                             
292900         WRITE UT5-POST FROM 001-W4766401                                 
293000       WHEN 2                                                             
293100         WRITE UT5-POST FROM 002-W4766402                                 
293200       WHEN 3                                                             
293300         WRITE UT5-POST FROM 003-W4766403                                 
293400     END-EVALUATE                                                         
293500                                                                          
293600     MOVE SPACE           TO POSTSUM-TRANSTYP                             
293700     MOVE 'W47664'        TO POSTSUM-FDNAMN                               
293800     MOVE 'W47668D5'      TO POSTSUM-DDNAMN2                              
293900     CALL POSTSUM USING POSTSUM-PARM                                      
294000     .                                                                    
294100     EJECT                                                                
294200 S15X-SKRIV-W47664R SECTION.                                              
294300*    DISPLAY 'S15X'                                                       
294400     EVALUATE W-POST-R                                                    
294500       WHEN 1                                                             
294600         WRITE UT9-POST FROM R-001-W4766401                               
294700       WHEN 2                                                             
294800         WRITE UT9-POST FROM R-002-W4766402                               
294900       WHEN 3                                                             
295000         WRITE UT9-POST FROM R-003-W4766403                               
295100     END-EVALUATE                                                         
295200                                                                          
295300     MOVE SPACE           TO POSTSUM-TRANSTYP                             
295400     MOVE 'W47664R'       TO POSTSUM-FDNAMN                               
295500     MOVE 'W47668D9'      TO POSTSUM-DDNAMN2                              
295600     CALL POSTSUM USING POSTSUM-PARM                                      
295700     .                                                                    
295800     EJECT                                                                
295900 S15Y-SKRIV-W47664S SECTION.                                              
296000*    DISPLAY 'S15Y'                                                       
296100     EVALUATE W-POST-S                                                    
296200       WHEN 1                                                             
296300         WRITE UTA-POST FROM S-001-W4766401                               
296400       WHEN 2                                                             
296500         WRITE UTA-POST FROM S-002-W4766402                               
296600       WHEN 3                                                             
296700         WRITE UTA-POST FROM S-003-W4766403                               
296800     END-EVALUATE                                                         
296900                                                                          
297000     MOVE SPACE           TO POSTSUM-TRANSTYP                             
297100     MOVE 'W47664S'       TO POSTSUM-FDNAMN                               
297200     MOVE 'W47668DA'      TO POSTSUM-DDNAMN2                              
297300     CALL POSTSUM USING POSTSUM-PARM                                      
297400     .                                                                    
297500     EJECT                                                                
297600 S16-SKRIV-W47674 SECTION.                                                
297700*    DISPLAY 'S16'                                                        
297800     WRITE UT6-POST FROM UT6-AREA                                         
297900                                                                          
298000     MOVE SPACE           TO POSTSUM-TRANSTYP                             
298100     MOVE 'W47674'        TO POSTSUM-FDNAMN                               
298200     MOVE 'W47668D6'      TO POSTSUM-DDNAMN2                              
298300     CALL POSTSUM USING POSTSUM-PARM                                      
298400     .                                                                    
298500     EJECT                                                                
298600 S17-SKRIV-W47665 SECTION.                                                
298700*    DISPLAY 'S17'                                                        
298800     WRITE UT7-POST FROM UT7-AREA                                         
298900                                                                          
299000     MOVE SPACE           TO POSTSUM-TRANSTYP                             
299100     MOVE 'W47665'        TO POSTSUM-FDNAMN                               
299200     MOVE 'W47668D7'      TO POSTSUM-DDNAMN2                              
299300     CALL POSTSUM USING POSTSUM-PARM                                      
299400     .                                                                    
299500     EJECT                                                                
299600 S20-INFIL-TILL-BILL  SECTION.                                            
299700      MOVE 'S20-INFIL'  TO WS-SEKTION                                     
299800                                                                          
299900      MOVE 4510-IDPRODNR      TO BILL-IDPRODNR                            
300000      MOVE 4510-IDKOLLI       TO BILL-IDKOLLI                             
300100      MOVE 4510-IDPURAD       TO BILL-IDPURAD                             
300200      MOVE 4510-DAFINDOC      TO BILL-DAFINDOC                            
300300                                 SPAR-DAFAKT                              
300400      MOVE 4510-IDDISTR       TO BILL-IDDISTR                             
300500      MOVE 4510-IDKUNDNR      TO BILL-IDKUNDNR                            
300600      MOVE 4508-IDFAKT        TO BILL-IDFAKT                              
300700      MOVE 4510-IDPARTNR      TO BILL-IDPARTNR                            
300800      MOVE 4510-IDSHIPM       TO BILL-IDSHIPM                             
300900      MOVE 4510-IDORDNR5      TO BILL-IDORDNR7                            
301000      MOVE 4510-IDDC          TO BILL-IDDC                                
301100      MOVE 4510-TISKEPPN      TO BILL-TISKEPPN                            
301200      MOVE 4510-KDVALISO      TO BILL-KDVALISO-FAKT                       
301300      MOVE 4510-SUBTO-TOT     TO BILL-SUBTO-TOT                           
301400      MOVE 4510-SUNTO-TOT     TO BILL-SUNTO-TOT                           
301500      MOVE 4510-SUVAT-FAKT    TO BILL-SUVAT-FAKT                          
301600      MOVE 4510-BEART         TO BILL-BEART                               
301700      MOVE 4510-VKARTNTO      TO BILL-VKARTNTO                            
301800      MOVE 4510-SUBTO-LINE    TO BILL-SUBTO-LINE                          
301900      MOVE 4510-SUNTO-LINE    TO BILL-SUNTO-LINE                          
302000      MOVE 4510-SUVAT-LINE    TO BILL-SUVAT-LINE                          
302100      MOVE 4510-IDFKNGRP      TO BILL-IDFKNGRP                            
302200      MOVE 4510-KDARTRAB      TO BILL-KDARTRAB                            
302300      MOVE 4510-KDPRODSL      TO BILL-KDPRODSL                            
302400      MOVE 4510-PRAVCOST      TO BILL-PRAVCOST                            
302500      MOVE 4510-PRAVCOST-CORE TO BILL-PRAVCOST-CORE                       
302600      MOVE 4510-KDVALISO-BET  TO BILL-KDVALISO-BET                        
302700      MOVE 4510-PRKURS-BET    TO BILL-PRKURS-BET                          
302800      MOVE 4510-PRKURS-FAKT   TO BILL-PRKURS-FAKT                         
302900      MOVE 4510-PRKURS-FIKTIV TO BILL-PRKURS-FIKTIV                       
303000                                                                          
303100      MOVE 4510-KDFAKSTA-EXP  TO BILL-KDFAKSTA-EXP                        
303200      MOVE 4510-SUNTO-PART-LOC                                            
303300                              TO BILL-SUNTO-PART-LOC                      
303400      MOVE 4510-SUVAT-BILLIT-TOT-PART-L                                   
303500                              TO BILL-SUVAT-BILLIT-TOT-PART-L             
303600      MOVE 4510-SUBTO-TOT-PART-LOC                                        
303700                              TO BILL-SUBTO-TOT-PART-LOC                  
303800      MOVE 4510-SUNTO-TOT-LOC TO BILL-SUNTO-TOT-LOC                       
303900      MOVE 4510-SUVAT-BILLIT-TOT-LOC                                      
304000                              TO BILL-SUVAT-BILLIT-TOT-LOC                
304100      MOVE 4510-SUBTO-TOT-LOC TO BILL-SUBTO-TOT-LOC                       
304200      MOVE 4510-KDVALISO-LOC  TO BILL-KDVALISO-LOC                        
304300      MOVE 4510-PRKURS-LOC    TO BILL-PRKURS-LOC                          
304400      MOVE 4510-KDTECKEN-LOC  TO BILL-KDTECKEN-LOC                        
304500      MOVE 4510-SUNTO-LOCC    TO BILL-SUNTO-LOCC                          
304600      MOVE 4510-SUNTO-PART-RECALC                                         
304700                              TO BILL-SUNTO-PART-RECALC                   
304800      MOVE 4510-SUVAT-BILLIT-TOT-PART-R                                   
304900                              TO BILL-SUVAT-BILLIT-TOT-PART-R             
305000      MOVE 4510-SUBTO-TOT-PART-RECALC                                     
305100                              TO BILL-SUBTO-TOT-PART-RECALC               
305200      MOVE 4510-SUNTO-TOT-RECALC                                          
305300                              TO BILL-SUNTO-TOT-RECALC                    
305400      MOVE 4510-SUVAT-BILLIT-TOT-RECALC                                   
305500                              TO BILL-SUVAT-BILLIT-TOT-RECALC             
305600      MOVE 4510-SUBTO-TOT-RECALC                                          
305700                              TO BILL-SUBTO-TOT-RECALC                    
305800      MOVE 4510-KDVALISO-RECALC                                           
305900                              TO BILL-KDVALISO-RECALC                     
306000      MOVE 4510-PRKURS-RECALC TO BILL-PRKURS-RECALC                       
306100      MOVE 4510-KDTECKEN-RECALC                                           
306200                              TO BILL-KDTECKEN-RECALC                     
306300      MOVE 4510-SUNTO-LOCC-RECALC                                         
306400                              TO BILL-SUNTO-LOCC-RECALC                   
306500      MOVE 4510-PRAVCOST-BILLIT                                           
306600                              TO BILL-PRAVCOST-BILLIT                     
306700      MOVE 4510-KDVALISO-AVC  TO BILL-KDVALISO-AVC                        
306800      MOVE 4510-PRARTNTO      TO BILL-PRARTNTO                            
306900      MOVE 4510-PRARTNTO-LOC  TO BILL-PRARTNTO-LOC                        
307000      MOVE 4510-KDVALISO-NTO  TO BILL-KDVALISO-NTO                        
307100      MOVE 4510-IDDC          TO BILL-IDDC                                
307200      MOVE 4510-IDDC-BILLIT   TO BILL-IDDC-BILLIT                         
307300      MOVE 4510-KVLEVART      TO BILL-KVLEVART                            
307400      MOVE 4510-KDARTURS      TO BILL-KDARTURS                            
307500      MOVE 4510-FLPCOO        TO BILL-FLPCOO                              
307600*                                                                         
307700*     IDVAT - FOR EV. FUTURE NEEDS (ONLY ON COPYBOOK W4760001)            
307800      MOVE SPACE              TO BILL-IDVAT-LEG                           
307900                                 BILL-IDVAT-RESP                          
308000                                 BILL-IDVAT-BET                           
308100                                 BILL-IDVAT-AGENT                         
308200                                 BILL-IDVAT-DDGS-RESP                     
308300                                                                          
308400     .                                                                    
308500     EJECT                                                                
308600 S21-NOLLA-RIL  SECTION.                                                  
308700     MOVE 'RIL'                 TO RIL-IDPTYP                             
308800     MOVE ZERO                  TO RIL-PRAVDRAG                           
308900                                   RIL-PREMBHNT                           
309000                                   RIL-PRFOERS                            
309100                                   RIL-PRFRAKT                            
309200                                   RIL-PRLEGKST                           
309300                                   RIL-PRMOMS                             
309400                                   RIL-SUFKTTILL                          
309500                                   RIL-KDFRAKT                            
309600     MOVE SPACE                 TO RIL-FILLERX12                          
309700     .                                                                    
309800     EJECT                                                                
309900 S22-SKRIV-RIL  SECTION.                                                  
310000     MOVE 'S22-SKRIV-RIL'        TO WS-SEKTION                            
310100                                                                          
310200       MOVE 'RIL'                TO RJX-IDPTYP                            
310300       MOVE RIL-W461RILN-CTX     TO RJX-FILLER                            
310400       MOVE WS2-IDFAKT           TO RJX-IDFAKT                            
310500       MOVE WS2-IDDISTR          TO RJX-IDDISTR                           
310600       MOVE ZERO                 TO RJX-IDKUNDNR                          
310700       MOVE ZERO                 TO RJX-IDORDER                           
310800       MOVE ZERO                 TO RJX-IDKUNDNR-S                        
310900       MOVE ZERO                 TO RJX-IDPRODNR                          
311000       MOVE ZERO                 TO RJX-IDKOLLI                           
311100       MOVE ZERO                 TO RJX-IDPURAD                           
311200       MOVE ZERO                 TO RJX-IDTRPBON                          
311300*                                                                         
311400       PERFORM S11-SKRIV-W47668                                           
311500     .                                                                    
311600     EJECT                                                                
311700 S40-HAMTA-PRAVCOST  SECTION.                                             
311800     MOVE BILL-IDDISTR          TO TEST-IDDISTR                           
311900     MOVE KORD-IDDC             TO W-IDDC                                 
312000     IF KORD-IDDC NOT = DCS-IDDC                                          
312100       MOVE KORD-IDDC           TO W-IDDC-B6                              
312200       PERFORM IMS-GU-WDB601                                              
312300     END-IF                                                               
312400     MOVE ORAD-IDARTNR          TO W-IDARTNR                              
312500     IF DCS-NDC-NA  OR                                                    
312600        ((DCS-CDC OR DCS-DDC)AND (DIST07-USA-RETAILER)) OR                
312700        ((DCS-CDC OR DCS-DDC)AND (DIST07-CAN-RETAILER))                   
312800        IF (DCS-CDC OR DCS-DDC) AND (DIST07-USA-RETAILER)                 
312900           MOVE  WC-NDC-US-RU           TO W-IDDC                         
313000        ELSE                                                              
313100          IF (DCS-CDC OR DCS-DDC) AND (DIST07-CAN-RETAILER)               
313200             MOVE WC-NDC-CA           TO W-IDDC                           
313300          END-IF                                                          
313400        END-IF                                                            
313500        PERFORM IMS-GU-WDK711                                             
313600        IF SEGMENT-FINNS                                                  
313700          MOVE SLAG-PRAVCOST    TO BILL-PRAVCOST                          
313800        END-IF                                                            
313900        MOVE W-IDARTNR          TO TEST-ARTIKEL                           
314000                                   W-IDARTNR-CORE                         
314100        IF BYT19-BYTES OR BYT19-RADIO                                     
314200          IF BYT19-BYTES                                                  
314300           ADD +6000            TO W-IDARTNR-CORE                         
314400          ELSE                                                            
314500           IF BYT19-RADIO                                                 
314600             ADD +1000          TO W-IDARTNR-CORE                         
314700           END-IF                                                         
314800          END-IF                                                          
314900          PERFORM IMS-GU-WDK711-CORE                                      
315000          IF SEGMENT-SAKNAS                                               
315100            MOVE ZERO           TO BILL-PRAVCOST-CORE                     
315200          ELSE                                                            
315300            MOVE CORE-SLAG-PRAVCOST TO BILL-PRAVCOST-CORE                 
315400          END-IF                                                          
315500        ELSE                                                              
315600           MOVE ZERO            TO BILL-PRAVCOST-CORE                     
315700        END-IF                                                            
315800     END-IF                                                               
315900     .                                                                    
316000     EJECT                                                                
316100 S50-HAMTA-USA-FRAKT SECTION.                                             
316200                                                                          
316300     MOVE BILL-IDSHIPM          TO W-IDSHIPM                              
316400     MOVE BILL-IDDISTR          TO W-IDDISTR                              
316500     MOVE BILL-IDKUNDNR         TO W-IDKUNDNR                             
316600     PERFORM IMS-GU-WDE111                                                
316700     IF SEGMENT-FINNS                                                     
316800      PERFORM IMS-GNP-WDE122                                              
316900      IF SEGMENT-FINNS                                                    
317000       IF TILL-PRFRAKT NOT = ZERO                                         
317100        MOVE TILL-PRFRAKT  TO SPAR-PRFRAKT-LOC                            
317200       END-IF                                                             
317300      END-IF                                                              
317400     END-IF                                                               
317500     .                                                                    
317600    EJECT                                                                 
317700* --- IMS SEKTIONER ---                                                   
317800                                                                          
317900 IMS-GU-WDE411-BSEQ SECTION.                                              
318000     MOVE 'IMS-GU-WDE411'   TO WS-SEKTION                                 
318100                                                                          
318200     STRING 'WDE411  (WDE4BSEQ =' W-WDE4BSEQ-X ') '                       
318300          DELIMITED BY SIZE INTO SSA1                                     
318400     MOVE '    '            TO GODK-STATUSKODER                           
318500     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE411 SSA1                    
318600     MOVE WDE4-STATUS-CODE  TO STATUS-WS                                  
318700     PERFORM IMS-STATUSKONTROLL                                           
318800     .                                                                    
318900     SKIP3                                                                
319000 IMS-GNP-WDE401 SECTION.                                                  
319100     MOVE 'IMS-GNP-WDE401'   TO WS-SEKTION                                
319200     MOVE 'WDE401'                 TO SSA1                                
319300     MOVE '    '                   TO GODK-STATUSKODER                    
319400     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-WDE401 SSA1                   
319500     MOVE WDE4-STATUS-CODE         TO STATUS-WS                           
319600     PERFORM IMS-STATUSKONTROLL                                           
319700     .                                                                    
319800     SKIP2                                                                
319900 IMS-GNP-WDE421  SECTION.                                                 
320000     MOVE 'IMS-GNP-WDE421'    TO WS-SEKTION                               
320100                                                                          
320200     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
320300          DELIMITED BY SIZE INTO SSA1                                     
320400     MOVE '    '              TO GODK-STATUSKODER                         
320500     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-WDE421 SSA1                   
320600     MOVE WDE4-STATUS-CODE    TO STATUS-WS                                
320700     PERFORM IMS-STATUSKONTROLL                                           
320800     .                                                                    
320900     SKIP3                                                                
321000 IMS-GU-WDE401-ESEQ SECTION.                                              
321100     MOVE 'GU-WDE401-ESEQ'  TO WS-SEKTION                                 
321200     STRING 'WDE401  (WDE4ESEQ =' W-WDE4ESEQ-X ')'                        
321300          DELIMITED BY SIZE INTO SSA1                                     
321400     MOVE '    '              TO GODK-STATUSKODER                         
321500     CALL CBLTDLI USING GU WDE4X-PCB DLI-IO-WDE401 SSA1                   
321600     MOVE WDE4X-STATUS-CODE   TO STATUS-WS                                
321700     PERFORM IMS-STATUSKONTROLL                                           
321800     .                                                                    
321900     SKIP3                                                                
322000 IMS-GNP-WDE411-ESEQ SECTION.                                             
322100     MOVE 'GNP-WDE411-ESEQ'   TO WS-SEKTION                               
322200     MOVE 'WDE411'     TO SSA1                                            
322300     MOVE '    '              TO GODK-STATUSKODER                         
322400     CALL CBLTDLI USING GNP WDE4X-PCB DLI-IO-WDE411 SSA1                  
322500     MOVE WDE4X-STATUS-CODE   TO STATUS-WS                                
322600     PERFORM IMS-STATUSKONTROLL                                           
322700     .                                                                    
322800     EJECT                                                                
322900 IMS-GU-WDE601 SECTION.                                                   
323000*    DISPLAY 'E601'                                                       
323100     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-E6-X ')'                     
323200            DELIMITED BY SIZE INTO SSA1                                   
323300     MOVE '  '                     TO GODK-STATUSKODER                    
323400     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
323500     MOVE WDE6-STATUS-CODE         TO STATUS-WS                           
323600     PERFORM IMS-STATUSKONTROLL                                           
323700     .                                                                    
323800     SKIP2                                                                
323900 IMS-GNP-WDE611 SECTION.                                                  
324000     MOVE 'IMS-GHNP-WDE611'     TO WS-SEKTION                             
324100     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-E6-X ')'                      
324200            DELIMITED BY SIZE INTO SSA1                                   
324300     MOVE '  '                     TO GODK-STATUSKODER                    
324400     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-WDE611 SSA1                   
324500     MOVE WDE6-STATUS-CODE         TO STATUS-WS                           
324600     PERFORM IMS-STATUSKONTROLL                                           
324700     .                                                                    
324800     SKIP3                                                                
324900 IMS-GU-WDK611 SECTION.                                                   
325000     MOVE 'IMS-GU-WDK611'      TO WS-SEKTION                              
325100*                                                                         
325200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
325300          DELIMITED BY SIZE INTO SSA1                                     
325400     MOVE 'WDK611 '           TO SSA2                                     
325500     MOVE '    '              TO GODK-STATUSKODER                         
325600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
325700     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
325800     PERFORM IMS-STATUSKONTROLL                                           
325900     .                                                                    
326000     SKIP3                                                                
326100 IMS-GU-WDK711 SECTION.                                                   
326200     MOVE 'IMS-GU-WDK711'    TO WS-SEKTION                                
326300                                                                          
326400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
326500          DELIMITED BY SIZE INTO SSA1                                     
326600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
326700          DELIMITED BY SIZE INTO SSA2                                     
326800     MOVE '  GE' TO GODK-STATUSKODER                                      
326900     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
327000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
327100     PERFORM IMS-STATUSKONTROLL                                           
327200     .                                                                    
327300     EJECT                                                                
327400 IMS-GU-WDK711-CORE SECTION.                                              
327500     MOVE 'IMS-GU-WDK711-CORE'    TO WS-SEKTION                           
327600                                                                          
327700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-CORE-X ')'                    
327800          DELIMITED BY SIZE INTO SSA1                                     
327900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
328000          DELIMITED BY SIZE INTO SSA2                                     
328100     MOVE '  GE' TO GODK-STATUSKODER                                      
328200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711-CORE SSA1 SSA2          
328300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
328400     PERFORM IMS-STATUSKONTROLL                                           
328500     .                                                                    
328600     EJECT                                                                
328700 IMS-GU-WDK712 SECTION.                                                   
328800     MOVE 'IMS-GU-WDK712'    TO WS-SEKTION                                
328900                                                                          
329000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
329100          DELIMITED BY SIZE INTO SSA1                                     
329200     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
329300          DELIMITED BY SIZE INTO SSA2                                     
329400     MOVE '  GE' TO GODK-STATUSKODER                                      
329500     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
329600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
329700     PERFORM IMS-STATUSKONTROLL                                           
329800     .                                                                    
329900     EJECT                                                                
330000 IMS-GU-WDB101 SECTION.                                                   
330100     MOVE 'IMS-GU-WDB101'       TO WS-SEKTION                             
330200*                                                                         
330300     STRING 'WDB101  (IDPARTNR =' W-IDPARTNR-X ')'                        
330400          DELIMITED BY SIZE INTO SSA1                                     
330500     MOVE '    '                TO GODK-STATUSKODER                       
330600     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
330700     MOVE WDB1-STATUS-CODE      TO STATUS-WS                              
330800     PERFORM IMS-STATUSKONTROLL                                           
330900     .                                                                    
331000     SKIP3                                                                
331100 IMS-GN-WDB101 SECTION.                                                   
331200     MOVE 'IMS-GU-WDB101'       TO WS-SEKTION                             
331300*                                                                         
331400     STRING 'WDB101  (IDPARTNR =' W-IDPARTNR-X ')'                        
331500          DELIMITED BY SIZE INTO SSA1                                     
331600     MOVE '    '                TO GODK-STATUSKODER                       
331700     CALL CBLTDLI USING GN WDB1-PCB DLI-IO-WDB101 SSA1                    
331800     MOVE WDB1-STATUS-CODE      TO STATUS-WS                              
331900     PERFORM IMS-STATUSKONTROLL                                           
332000     .                                                                    
332100     SKIP3                                                                
332200 IMS-GU-WDB201 SECTION.                                                   
332300     MOVE 'IMS-GU-WDB201'       TO WS-SEKTION                             
332400                                                                          
332500     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
332600          DELIMITED BY SIZE INTO SSA1                                     
332700     MOVE '    '                TO GODK-STATUSKODER                       
332800     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
332900     MOVE WDB2-STATUS-CODE      TO STATUS-WS                              
333000     PERFORM IMS-STATUSKONTROLL                                           
333100     .                                                                    
333200     SKIP3                                                                
333300 IMS-GU-WDB201A  SECTION.                                                 
333400     MOVE 'IMS-GU-WDB201A'       TO WS-SEKTION                            
333500                                                                          
333600     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
333700                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
333800          DELIMITED BY SIZE INTO SSA1                                     
333900     MOVE '    ' TO GODK-STATUSKODER                                      
334000     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
334100     MOVE WDB2-STATUS-CODE      TO STATUS-WS                              
334200     PERFORM IMS-STATUSKONTROLL                                           
334300     .                                                                    
334400     SKIP3                                                                
334500 IMS-GU-WDQ201 SECTION.                                                   
334600     MOVE 'GU-WDQ201'     TO WS-SEKTION                                   
334700                                                                          
334800     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
334900            DELIMITED BY SIZE INTO SSA1                                   
335000     MOVE '  GE'                   TO GODK-STATUSKODER                    
335100     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
335200     MOVE WDQ2-STATUS-CODE         TO STATUS-WS                           
335300     PERFORM IMS-STATUSKONTROLL                                           
335400     .                                                                    
335500     SKIP2                                                                
335600 IMS-GU-WDGX4491  SECTION.                                                
335700     MOVE 'IMS-GU-WDGX4491'  TO WS-SEKTION                                
335800                                                                          
335900     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4491-X ')'                    
336000          DELIMITED BY SIZE INTO SSA1                                     
336100     MOVE '  GE'                 TO GODK-STATUSKODER                      
336200     CALL CBLTDLI USING GU 4494-PCB DLI-IO-4494 SSA1                      
336300     MOVE 4494-STATUS-CODE       TO STATUS-WS                             
336400     PERFORM IMS-STATUSKONTROLL                                           
336500     .                                                                    
336600     SKIP3                                                                
336700 IMS-GNP-WDGX4494 SECTION.                                                
336800     MOVE 'IMS-GNP-WDGX4494'  TO WS-SEKTION                               
336900                                                                          
337000     STRING 'WDGX4494*F(DALASTN  =' W-DALASTN-X                           
337100                      '&IDGMTREF =' W-IDGMTREF-X                          
337200                      '&IDLBBET  =' W-IDLBBET-X                           
337300                      '&IDKOLLI  =' W-IDKOLLI-4-X ')'                     
337400          DELIMITED BY SIZE INTO SSA1                                     
337500     MOVE '  GE'                 TO GODK-STATUSKODER                      
337600     CALL CBLTDLI USING GNP 4494-PCB DLI-IO-4494 SSA1                     
337700     MOVE 4494-STATUS-CODE       TO STATUS-WS                             
337800     PERFORM IMS-STATUSKONTROLL                                           
337900     .                                                                    
338000     SKIP3                                                                
338100 IMS-GU-WDGX4507  SECTION.                                                
338200     MOVE 'GU-WDGX4507'     TO WS-SEKTION                                 
338300                                                                          
338400     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4507-X ')'                    
338500          DELIMITED BY SIZE INTO SSA1                                     
338600     MOVE '    '                 TO GODK-STATUSKODER                      
338700     CALL CBLTDLI USING GU 4507-PCB DLI-IO-4510 SSA1                      
338800     MOVE 4507-STATUS-CODE       TO STATUS-WS                             
338900     PERFORM IMS-STATUSKONTROLL                                           
339000     .                                                                    
339100     SKIP3                                                                
339200 IMS-GHNP-WDGX4508  SECTION.                                              
339300     MOVE 'GHNP-WDGX4508'   TO WS-SEKTION                                 
339400                                                                          
339500     STRING 'WDGX4508(IDFAKT  >=' W-IDFAKT-4508-MIN-X                     
339600                    '&IDFAKT  <=' W-IDFAKT-4508-MAX-X                     
339700                    '&FLKLAR   =' W-FLKLAR-X ')'                          
339800          DELIMITED BY SIZE INTO SSA1                                     
339900     MOVE '  GE'               TO GODK-STATUSKODER                        
340000     CALL CBLTDLI USING GHNP 4507-PCB DLI-IO-4508 SSA1                    
340100     MOVE 4507-STATUS-CODE       TO STATUS-WS                             
340200     PERFORM IMS-STATUSKONTROLL                                           
340300     .                                                                    
340400     SKIP3                                                                
340500 IMS-GHNP-WDGX4508-KVAL  SECTION.                                         
340600     MOVE 'GHNP-WDGX4508-KVAL'   TO WS-SEKTION                            
340700                                                                          
340800     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4507-X ')'                    
340900          DELIMITED BY SIZE INTO SSA1                                     
341000     STRING 'WDGX4508*F(IDFAKT   =' W-IDFAKT-4508-X ')'                   
341100          DELIMITED BY SIZE INTO SSA2                                     
341200     MOVE '      '               TO GODK-STATUSKODER                      
341300     CALL CBLTDLI USING GHNP 4507-PCB DLI-IO-4508 SSA1 SSA2               
341400     MOVE 4507-STATUS-CODE       TO STATUS-WS                             
341500     PERFORM IMS-STATUSKONTROLL                                           
341600     .                                                                    
341700     SKIP3                                                                
341800 IMS-DLET-WDGX4508-4510  SECTION.                                         
341900     MOVE 'DLET-WDGX4508'   TO WS-SEKTION                                 
342000                                                                          
342100     MOVE '      '               TO GODK-STATUSKODER                      
342200     CALL CBLTDLI USING DLET 4507-PCB DLI-IO-4508                         
342300     MOVE 4507-STATUS-CODE       TO STATUS-WS                             
342400     PERFORM IMS-STATUSKONTROLL                                           
342500     .                                                                    
342600     SKIP3                                                                
342700 IMS-GHNP-WDGX4510  SECTION.                                              
342800     MOVE 'GHNP-WDGX4510'   TO WS-SEKTION                                 
342900                                                                          
343000     STRING 'WDGX4508(IDFAKT   =' W-IDFAKT-4508-X ')'                     
343100          DELIMITED BY SIZE INTO SSA1                                     
343200     MOVE   'WDGX4510'     TO SSA2                                        
343300     MOVE '  GE'               TO GODK-STATUSKODER                        
343400     CALL CBLTDLI USING GHNP 4507-PCB DLI-IO-4510 SSA1 SSA2               
343500     MOVE 4507-STATUS-CODE       TO STATUS-WS                             
343600     PERFORM IMS-STATUSKONTROLL                                           
343700     .                                                                    
343800     SKIP3                                                                
343900 IMS-GU-WDE111 SECTION.                                                   
344000     MOVE 'IMS-GU-WDE111'   TO WS-SEKTION                                 
344100                                                                          
344200     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
344300          DELIMITED BY SIZE INTO SSA1                                     
344400     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
344500          DELIMITED BY SIZE INTO SSA2                                     
344600     MOVE '  GE' TO GODK-STATUSKODER                                      
344700     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE111 SSA1 SSA2               
344800     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
344900     PERFORM IMS-STATUSKONTROLL                                           
345000     .                                                                    
345100     EJECT                                                                
345200 IMS-GU-WDE131   SECTION.                                                 
345300     MOVE 'IMS-GU-WDE131'   TO WS-SEKTION                                 
345400                                                                          
345500     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
345600          DELIMITED BY SIZE INTO SSA1                                     
345700     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
345800          DELIMITED BY SIZE INTO SSA2                                     
345900     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
346000          DELIMITED BY SIZE INTO SSA3                                     
346100     STRING 'WDE131  (IDPURAD  =' W-IDPURAD-X ')'                         
346200          DELIMITED BY SIZE INTO SSA4                                     
346300     MOVE '  ' TO GODK-STATUSKODER                                        
346400     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE131 SSA1 SSA2               
346500                                                  SSA3 SSA4               
346600     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
346700     PERFORM IMS-STATUSKONTROLL                                           
346800     .                                                                    
346900 IMS-GNP-WDE141   SECTION.                                                
347000     MOVE 'IMS-GU-WDE141'   TO WS-SEKTION                                 
347100                                                                          
347200     MOVE 'WDE141 ' TO SSA1                                               
347300     MOVE '  GE' TO GODK-STATUSKODER                                      
347400     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE141 SSA1                   
347500     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
347600     PERFORM IMS-STATUSKONTROLL                                           
347700     .                                                                    
347800 IMS-GNP-WDE122 SECTION.                                                  
347900     MOVE 'IMS-GNP-WDE122'    TO WS-SEKTION                               
348000                                                                          
348100     MOVE 'WDE122'      TO SSA1                                           
348200     MOVE '    ' TO GODK-STATUSKODER                                      
348300     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE122 SSA1                   
348400     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
348500     PERFORM IMS-STATUSKONTROLL                                           
348600     .                                                                    
348700     EJECT                                                                
348800 IMS-GU-WDB601    SECTION.                                                
348900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
349000          DELIMITED BY SIZE INTO SSA1                                     
349100     MOVE '  GE' TO GODK-STATUSKODER                                      
349200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
349300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
349400     PERFORM IMS-STATUSKONTROLL                                           
349500     IF SEGMENT-SAKNAS                                                    
349600         MOVE SPACE TO DCS-KDDC                                           
349700     END-IF                                                               
349800     .                                                                    
349900     SKIP3                                                                
350000 IMS-GU-WDQ301   SECTION.                                                 
350100     MOVE 'IMS-GU-WDQ301 '    TO WS-SEKTION                               
350200                                                                          
350300     STRING 'WDQ301  (WDQ301KY =' W-WDQ301-KEY-X ')'                      
350400            DELIMITED BY SIZE INTO SSA1                                   
350500     MOVE '  GE' TO GODK-STATUSKODER                                      
350600     CALL CBLTDLI USING GU    WDQ3-PCB DLI-IO-WDQ301 SSA1                 
350700     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
350800     PERFORM IMS-STATUSKONTROLL                                           
350900     .                                                                    
351000     SKIP3                                                                
351100     EJECT                                                                
351200 DB2-SELECT-TP4TRAN     SECTION.                                          
351300     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
351400                                                                          
351500     MOVE 000100 TO GODK-SQLCODEKODER                                     
351600                                                                          
351700     EXEC SQL                                                             
351800           SELECT  DISTINCT                                               
351900                   IDDC_REC                                               
352000                                                                          
352100           INTO   :TP4TRAN-IDDC-REC                                       
352200                                                                          
352300           FROM    TP4TRAN                                                
352400                                                                          
352500           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
352600     END-EXEC                                                             
352700                                                                          
352800     MOVE SQLCODE TO SQLCODE-WS                                           
352900     PERFORM DB2-STATUSKONTROLL                                           
353000     .                                                                    
353100     EJECT                                                                
353200 IMS-STATUSKONTROLL SECTION.                                              
353300                                                                          
353400     SET STATUS-IX TO 1                                                   
353500     SEARCH GODK-STATUS                                                   
353600       AT END                                                             
353700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
353800           DELIMITED BY SIZE INTO FELTEXT                                 
353900         CALL FELLOG                                                      
354000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
354100         CONTINUE                                                         
354200     END-SEARCH                                                           
354300     .                                                                    
354400     EJECT                                                                
354500 DB2-STATUSKONTROLL  SECTION.                                             
354600                                                                          
354700     SET SQLCODE-IX TO 1                                                  
354800     SEARCH GODK-SQLCODE                                                  
354900       AT END                                                             
355000          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
355100          DELIMITED BY SIZE INTO FELTEXT                                  
355200          CALL ABEND USING RKOD-ABEND-DB2                                 
355300       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
356000     END-SEARCH                                                           
360000     .                                                                    
