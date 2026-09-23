000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4765100.                                                
000400 AUTHOR.         MOGREN STINA.                                            
000500 DATE-WRITTEN.   02/04/12.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        INGÅR I SOP-RUTIN W476D5                                         
001000*        PROGRAMMET LÄSER W47650 , SORTERADE POSTER                       
001100*        (FAKT,SHIPM,DIST,KUND,ORDER,PROD,KOLLI,PUR,PTYP)                 
001200*                                                                         
001300*  'RIK' POSTER TILL VIPS W476RIK/RIL/RIM/RIN/RIO/RIP/RIZ                 
001400*        INVOICE-INFO VIPS-IMP  PÅ W47653                                 
001500*                                                                         
001600*  'VR ' POSTER TILL VR , W475F801-W475F805                               
001700*        INFO TIL VR-SYSTEMET                                             
001800*                                                                         
001900*  'FAK' POSTER TILL      W42108 PER SVERIGE/ÖVR.VÄRLDEN                  
002000*        INFO TIL FAKTURASTATISTIK-SYSTEM                                 
002100*        BORTTAGET 04-09-24                                               
002200*                                                                         
002300*  '330' SKAPA EN POST PER KOLLI-RAD PÅ W47654                            
002400*        SALES STATISTICS                                                 
002500*                                                                         
002600*  '463' SKAPA EN POST PER KOLLI-RAD PÅ W47658                            
002700*        INFO OM DDGS-LEVERANSER                                          
002800*                                                                         
002900*  '510' SKAPA EN POST PER KOLLI-RAD PÅ W47660                            
003000*        INFO TILL W500,  EKONOMI                                         
003100*                                                                         
003200*  'R33' SKAPA EN POST PER KOLLI PÅ W47663                                
003300*        INFO TILL      , INLEVERANSER                                    
003400**       (SKAPAS AV 510-POSTERNA)                                         
003500*                                                                         
003600*  'BYT' SKAPA EN POST PER KOLLI-RAD PÅ W4768M                            
003700*        INFO TILL BYTES-SYSTEMET                                         
003800**       (SKAPAS AV 510-POSTERNA)                                         
003900*                                                                         
004000*  'GRK' SKAPA EN POST PER FAKTURA-RAD PÅ W4765G                          
004100*        INFO TILL DISTR 1558 1578 GREKLAND                               
004200**       (SKAPAS AV 510-POSTERNA)                                         
004300*                                                                         
004400*  'W33' SKAPA EN POST PER FAKTURA-RAD PÅ W47676                          
004500*        INFO TILL DISTR USA/CANADA                                       
004600*                                                                         
004700*  'LEV' SKAPA EN POST PER FAKTURA-RAD PÅ W47677                          
004800*        LEVERANSANM. NDC USA/CANADA                                      
004900*        REREFILL, SCRAPPING                                              
005000*                                                                         
005100*  'LAB' SKAPA EN POST PER FAKTURA-RAD PÅ W4768D                          
005200*                                                                         
005300*  'DUB' SKAPA EN POST PER FAKTURA-RAD PÅ W4768Z                          
005400*        INFO TILL DISTR 6247 DUBAI  (FÖRENADE ARABEMIRATEN)              
005500*        (SKAPAS AV 510-POSTERNA)                                         
005600*                                                                         
005700*  'SAT' SKAPA EN POST PER SATS   PÅ W4765H                               
005800*        FLYTTAT TILL W47668                                              
005900*                                                                         
006000*        MED INFO FRÅN WDE4 BL.A.                                         
006100*                                                                         
006200*        PROGRAMMET LÄSER      WDE4                                       
006300*                              WDE6                                       
006400*                              WDQ2                                       
006500*                              WDB1 / WDB2                                
006600*                              WDK6                                       
006700*                              WDK7                                       
006800*                              WDR4                                       
006900*                                                                         
007000*    ABENDKODER:                                                          
007100*        U0016 -  . . . .                                                 
007200*        U1000 -  . . . .                                                 
007300*                                                                         
007400*    E-TRACKER 10169250 - CHINA WAREHOUSE EXTRA PROJEKT-1                 
007500*    E-TRACKER 10231040 - KONSOLIDERADE SW-ORDER                          
007600*                                                                         
007700     SKIP3                                                                
007800 ENVIRONMENT DIVISION.                                                    
007900     SKIP2                                                                
008000 INPUT-OUTPUT SECTION.                                                    
008100                                                                          
008200 FILE-CONTROL.                                                            
008300     SKIP2                                                                
008400*          --- POSTER FRÅN W4765000 SOM SKA KOMPLETTERAS                  
008500     SELECT W47650                     ASSIGN TO W47651D1.                
008600     SKIP2                                                                
008700*          --- KOMPLETTERAD FIL W47651 VIPS/IMPORTER                      
008800     SELECT W47653                     ASSIGN TO W47651D3.                
008900     EJECT                                                                
009000*          --- KOMPLETTERAD FIL W47651 SALES STAT                         
009100     SELECT W47654                     ASSIGN TO W47651D4.                
009200     EJECT                                                                
009300*          --- KOMPLETTERAD FIL W47651 INFO DDGS                          
009400     SELECT W47658                     ASSIGN TO W47651D5.                
009500     EJECT                                                                
009600*          --- KOMPLETTERAD FIL W47651 INFO ACCOUNTING                    
009700     SELECT W47660                     ASSIGN TO W47651D6.                
009800*          --- KOMPLETTERAD FIL W47651 INFO ACCOUNTING                    
009900     SELECT W4768E                     ASSIGN TO W47651DG.                
010000*          --- KOMPLETTERAD FIL W47651 INFO ACCOUNTING                    
010100     SELECT W4768F                     ASSIGN TO W47651DH.                
010200*          --- KOMPLETTERAD FIL W47651 INFO ACCOUNTING                    
010300     SELECT W4768G                     ASSIGN TO W47651DI.                
010400* --- FIL VR                                                              
010500     SELECT W47662                     ASSIGN TO W47651D7.                
010600* --- INLEV.STAT USA/CANADA                                               
010700     SELECT W47676                     ASSIGN TO W47651D8.                
010800* --- FIL STAT - ÖVR.VÄRLDEN                                              
010900     SELECT W47677                     ASSIGN TO W47651D9.                
011000* --- FIL DIREKTLEV - INLEV                                               
011100     SELECT W47663                     ASSIGN TO W47651DA.                
011200* --- FIL BYTES SYSTEMET                                                  
011300     SELECT W4768M                     ASSIGN TO W47651DC.                
011400* --- FIL INFO GREKLAND                                                   
011500     SELECT W4765G                     ASSIGN TO W47651DD.                
011600* --- FIL INFO LAB  USA                                                   
011700     SELECT W4768D                     ASSIGN TO W47651DE.                
011800* --- FIL INFO SAUDARABIEN                                                
011900     SELECT W4768Z                     ASSIGN TO W47651DF.                
012000     EJECT                                                                
012100 DATA DIVISION.                                                           
012200     SKIP2                                                                
012300 FILE SECTION.                                                            
012400     SKIP3                                                                
012500 FD  W47650                                                               
012600     RECORDING       F                                                    
012700     BLOCK CONTAINS  0.                                                   
012800                                                                          
012900*01  -COPY W4765001     -L.                                               
013000     SKIP3                                                                
013100 FD  W47653                                                               
013200     RECORDING       F                                                    
013300     BLOCK CONTAINS  0.                                                   
013400***                    W461RIO2                                           
013500*01  UT2-POST   -COPY W4765201     -L.                                    
013600     EJECT                                                                
013700 FD  W47654                                                               
013800     RECORDING       F                                                    
013900     BLOCK CONTAINS  0.                                                   
014000*01  UT3-POST   -COPY W330099      -L.                                    
014100     EJECT                                                                
014200 FD  W47658                                                               
014300     RECORDING       F                                                    
014400     BLOCK CONTAINS  0.                                                   
014500*01  UT4-POST   -COPY W46341       -L.                                    
014600     EJECT                                                                
014700 FD  W47660                                                               
014800     RECORDING       F                                                    
014900     BLOCK CONTAINS  0.                                                   
015000*01  UT5-POST   -COPY W51060       -L.                                    
015100     EJECT                                                                
015200 FD  W4768E                                                               
015300     RECORDING       F                                                    
015400     BLOCK CONTAINS  0.                                                   
015500*01  UT6-POST   -COPY W57060       -L.                                    
015600     EJECT                                                                
015700 FD  W4768F                                                               
015800     RECORDING       F                                                    
015900     BLOCK CONTAINS  0.                                                   
016000*01  UT7-POST   -COPY W57060       -L.                                    
016100     EJECT                                                                
016200 FD  W4768G                                                               
016300     RECORDING       F                                                    
016400     BLOCK CONTAINS  0.                                                   
016500*01  UT9-POST   -COPY W57060       -L.                                    
016600     EJECT                                                                
016700 FD  W47662                                                               
016800     LABEL RECORD   STANDARD                                              
016900     RECORDING      V                                                     
017000     BLOCK CONTAINS 0.                                                    
017100                                                                          
017200 01  VR-POST-5.                                                           
017300*    03  FILLER   -COPY W475F800    -L.                                   
017400*    03  FILLER   -COPY W475F805    -L.                                   
017500                                                                          
017600 01  VR-POST-4.                                                           
017700*    03  FILLER   -COPY W475F800    -L.                                   
017800*    03  FILLER   -COPY W475F804    -L.                                   
017900                                                                          
018000 01  VR-POST-3.                                                           
018100*    03  FILLER   -COPY W475F800    -L.                                   
018200*    03  FILLER   -COPY W475F803    -L.                                   
018300                                                                          
018400 01  VR-POST-2.                                                           
018500*    03  FILLER   -COPY W475F800    -L.                                   
018600*    03  FILLER   -COPY W475F802    -L.                                   
018700                                                                          
018800 01  VR-POST-1.                                                           
018900*    03  FILLER   -COPY W475F800    -L.                                   
019000*    03  FILLER   -COPY W475F801    -L.                                   
019100     EJECT                                                                
019200 FD  W47676                                                               
019300     RECORDING       F                                                    
019400     BLOCK CONTAINS  0.                                                   
019500*01  UT8-POST     -COPY W4758301     -L.                                  
019600     EJECT                                                                
019700 FD  W47677                                                               
019800     RECORDING       V                                                    
019900     BLOCK CONTAINS  0.                                                   
020000*01  LEVAN-POST   -COPY W418REFB     -L.                                  
020100 FD  W47663                                                               
020200     RECORDING       F                                                    
020300     BLOCK CONTAINS  0.                                                   
020400*01  UTA-POST     -COPY W4758U01     -L.                                  
020500     EJECT                                                                
020600 FD  W4768M                                                               
020700     LABEL RECORD   STANDARD                                              
020800     RECORDING      F                                                     
020900     BLOCK CONTAINS 0.                                                    
021000*01  POST -COPY W371FAK    -PRE BYTES- -L.                                
021100     EJECT                                                                
021200 FD  W4765G                                                               
021300     LABEL RECORD   STANDARD                                              
021400     RECORDING      F                                                     
021500     BLOCK CONTAINS 0.                                                    
021600                                                                          
021700*01  GRK-POST     -COPY W476GRK      -L.                                  
021800     EJECT                                                                
021900 FD  W4768D                                                               
022000     LABEL RECORD   STANDARD                                              
022100     RECORDING      V                                                     
022200     BLOCK CONTAINS 0.                                                    
022300                                                                          
022400*01  LAB-POST     -COPY W51095X      -L.                                  
022500     EJECT                                                                
022600 FD  W4768Z                                                               
022700     LABEL RECORD   STANDARD                                              
022800     RECORDING      F                                                     
022900     BLOCK CONTAINS 0.                                                    
023000                                                                          
023100*01  DUB-POST     -COPY W476DUB      -L.                                  
023200     EJECT                                                                
023300 WORKING-STORAGE SECTION.                                                 
023400                                                                          
023500 77  IDPGM                       PIC X(8)    VALUE 'W4765100'.            
023600 77  JA                          PIC X       VALUE 'J'.                   
023700 77  NEJ                         PIC X       VALUE 'N'.                   
023800                                                                          
023900 77  FL-SKRIV-VR1                PIC X       VALUE 'N'.                   
024000 77  FL-SKRIV-95X                PIC X       VALUE 'N'.                   
024100                                                                          
024200 77  W47650-EOF-SW               PIC X       VALUE 'N'.                   
024300     88  END-OF-W47650                       VALUE 'J'.                   
024400 77  W-RIM-SW                    PIC X       VALUE 'N'.                   
024500     88  RIM-POST                            VALUE 'J'.                   
024600                                                                          
024700 77  W-RIM-US                    PIC X       VALUE 'N'.                   
024800     88  RIM-US                              VALUE 'J'.                   
024900                                                                          
025000 77  FL-SKRIV-TULL               PIC X       VALUE 'N'.                   
025100 77  FL-FOERSTA-GANG             PIC X       VALUE 'J'.                   
025200 01  W-8D                        PIC S9      VALUE ZERO COMP-3.           
025300     EJECT                                                                
025400                                                                          
025500 77  IX                          PIC S9(3)  VALUE ZERO COMP-3.            
025600 77  MAX-IX                      PIC S9(3)  VALUE +7   COMP-3.            
025700 77  WS-IDMARKBO                 PIC X      VALUE SPACE.                  
025800 77  WS-KDVALISO-MC              PIC X(3)   VALUE 'SEK'.                  
025900 77  WS-KDVALISO                 PIC X(3)   VALUE SPACE.                  
026000 77  W-KDVALISO                  PIC X(3)   VALUE SPACE.                  
026100 77  WS-IDKST                    PIC X(10)  VALUE SPACE.                  
026200 77  KDRC-DISPLAY                PIC Z(5).                                
026300                                                                          
026400 77  FAST-KURS-BATH       PIC S9(6)V9(5) COMP-3 VALUE +19.00000.          
026500                                                                          
026600 77  WS-IDRADNR                  PIC S9(9) COMP SYNC VALUE +0.            
026700 77  WR-IDFAKT                   PIC S9(7) COMP-3  VALUE ZERO.            
026800 77  TEST-IDFKNGRP               PIC S9(5)   COMP-3.                      
026900     88  FKNGRP-VSA-IMP                      VALUE 1788.                  
027000     88  FKNGRP-EXT-WARRANTY                 VALUE 1728.                  
027100     88  FKNGRP-VOC-PART                     VALUE 3988.                  
027200                                                                          
027300 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
027400 01  WS-DISP                     PIC 9(12)   VALUE ZERO.                  
027500 01  W-KUNDREF                   PIC 9(10)   VALUE ZERO.                  
027600 01  FILLER                      REDEFINES W-KUNDREF.                     
027700     03  W-IDKUNDRF-RO           PIC 9(7).                                
027800     03  FILLER                  PIC X(3).                                
027900 01  W-KUNDREF2                  PIC 9(10)   VALUE ZERO.                  
028000 01  FILLER                      REDEFINES W-KUNDREF2.                    
028100     03  W-IDKUNDRF-RO2          PIC 9(5).                                
028200     03  FILLER                  PIC X(3).                                
028300                                                                          
028400 01  WS-IDKUNDRF.                                                         
028500     03  WS-IDKUNDRF-1-5         PIC 9(5).                                
028600     03  WS-IDKUNDRF-X        REDEFINES WS-IDKUNDRF-1-5.                  
028700       05  FILLER                PIC X(3).                                
028800       05  WS-IDKUNDRF-4-5       PIC 9(2).                                
028900     03  WS-IDKUNDRF-6-10        PIC X(5).                                
029000                                                                          
029100 01  WRAD-KDSOFT                 PIC S9      VALUE ZERO COMP-3.           
029200                                                                          
029300 01  WRO-IDKUNDRF               PIC X(10)  VALUE '00000     '.            
029400                                                                          
029500 01  WS-WIN                     PIC X(30)  VALUE SPACE.                   
029600 01  WS-WUT                     PIC X(30)  VALUE SPACE.                   
029700 01  WS-NUM9                    PIC 9(9).                                 
029800 01  WS-IDKUNDNR-CHECK          PIC X(7) VALUE SPACE.                     
029900                                                                          
030000 01  W-FILL7                    PIC 9(7)   VALUE ZERO.                    
030100                                                                          
030200 01  WS-WDB2-IDFTG              PIC  9(2) VALUE ZERO.                     
030300 01  WS-WDB6-IDFTG              PIC  9(2) VALUE ZERO.                     
030400                                                                          
030500 01  W-VKORDBTO-ORDER-LB        PIC S9(8)V9(1) COMP-3 VALUE ZERO.         
030600 01  W-VKORDBTO-ORDER           PIC S9(8)V9(1) COMP-3 VALUE ZERO.         
030700 01  W-PRARTNTO                 PIC S9(7)V9(2) COMP-3 VALUE ZERO.         
030800 01  SPAR-PRFRAKT-LOC            PIC 9(7)V9(2)  VALUE ZERO.               
030900                                                                          
031000 01  WSPAR1-PRFRAKT             PIC S9(7)V9(2) VALUE ZERO COMP-3.         
031100 01  WSPAR1-PREMBHNT            PIC S9(7)V9(2) VALUE ZERO COMP-3.         
031200 01  WSPAR1-PRFOERS             PIC S9(7)V9(2) VALUE ZERO COMP-3.         
031300 01 DB2-LASNING.                                                          
031400     03 FILLER                   PIC X(16)   VALUE                        
031500                                             'WS-DB2-SEKTION'.            
031600     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
031700                                                                          
031800                                                                          
031900 01 NYCKLAR-TP4TRAN.                                                      
032000     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
032100                                                                          
032200                                                                          
032300 77  W-TIAAMMDD                  PIC 9(6).                                
032400 77  W-TIKLOCK                   PIC 9(8).                                
032500                                                                          
032600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
032700 01  FILLER REDEFINES DAGENS-DATUM.                                       
032800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
032900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
033000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
033100 01  WS-DAGENS-DATUM-GRP.                                                 
033200     03 DAGENS-AA               PIC 9(2).                                 
033300     03 DAGENS-AAMMDD           PIC 9(6).                                 
033400                                                                          
033500 01  AKTUELL-TID.                                                         
033600     03  AKTUELL-TTMM-LOC    PIC 9(4).                                    
033700     03  FILLER              PIC 9(4).                                    
033800 01  W-DATUM                     PIC 9(7)    VALUE ZERO.                  
033900 01  FILLER                      REDEFINES   W-DATUM.                     
034000     03  FILLER                  PIC 9.                                   
034100     03  W-DATUM6                PIC 9(6).                                
034200                                                                          
034300 01  KONV-TIPLLEVD.                                                       
034400     03  FILLER                  PIC 9(2).                                
034500     03  KONV-TIPLLEVD1          PIC 9(3).                                
034600     EJECT                                                                
034700                                                                          
034800 01  XX-RIO2.                                                             
034900   03  XX-PRARTNTO         PIC S9(11)V9(2)  COMP-3 VALUE ZERO.            
035000   03  XX-PRARTBTO         PIC S9(11)V9(2)  COMP-3 VALUE ZERO.            
035100   03  X4-PRARTNTO         PIC S9(7)V9(2)  COMP-3 VALUE ZERO.             
035200   03  X4-PRARTBTO         PIC S9(7)V9(2)  COMP-3 VALUE ZERO.             
035300                                                                          
035400 01  GEN-IDSTATNR             PIC S9(9)  COMP-3 VALUE 87089997.           
035500 77  FIRST-REC-TRANS-SW          PIC X       VALUE 'N'.                   
035600     88  NOT-FIRST-REC-TRANS                 VALUE 'N'.                   
035700     88  FIRST-REC-TRANS                     VALUE 'J'.                   
035800 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
035900 77  W-IDARTNR-EDIT-X            PIC Z(9).                                
036000 77  WS-ADDRESS-WHSTOCKA         PIC X(50)                                
036100       VALUE 'CARPARTS.PULS.WHSTOCKADJ'.                                  
036200 77  WS-ADDRESS-MQASYNC          PIC X(50)                                
036300       VALUE 'CARPARTS.PULS.MQASYNC'.                                     
036400                                                                          
036500 01  TEST-ARTIKEL                PIC 9(9)    COMP-3.                      
036600*01  FILLER  -COPY WWART04       -RED  TEST-ARTIKEL.                      
036700*- - - - - - - - - - - -TEST BYTES-ARTIKLAR                               
036800*01  FILLER  -COPY WWBYT02       -RED  TEST-ARTIKEL.                      
036900     EJECT                                                                
037000*01  FILLER  -COPY WWBYT19       -RED  TEST-ARTIKEL.                      
037100     EJECT                                                                
037200                                                                          
037300                                                                          
037400 01  DYNAMISKA-SUBPROGRAM.                                                
037500*                                                                         
037600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
037700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
037800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
037900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
038000     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
038100     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
038200     03  W335COST                PIC X(8)    VALUE 'W335COST'.            
038300     03  W335CURR                PIC X(8)    VALUE 'W335CURR'.            
038400     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
038500     03  W460DIS1                PIC X(8)    VALUE 'W460DIS1'.            
038600     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
038700     03  W009REDU                PIC X(8)    VALUE 'W009REDU'.            
038800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
038900     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
039000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
039100     SKIP2                                                                
039200 01  FILLER              PIC X(16) VALUE 'WL01TIDZ-AREA'.                 
039300*01  -COPY WL01TIDZ                                                       
039400     EJECT                                                                
039500 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
039600 01  -COPY WZ01SEND                                                       
039700     EJECT                                                                
039800*    ----PARAMETRAR TILL W335PRIS                                         
039900 01 FILLER                       PIC X(8)    VALUE 'W335PRIS'.            
040000*   -COPY W335PRIS                                                        
040100*    ----PARAMETRAR TILL W411EXCH                                         
040200 01 FILLER                       PIC X(8)    VALUE 'W411EXCH'.            
040300*   -COPY W411EXCH                                                        
040400*    --- PARAMETRAR TILL W335COST                                         
040500 01 FILLER                       PIC X(8)    VALUE 'W335COST'.            
040600*   -COPY W335COST                                                        
040700     EJECT                                                                
040800*    --- PARAMETRAR TILL W335CURR                                         
040900 01 FILLER                       PIC X(8)    VALUE 'W335CURR'.            
041000*   -COPY W335CURR                                                        
041100     EJECT                                                                
041200*01  -COPY  W460DIS1                                                      
041300     EJECT                                                                
041400*01  -COPY  W460LISO                                                      
041500     EJECT                                                                
041600*    --- PARAMETRAR TILL W400ARTU                                         
041700                                                                          
041800*01  -COPY W400ARTU                                                       
041900                                                                          
042000 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
042100*01  -COPY WDATAREA.                                                      
042200     EJECT                                                                
042300                                                                          
042400*                                                                         
042500 01  WS-PRKURS               PIC S9(6)V9(5) VALUE ZERO COMP-3.            
042600*                                                                         
042700 01  WS-BEL                  PIC S9(11)V9(2) VALUE ZERO COMP-3.           
042800 01  WS-BELJPY               PIC S9(11)      VALUE ZERO COMP-3.           
042900*                                                                         
043000 01  WS-DALASTN                  PIC S9(7)   VALUE ZERO COMP-3.           
043100*                                                                         
043200*       PARAMETRAR TILL WWOMVAND                                          
043300*    -COPY WWOMVAND                                                       
043400*                                                                         
043500*    --- PARAMETRAR TILL W009CIA                                          
043600*01  -COPY W009CIA                                                        
043700*    --- PARAMETRAR TILL ABEND                                            
043800                                                                          
043900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
044000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
044100     SKIP2                                                                
044200 01  FELTEXT.                                                             
044300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
044400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
044500     EJECT                                                                
044600*    --NYCKLAR FÖR HOP-SORTERING                                          
044700 01  WS-NYCKLAR.                                                          
044800   03 WSSA-NYCKLAR.                                                       
044900     05  WS-IDFAKT               PIC S9(7)  VALUE ZERO  COMP-3.           
045000     05  WS-IDDISTR              PIC S9(5)  VALUE ZERO  COMP-3.           
045100   03 WSSB-NYCKLAR.                                                       
045200     05  WS-IDKUNDNR             PIC S9(7)  VALUE ZERO  COMP-3.           
045300     05  WS-IDORDNR7             PIC S9(7)  VALUE ZERO  COMP-3.           
045400     05  WS-IDPRODNR             PIC S9(7)  VALUE ZERO  COMP-3.           
045500     05  WS-IDKOLLI              PIC S9(5)  VALUE ZERO  COMP-3.           
045600                                                                          
045700 01  WS1-NYCKLAR.                                                         
045800     03  WS1-IDFAKT              PIC S9(7)  VALUE ZERO  COMP-3.           
045900     03  WS1-IDDISTR             PIC S9(5)  VALUE ZERO  COMP-3.           
046000     03  WS1-IDKUNDNR            PIC S9(7)  VALUE ZERO  COMP-3.           
046100     03  WS1-IDORDNR7            PIC S9(7)  VALUE ZERO  COMP-3.           
046200     03  WS1-IDPRODNR            PIC S9(7)  VALUE ZERO  COMP-3.           
046300     03  WS1-IDKOLLI             PIC S9(5)  VALUE ZERO  COMP-3.           
046400                                                                          
046500 01  WS2-NYCKLAR.                                                         
046600     03  WS2-IDFAKT              PIC S9(7)  VALUE ZERO  COMP-3.           
046700     03  WS2-IDDISTR             PIC S9(5)  VALUE ZERO  COMP-3.           
046800     03  WS2-IDKUNDNR            PIC S9(7)  VALUE ZERO  COMP-3.           
046900     03  WS2-IDORDNR7            PIC S9(7)  VALUE ZERO  COMP-3.           
047000     03  WS2-IDPRODNR            PIC S9(7)  VALUE ZERO  COMP-3.           
047100     03  WS2-IDKOLLI             PIC S9(5)  VALUE ZERO  COMP-3.           
047200 01  WS2-IDDC                    PIC X(2)   VALUE SPACE.                  
047300                                                                          
047400 01  WS5-NYCKLAR.                                                         
047500     03  WS5-IDFAKT              PIC S9(7)  VALUE ZERO  COMP-3.           
047600     03  WS5-IDDISTR             PIC S9(5)  VALUE ZERO  COMP-3.           
047700     03  WS5-IDKUNDNR            PIC S9(7)  VALUE ZERO  COMP-3.           
047800     03  WS5-IDORDNR7            PIC S9(7)  VALUE ZERO  COMP-3.           
047900     03  WS5-IDPRODNR            PIC S9(7)  VALUE ZERO  COMP-3.           
048000     03  WS5-IDKOLLI             PIC S9(5)  VALUE ZERO  COMP-3.           
048100                                                                          
048200 01  WS6-NYCKLAR.                                                         
048300     03  WS6-IDFAKT              PIC S9(7)  VALUE ZERO  COMP-3.           
048400     03  WS6-IMDISTR             PIC S9(5)  VALUE ZERO  COMP-3.           
048500     03  WS6-IDKUNDNR            PIC S9(7)  VALUE ZERO  COMP-3.           
048600     03  WS6-IDORDNR7            PIC S9(7)  VALUE ZERO  COMP-3.           
048700     03  WS6-IDPRODNR            PIC S9(7)  VALUE ZERO  COMP-3.           
048800     03  WS6-IDKOLLI             PIC S9(5)  VALUE ZERO  COMP-3.           
048900                                                                          
049000 01  WS7-NYCKLAR.                                                         
049100     03  WS7-IDFAKT              PIC S9(7)  VALUE ZERO  COMP-3.           
049200     03  WS7-IDDISTR             PIC S9(5)  VALUE ZERO  COMP-3.           
049300     03  WS7-IDKUNDNR            PIC S9(7)  VALUE ZERO  COMP-3.           
049400     03  WS7-IDORDNR7            PIC S9(7)  VALUE ZERO  COMP-3.           
049500     03  WS7-IDPRODNR            PIC S9(7)  VALUE ZERO  COMP-3.           
049600     03  WS7-IDKOLLI             PIC S9(5)  VALUE ZERO  COMP-3.           
049700   01    WS7-IDKUNDRF            PIC X(10)  VALUE SPACE.                  
049800                                                                          
049900 01  WS8-NYCKLAR.                                                         
050000   03 WS81-NYCKLAR.                                                       
050100     05  WS8-IDFAKT              PIC S9(7)  VALUE ZERO  COMP-3.           
050200     05  WS8-IDDISTR             PIC S9(5)  VALUE ZERO  COMP-3.           
050300   03 WS82-NYCKLAR.                                                       
050400     05  WS8-IDKUNDNR            PIC S9(7)  VALUE ZERO  COMP-3.           
050500     05  WS8-IDORDNR7            PIC S9(7)  VALUE ZERO  COMP-3.           
050600     05  WS8-IDPRODNR            PIC S9(7)  VALUE ZERO  COMP-3.           
050700     05  WS8-IDKOLLI             PIC S9(5)  VALUE ZERO  COMP-3.           
050800                                                                          
050900 01  WSB-NYCKLAR.                                                         
051000     03  WSB-IDFAKT              PIC S9(7)  VALUE ZERO  COMP-3.           
051100     03  WSB-IDDISTR             PIC S9(5)  VALUE ZERO  COMP-3.           
051200     03  WSB-IDKUNDNR            PIC S9(7)  VALUE ZERO  COMP-3.           
051300     03  WSB-IDORDNR7            PIC S9(7)  VALUE ZERO  COMP-3.           
051400     03  WSB-IDPRODNR            PIC S9(7)  VALUE ZERO  COMP-3.           
051500     03  WSB-IDKOLLI             PIC S9(5)  VALUE ZERO  COMP-3.           
051600                                                                          
051700 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
051800*01  FILLER  -COPY WWDIST07   -RED TEST-IDDISTR.                          
051900     EJECT                                                                
052000*01  FILLER  -COPY WWDIST18   -RED TEST-IDDISTR.                          
052100     EJECT                                                                
052200*01  FILLER  -COPY WWDIST19   -RED TEST-IDDISTR.                          
052300     EJECT                                                                
052400*01  FILLER  -COPY WWDIST28   -RED TEST-IDDISTR.                          
052500     EJECT                                                                
052600*01  FILLER  -COPY WWDIST34   -RED TEST-IDDISTR.                          
052700     EJECT                                                                
052800*01  FILLER  -COPY WWDIST35   -RED TEST-IDDISTR.                          
052900     EJECT                                                                
053000*01  FILLER  -COPY WWDIST74   -RED TEST-IDDISTR.                          
053100     EJECT                                                                
053200*01  FILLER  -COPY WWDIST77   -RED TEST-IDDISTR.                          
053300     EJECT                                                                
053400*01  FILLER  -COPY WWDIST79   -RED TEST-IDDISTR.                          
053500     EJECT                                                                
053600*01  FILLER  -COPY WWDIST92   -RED TEST-IDDISTR.                          
053700     EJECT                                                                
053800*01  FILLER  -COPY WWDIS130   -RED TEST-IDDISTR.                          
053900     EJECT                                                                
054000 01  FILLER                      PIC X(16)  VALUE 'REFILLTABDC'.          
054100*   -COPY WWDIST57                                                        
054200*01  FILLER  -COPY WWDC99.                                                
054300                                                                          
054400*01  FILLER  -COPY WWIDFTG.                                               
054500                                                                          
054600*01  -COPY   WWDCKONS                                                     
054700                                                                          
054800     EJECT                                                                
054900                                                                          
055000*    --- PARAMETER TILL  W930VAL                                          
055100*01  -COPY W930VAL                                                        
055200                                                                          
055300*    --- PARAMETRAR TILL POSTSUM                                          
055400*                                                                         
055500*01  -COPY W0005   -PRE  POSTSUM-                                         
055600     EJECT                                                                
055700 01  IN-AREA-START               PIC X(16)   VALUE                        
055800                                 'IN-AREA-START  '.                       
055900     SKIP2                                                                
056000                                                                          
056100*01  AREA -COPY W4765001    -PRE IN-                                      
056200     EJECT                                                                
056300*01  -COPY W4760001                                                       
056400                                                                          
056500*01  U-AREA -COPY  W4765201                                               
056600                                                                          
056700*********************************************                             
056800 01  UT-AREA-START               PIC X(16)   VALUE                        
056900                                 'UT-AREA-START  '.                       
057000 01  UT-AREA                     PIC X(195)  VALUE SPACES.                
057100 01  FILLER                      REDEFINES   UT-AREA.                     
057200     03  UT-IDPTYP               PIC X(3).                                
057300     SKIP2                                                                
057400*********************************************                             
057500 01  UT-AREA2-START              PIC X(16)   VALUE                        
057600                                 'UT-AREA2-START '.                       
057700 01  UT2-AREA                    PIC X(250)  VALUE SPACES.                
057800 01  FILLER                      REDEFINES   UT2-AREA.                    
057900     03  UT2-IDPTYP              PIC X(3).                                
058000     SKIP2                                                                
058100 01  FILLER                      PIC X(16)   VALUE 'W476RIK'.             
058200*01  -COPY W461RIK1                                                       
058300*                                                                         
058400 01  FILLER                      PIC X(16)   VALUE 'W476RIL'.             
058500*01  -COPY W461RILN                                                       
058600*                                                                         
058700 01  FILLER                      PIC X(16)   VALUE 'W476RIM'.             
058800*01  -COPY W461RIM2                                                       
058900*                                                                         
059000 01  FILLER                      PIC X(16)   VALUE 'W476RIN'.             
059100*01  -COPY W461RINN                                                       
059200*                                                                         
059300 01  FILLER                      PIC X(16)   VALUE 'W476RIO'.             
059400*01  -COPY W461RIO2                                                       
059500*                                                                         
059600 01  FILLER                      PIC X(16)   VALUE 'W476RIP'.             
059700*01  -COPY W461RIPN                                                       
059800*                                                                         
059900 01  FILLER                      PIC X(16)   VALUE 'W476RIZ'.             
060000*01  -COPY W461RIZN                                                       
060100     EJECT                                                                
060200                                                                          
060300*********************************************                             
060400 01  UT3-AREA-START               PIC X(16)   VALUE                       
060500                                 'UT3-AREA-START '.                       
060600     SKIP2                                                                
060700 01  UT3-AREA.                                                            
060800*    03  -COPY W330099      -PRE UT3-                                     
060900     EJECT                                                                
061000                                                                          
061100*********************************************                             
061200 01  UT4-AREA-START              PIC X(16)   VALUE                        
061300                                 'UT4-AREA-START '.                       
061400     SKIP2                                                                
061500 01  UT4-AREA.                                                            
061600*    03  -COPY W46341       -PRE UT4-                                     
061700     EJECT                                                                
061800*********************************************                             
061900 01  UT5-AREA-START              PIC X(16)   VALUE                        
062000                                 'UT5-AREA-START '.                       
062100 01  FILLER                      PIC X(16)   VALUE 'W510EKHA'.            
062200 01  UT5-AREA.                                                            
062300*    03  -COPY W51060     -PRE UT5-                                       
062400                                                                          
062500 01  UT5-FILLER.                                                          
062600     03  UT5-FILL                PIC X(250)  VALUE SPACES.                
062700     03  FILLER                  REDEFINES   UT5-FILL.                    
062800      05  FILLER                 PIC X(74).                               
062900      05  UT5-IDPTYP             PIC X(3).                                
063000     SKIP2                                                                
063100*********************************************                             
063200 01  UT6-AREA-START              PIC X(16)   VALUE                        
063300                                 'UT6-AREA-START '.                       
063400 01  FILLER                      PIC X(16)   VALUE 'W570EKHA'.            
063500 01  UT6-AREA.                                                            
063600*    03  -COPY W57060     -PRE UT6-                                       
063700                                                                          
063800 01  UT6-FILLER.                                                          
063900     03  UT6-FILL                PIC X(246)  VALUE SPACES.                
064000     03  FILLER                  REDEFINES   UT6-FILL.                    
064100      05  FILLER                 PIC X(74).                               
064200      05  UT6-IDPTYP             PIC X(3).                                
064300     SKIP2                                                                
064400*********************************************                             
064500 01  UT7-AREA-START              PIC X(16)   VALUE                        
064600                                 'UT7-AREA-START '.                       
064700 01  FILLER                      PIC X(16)   VALUE 'W515EKHA'.            
064800 01  UT7-AREA.                                                            
064900*    03  -COPY W57060     -PRE UT7-                                       
065000                                                                          
065100 01  UT7-FILLER.                                                          
065200     03  UT7-FILL                PIC X(246)  VALUE SPACES.                
065300     03  FILLER                  REDEFINES   UT7-FILL.                    
065400      05  FILLER                 PIC X(74).                               
065500      05  UT7-IDPTYP             PIC X(3).                                
065600     SKIP2                                                                
065700*********************************************                             
065800*        UTAREA VR                                                        
065900*********************************************                             
066000     SKIP2                                                                
066100 01  FILLER                      PIC X(16) VALUE 'UTAREA VR '.            
066200 01  VR-UTAREA.                                                           
066300*    03  -COPY W475F800          -PRE VR-                                 
066400*    03  DATA-AREA -COPY W475F805    -PRE VR- -L.                         
066500     SKIP3                                                                
066600*    03  POST -COPY W475F801     -PRE VR1- -RED VR-DATA-AREA              
066700     EJECT                                                                
066800*    03  POST -COPY W475F802     -PRE VR2- -RED VR-DATA-AREA              
066900     SKIP3                                                                
067000*    03  POST -COPY W475F803     -PRE VR3- -RED VR-DATA-AREA              
067100     EJECT                                                                
067200*    03  POST -COPY W475F804     -PRE VR4- -RED VR-DATA-AREA              
067300     EJECT                                                                
067400*    03  POST -COPY W475F805     -PRE VR5- -RED VR-DATA-AREA              
067500     EJECT                                                                
067600*********************************************                             
067700*        UTAREA VR1                                                       
067800*********************************************                             
067900     SKIP2                                                                
068000 01  FILLER                      PIC X(16) VALUE 'UTAREA VR1'.            
068100 01  VR1-UTAREA.                                                          
068200*    03  -COPY W475F800              -PRE VR1- -L.                        
068300*    03  DATA-AREA -COPY W475F805    -PRE VR1- -L.                        
068400     SKIP3                                                                
068500*    03  POST -COPY W475F801     -PRE VRSPAR1- -RED VR1-DATA-AREA         
068600     EJECT                                                                
068700*********************************************                             
068800 01  UT8-AREA-START              PIC X(16)   VALUE                        
068900                                 'UT8-AREA-START '.                       
069000     SKIP2                                                                
069100*********************************************                             
069200 01  UT9-AREA-START              PIC X(16)   VALUE                        
069300                                 'UT9-AREA-START '.                       
069400 01  FILLER                      PIC X(16)   VALUE 'W561EKHA'.            
069500 01  UT9-AREA.                                                            
069600*    03  -COPY W57060     -PRE UT9-                                       
069700                                                                          
069800 01  UT9-FILLER.                                                          
069900     03  UT9-FILL                PIC X(246)  VALUE SPACES.                
070000     03  FILLER                  REDEFINES   UT9-FILL.                    
070100      05  FILLER                 PIC X(74).                               
070200      05  UT9-IDPTYP             PIC X(3).                                
070300     SKIP2                                                                
070400 01  W33-AREA.                                                            
070500*    03  -COPY W4758301     -PRE W33-                                     
070600     EJECT                                                                
070700*********************************************                             
070800 01  LEV-AREA-START              PIC X(16)   VALUE                        
070900                                 'LEVAN-AREA   '.                         
071000     SKIP2                                                                
071100 01  LEVAN-UTAREA.                                                        
071200*    03  -COPY W418REFB     -PRE LEVAN-                                   
071300     EJECT                                                                
071400*********************************************                             
071500 01  UTA-AREA-START              PIC X(16)   VALUE                        
071600                                 'UTA-AREA-START '.                       
071700     SKIP2                                                                
071800 01  UTA-AREA.                                                            
071900*    03  -COPY W4758U01     -PRE UTA-                                     
072000     EJECT                                                                
072100*********************************************                             
072200 01  UTB-AREA-START              PIC X(16)   VALUE                        
072300                                 'UTB-AREA-START '.                       
072400     SKIP2                                                                
072500 01  UTB-AREA.                                                            
072600*    03  -COPY W475890      -PRE 681-                                     
072700*    03  -COPY W475892      -PRE 682- -RED 681-W475890                    
072800 01  UTB2-AREA.                                                           
072900*    03  -COPY W475890      -PRE JFR-                                     
073000     EJECT                                                                
073100*********************************************                             
073200 01  FILLER                      PIC X(16) VALUE 'UTAREA BYTES'.          
073300 01  BYTES-UTAREA.                                                        
073400*    03  -COPY W371FAK      -PRE BYTES-                                   
073500*********************************************                             
073600 01  FILLER                      PIC X(16) VALUE 'UTAREA GREKL'.          
073700 01  GRK-UTAREA.                                                          
073800*    03  -COPY W476GRK                                                    
073900     EJECT                                                                
074000*********************************************                             
074100 01  FILLER                      PIC X(16) VALUE 'UTAREA LAB'.            
074200 01  LAB-UTAREA.                                                          
074300*    03  -COPY W51094X      -PRE EK94X-                                   
074400*    03  -COPY W51095X      -PRE EK95X-                                   
074500     EJECT                                                                
074600*********************************************                             
074700 01  FILLER                      PIC X(16) VALUE 'UTAREA SAUDI'.          
074800 01  DUB-UTAREA.                                                          
074900*    03  -COPY W476DUB      -PRE DUB-                                     
075000     EJECT                                                                
075100*********************************************                             
075200*    NOTAFISCAL                                                           
075300 01  NOTF-AREA.                                                           
075400*    03  -COPY W611NOTF                                                   
075500     EJECT                                                                
075600 01  FILLER                      PIC X(16)   VALUE 'MSG PROP'.            
075700*01  -COPY WZ04PROP                                                       
075800     EJECT                                                                
075900                                                                          
076000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
076100*                                                                         
076200     EJECT                                                                
076300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
076400     SKIP3                                                                
076500 01  NYCKLAR-TILL-DLI.                                                    
076600     03  W-IDSHIPM-X.                                                     
076700         05  W-IDSHIPM           PIC  9(7)   VALUE ZERO.                  
076800     03  W-WDE111KY-X.                                                    
076900         05  W-IDDISTR-E1        PIC S9(5)   VALUE ZERO COMP-3.           
077000         05  W-IDKUNDNR-E1       PIC S9(7)   VALUE ZERO COMP-3.           
077100     03  W-WDE121KY-X.                                                    
077200         05  W-IDPRODNR-E1       PIC S9(7)   VALUE ZERO COMP-3.           
077300         05  W-IDKOLLI-E1        PIC S9(5)   VALUE ZERO COMP-3.           
077400     03  W-IDPURAD-X.                                                     
077500         05  W-IDPURAD-E1        PIC S9(5)   VALUE ZERO COMP-3.           
077600                                                                          
077700     03  W-WDE4BSEQ-X.                                                    
077800         05  W-IDPRODNR-E4       PIC S9(7)   VALUE ZERO COMP-3.           
077900         05  W-IDPURAD-E4        PIC S9(5)   VALUE ZERO COMP-3.           
078000     03  W-WDE421KY-X.                                                    
078100         05  W-IDPRODNR-E421     PIC S9(7)   VALUE ZERO COMP-3.           
078200         05  W-IDKOLLI-E421      PIC S9(5)   VALUE ZERO COMP-3.           
078300     03  W-WDE4ESEQ-X.                                                    
078400         05  W-IDPRODNR-ESEQ     PIC S9(7)   VALUE ZERO COMP-3.           
078500                                                                          
078600     03  W-IDPRODNR-E6-X.                                                 
078700         05  W-IDPRODNR-E6       PIC S9(7)   VALUE ZERO COMP-3.           
078800     03  W-IDKOLLI-E6-X.                                                  
078900         05  W-IDKOLLI-E6        PIC S9(5)   VALUE ZERO COMP-3.           
079000     03  W-IDGMT-X.                                                       
079100         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO COMP-3.           
079200         05  W-IDKUNDNR-B2       PIC S9(7)   VALUE ZERO COMP-3.           
079300                                                                          
079400     03  W-IDGMT-MIN-X.                                                   
079500         05  W-IDDISTR-MIN-B2    PIC S9(5)   VALUE ZERO COMP-3.           
079600         05  W-IDKUNDNR-MIN-B2   PIC S9(7)   VALUE ZERO COMP-3.           
079700                                                                          
079800     03  W-IDGMT-MAX-X.                                                   
079900         05  W-IDDISTR-MAX-B2    PIC S9(5)   VALUE ZERO COMP-3.           
080000         05  W-IDKUNDNR-MAX-B2   PIC S9(7)   VALUE ZERO COMP-3.           
080100                                                                          
080200     03  W-IDARTNR-X.                                                     
080300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
080400                                                                          
080500     03  W-IDARTNR-CORE-X.                                                
080600         05  W-IDARTNR-CORE      PIC S9(9)   VALUE ZERO COMP-3.           
080700                                                                          
080800     03  W-IDDC-X.                                                        
080900         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
081000                                                                          
081100     03  W-WDB101KY-X.                                                    
081200         05  W-IDPARTNR          PIC X(9)    VALUE SPACE.                 
081300         05  W-IDFTG             PIC 9(2)    VALUE ZERO.                  
081400                                                                          
081500     03  W-IDORDER-X.                                                     
081600         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
081700* TILL R4 - TRANSPORTINFO BOLLA                                           
081800     03  W-WDGXKEY-4491-X.                                                
081900         05  W-IDHTYP-4491       PIC X(4)    VALUE '4491'.                
082000         05  W-IDDC-4491         PIC X(2)    VALUE SPACE.                 
082100         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
082200                                                                          
082300     03  W-DALASTN-X.                                                     
082400         05  W-DALASTN           PIC  9(8)   VALUE ZERO.                  
082500     03  W-IDKOLLI-4-X.                                                   
082600         05  W-IDKOLLI-4         PIC S9(5)   VALUE ZERO COMP-3.           
082700     03  W-IDGMTREF-X.                                                    
082800         05  W-IDDISTR-4         PIC S9(5)   VALUE ZERO COMP-3.           
082900         05  W-IDKUNDNR-4        PIC S9(7)   VALUE ZERO COMP-3.           
083000         05  W-IDKUNDRF.                                                  
083100             07  W-IDORDNR7-4    PIC 9(7)    VALUE ZERO.                  
083200             07  FILLER          PIC X(3)    VALUE SPACE.                 
083300     03  W-IDLBBET-X.                                                     
083400         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
083500     03  W-IDDC-B6-X.                                                     
083600         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
083700                                                                          
083800*                                                                         
083900     SKIP2                                                                
084000*    --- STATUS-KOD FRÅN IMS                                              
084100 01  STATUS-WS                   PIC XX.                                  
084200     88  SEGMENT-FINNS                       VALUE '  '.                  
084300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
084400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
084500     SKIP2                                                                
084600 01  GODK-STATUSKODER.                                                    
084700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
084800     SKIP3                                                                
084900 01  SSA1                        PIC X(96).                               
085000 01  SSA2                        PIC X(64).                               
085100 01  SSA3                        PIC X(64).                               
085200 01  SSA4                        PIC X(64).                               
085300     EJECT                                                                
085400*                            DB2 FUNKTIONSKODER                           
085500 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
085600       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
085700                                                                          
085800 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
085900 01  DB2-WS.                                                              
086000     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
086100         88  CURSOR-OK                       VALUE 000.                   
086200         88  RADER-FINNS                     VALUE 000.                   
086300         88  RADER-SAKNAS                    VALUE 100.                   
086400         88  ATKOMST-FEL                     VALUE 904.                   
086500     03  GODK-SQLCODEKODER.                                               
086600         05  GODK-SQLCODE OCCURS 5                                        
086700             INDEXED BY SQLCODE-IX PIC 9(3).                              
086800 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
086900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
087000     EJECT                                                                
087100*    --- IMS FUNKTIONSKODER                                               
087200*01  -COPY W0003                                                          
087300     EJECT                                                                
087400*    ---  DLI INPUT-OUTPUT AREA                                           
087500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE401'.         
087600 01  DLI-IO-WDE401.                                                       
087700*    03  -COPY WDE401                                                     
087800     EJECT                                                                
087900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE411'.         
088000 01  DLI-IO-WDE411.                                                       
088100*    03  -COPY WDE411                                                     
088200     EJECT                                                                
088300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE421'.         
088400 01  DLI-IO-WDE421.                                                       
088500*    03  -COPY WDE421                                                     
088600     EJECT                                                                
088700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE601'.         
088800 01  DLI-IO-WDE601.                                                       
088900*    03  -COPY WDE601                                                     
089000     EJECT                                                                
089100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE611'.         
089200 01  DLI-IO-WDE611.                                                       
089300*    03  -COPY WDE611                                                     
089400     EJECT                                                                
089500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE111'.         
089600 01  DLI-IO-WDE111.                                                       
089700*    03  -COPY WDE111                                                     
089800     EJECT                                                                
089900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE131'.         
090000 01  DLI-IO-WDE131.                                                       
090100*    03  -COPY WDE131                                                     
090200     EJECT                                                                
090300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE141'.         
090400 01  DLI-IO-WDE141.                                                       
090500*    03  -COPY WDE141                                                     
090600     EJECT                                                                
090700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE122'.         
090800 01  DLI-IO-WDE122.                                                       
090900*    03  -COPY WDE122                                                     
091000     EJECT                                                                
091100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDQ201'.         
091200 01  DLI-IO-WDQ201.                                                       
091300*    03  -COPY WDQ201                                                     
091400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK601'.         
091500 01  DLI-IO-WDK601.                                                       
091600*    03  -COPY WDK601                                                     
091700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK611'.         
091800 01  DLI-IO-WDK611.                                                       
091900*    03  -COPY WDK611                                                     
092000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
092100 01  DLI-IO-WDK711.                                                       
092200*    03  -COPY WDK711                                                     
092300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
092400 01  DLI-IO-WDK711-CORE.                                                  
092500*    03  -COPY WDK711    -PRE CORE-                                       
092600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB101'.         
092700 01  DLI-IO-WDB101.                                                       
092800*    03  -COPY WDB101                                                     
092900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB201'.         
093000 01  DLI-IO-WDB201.                                                       
093100*    03  -COPY WDB201                                                     
093200 01  FILLER                      PIC X(16)   VALUE 'WDGX-4494  '.         
093300 01  DLI-IO-4494.                                                         
093400     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
093500     03  WL449112 REDEFINES IO-AREA.                                      
093600*        05  -COPY WDGX4494                                               
093700     EJECT                                                                
093800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB601'.         
093900                                                                          
094000 01  DLI-IO-AREA-B601.                                                    
094100*    03  -COPY WDB601                                                     
094200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB601'.         
094300                                                                          
094400*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
094500     EJECT                                                                
094600     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
094700     EJECT                                                                
094800 LINKAGE SECTION.                                                         
094900                                                                          
095000 01  IO-PCB                      PIC X.                                   
095100 01  MQASYNC-PCB                 PIC X.                                   
095200*01  -COPY W0008  -PRE WDE4-                                              
095300     05  FILLER                  PIC X.                                   
095400*01  -COPY W0008  -PRE WDE4X-                                             
095500     05  FILLER                  PIC X.                                   
095600*01  -COPY W0008  -PRE WDE6-                                              
095700     05  FILLER                  PIC X.                                   
095800*01  -COPY W0008  -PRE WDK6-                                              
095900     05  FILLER                  PIC X.                                   
096000*01  -COPY W0008  -PRE WDK7-                                              
096100     05  FILLER                  PIC X.                                   
096200*01  -COPY W0008  -PRE WDQ2-                                              
096300     05  FILLER                  PIC X.                                   
096400*01  -COPY W0008  -PRE WDB1-                                              
096500     05  FILLER                  PIC X.                                   
096600*01  -COPY W0008  -PRE WDB2-                                              
096700     05  FILLER                  PIC X.                                   
096800*01  -COPY W0008  -PRE 4494-                                              
096900     05  FILLER                  PIC X.                                   
097000*01  -COPY W0008  -PRE WDE1-                                              
097100     05  FILLER                  PIC X.                                   
097200*01  -COPY W0008  -PRE WDB6-                                              
097300     05  FILLER                  PIC X.                                   
097400 01  PRIS-WDK6-PCB               PIC X.                                   
097500 01  PRIS-WDK7-PCB               PIC X.                                   
097600 01  PRIS-WDB2-PCB               PIC X.                                   
097700 01  PRIS-WDB1-PCB               PIC X.                                   
097800 01  PRIS-WDC1-PCB               PIC X.                                   
097900 01  PRIS-WDC2-PCB               PIC X.                                   
098000 01  COST-WDK6-PCB               PIC X.                                   
098100 01  COST-WDK7-PCB               PIC X.                                   
098200 01  COST-WDF1-PCB               PIC X.                                   
098300 01  COST-9305-PCB               PIC X.                                   
098400 01  COST-WDK72-PCB              PIC X.                                   
098500 01  COST-WDB6-PCB               PIC X.                                   
098600 01  PRIS-COST-WDK6-PCB          PIC X.                                   
098700 01  PRIS-COST-WDK7-PCB          PIC X.                                   
098800 01  PRIS-COST-WDF1-PCB          PIC X.                                   
098900 01  PRIS-COST-9305-PCB          PIC X.                                   
099000 01  PRIS-COST-WDK72-PCB         PIC X.                                   
099100 01  PRIS-COST-WDB6-PCB          PIC X.                                   
099200     EJECT                                                                
099300 PROCEDURE DIVISION  USING   IO-PCB MQASYNC-PCB                           
099400                             WDE4-PCB                                     
099500                             WDE4X-PCB                                    
099600                             WDE6-PCB                                     
099700                             WDK6-PCB                                     
099800                             WDK7-PCB                                     
099900                             WDQ2-PCB                                     
100000                             WDB1-PCB                                     
100100                             WDB2-PCB                                     
100200                             4494-PCB                                     
100300                             WDE1-PCB                                     
100400                             WDB6-PCB                                     
100500                             PRIS-WDK6-PCB  PRIS-WDK7-PCB                 
100600                             PRIS-WDB2-PCB                                
100700                             PRIS-WDB1-PCB                                
100800                             PRIS-WDC1-PCB  PRIS-WDC2-PCB                 
100900                             COST-WDK6-PCB                                
101000                             COST-WDK7-PCB                                
101100                             COST-WDF1-PCB                                
101200                             COST-9305-PCB                                
101300                             COST-WDK72-PCB COST-WDB6-PCB                 
101400                             PRIS-COST-WDK6-PCB                           
101500                             PRIS-COST-WDK7-PCB                           
101600                             PRIS-COST-WDF1-PCB                           
101700                             PRIS-COST-9305-PCB                           
101800                             PRIS-COST-WDK72-PCB                          
101900                             PRIS-COST-WDB6-PCB.                          
102000 MAIN SECTION.                                                            
102100     ENTRY 'DLITCBL' USING   IO-PCB MQASYNC-PCB                           
102200                             WDE4-PCB                                     
102300                             WDE4X-PCB                                    
102400                             WDE6-PCB                                     
102500                             WDK6-PCB                                     
102600                             WDK7-PCB                                     
102700                             WDQ2-PCB                                     
102800                             WDB1-PCB                                     
102900                             WDB2-PCB                                     
103000                             4494-PCB                                     
103100                             WDE1-PCB                                     
103200                             WDB6-PCB                                     
103300                             PRIS-WDK6-PCB  PRIS-WDK7-PCB                 
103400                             PRIS-WDB2-PCB                                
103500                             PRIS-WDB1-PCB                                
103600                             PRIS-WDC1-PCB  PRIS-WDC2-PCB                 
103700                             COST-WDK6-PCB                                
103800                             COST-WDK7-PCB                                
103900                             COST-WDF1-PCB                                
104000                             COST-9305-PCB                                
104100                             COST-WDK72-PCB COST-WDB6-PCB                 
104200                             PRIS-COST-WDK6-PCB                           
104300                             PRIS-COST-WDK7-PCB                           
104400                             PRIS-COST-WDF1-PCB                           
104500                             PRIS-COST-9305-PCB                           
104600                             PRIS-COST-WDK72-PCB                          
104700                             PRIS-COST-WDB6-PCB.                          
104800                                                                          
104900                                                                          
105000     PERFORM A-INIT                                                       
105100                                                                          
105200     PERFORM S01-LAES-W47650                                              
105300     PERFORM UNTIL END-OF-W47650                                          
105400                                                                          
105500       PERFORM C-LAES-WDE4                                                
105600                                                                          
105700       EVALUATE IN-IDPTYP                                                 
105800        WHEN 'RIK'                                                        
105900          IF WS2-NYCKLAR NOT = WS-NYCKLAR                                 
106000            PERFORM F-SKAPA-HUVUD-POSTER2                                 
106100            MOVE WS-NYCKLAR      TO WS2-NYCKLAR                           
106200            MOVE IN-IDDC         TO WS2-IDDC                              
106300          END-IF                                                          
106400        WHEN '510'                                                        
106500          IF WS5-NYCKLAR NOT = WS-NYCKLAR                                 
106600                                                                          
106700            PERFORM J-SKAPA-HUVUD-POSTER5                                 
106800            PERFORM N-SKAPA-R33-DIRLEV-EXP                                
106900            MOVE WS-NYCKLAR      TO WS5-NYCKLAR                           
107000          END-IF                                                          
107100        WHEN 'VR '                                                        
107200          IF WS6-NYCKLAR NOT = WS-NYCKLAR                                 
107300            PERFORM L-SKAPA-HUVUD-POSTER6                                 
107400            MOVE WS-NYCKLAR      TO WS6-NYCKLAR                           
107500          END-IF                                                          
107600        WHEN 'W33'                                                        
107700          IF IN-IDKOLLI = 0 AND IN-IDPURAD = 0                            
107800            CONTINUE                                                      
107900          ELSE                                                            
108000            IF WS7-NYCKLAR NOT = WS-NYCKLAR                               
108100              PERFORM M-SKAPA-HUVUD-POST7                                 
108200              MOVE WS-NYCKLAR      TO WS7-NYCKLAR                         
108300            END-IF                                                        
108400          END-IF                                                          
108500        WHEN 'LAB'                                                        
108600          IF WS81-NYCKLAR NOT = WSSA-NYCKLAR                              
108700            PERFORM S-SKAPA-HUVUD-POSTER8                                 
108800            MOVE WS-NYCKLAR      TO WS8-NYCKLAR                           
108900          END-IF                                                          
109000       END-EVALUATE                                                       
109100                                                                          
109200       EVALUATE IN-IDPTYP                                                 
109300        WHEN 'RIK'                                                        
109400          PERFORM G-SKAPA-RAD-POST2                                       
109500        WHEN '330'                                                        
109600          PERFORM H-SKAPA-STATISTIK-POST3                                 
109700        WHEN '463'                                                        
109800          PERFORM I-SKAPA-INFO-POST4                                      
109900        WHEN '510'                                                        
110000          PERFORM K-SKAPA-RAD-POST5                                       
110100          PERFORM P-SKAPA-RAD-POSTBYTES                                   
110200          PERFORM Q-SKAPA-RAD-GREKLAND                                    
110300          PERFORM T-SKAPA-RAD-SAUDARABIEN                                 
110400        WHEN 'VR '                                                        
110500          PERFORM L-SKAPA-RAD-POST6                                       
110600        WHEN 'W33'                                                        
110700          PERFORM M-SKAPA-RAD-POST7                                       
110800        WHEN 'LEV'                                                        
110900          PERFORM R-SKAPA-RAD-LEVANM                                      
111000        WHEN 'LAB'                                                        
111100          PERFORM S-SKAPA-RAD-LAB                                         
111200       END-EVALUATE                                                       
111300*                                                                         
111400       PERFORM S10-SPARA-ID                                               
111500                                                                          
111600                                                                          
111700       PERFORM S01-LAES-W47650                                            
111800     END-PERFORM                                                          
111900     IF DCS-KDTRADP = 'BR12' AND FIRST-REC-TRANS                          
112000       PERFORM S33-SEND-CLOSE                                             
112100     END-IF                                                               
112200                                                                          
112300                                                                          
112400     PERFORM Z-FINIT                                                      
112500                                                                          
112600     MOVE ZERO TO RETURN-CODE                                             
112700     GOBACK                                                               
112800     .                                                                    
112900     EJECT                                                                
113000 A-INIT SECTION.                                                          
113100     OPEN INPUT  W47650                                                   
113200                                                                          
113300     OPEN OUTPUT W47653                                                   
113400                 W47654                                                   
113500                 W47658                                                   
113600                 W47660                                                   
113700                 W47663                                                   
113800                 W47662                                                   
113900                 W47676                                                   
114000                 W47677                                                   
114100                 W4768M                                                   
114200                 W4765G                                                   
114300                 W4768D                                                   
114400                 W4768Z                                                   
114500                 W4768E                                                   
114600                 W4768F                                                   
114700                 W4768G                                                   
114800                                                                          
114900     ACCEPT DAGENS-DATUM  FROM DATE                                       
115000                                                                          
115100     ACCEPT W-TIAAMMDD    FROM DATE                                       
115200     ACCEPT  W-TIKLOCK    FROM TIME                                       
115300     MOVE W-TIKLOCK    TO AKTUELL-TID                                     
115400                                                                          
115500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
115600                                                                          
115700     MOVE ZERO                  TO OHUV-IDORDER                           
115800                                   VORD-IDPRODNR                          
115900                                   VORD-TILASTN-SK                        
116000                                   KOLLI-IDKOLLI                          
116100                                   ORAD-IDPURAD                           
116200                                   ORAD-IDARTNR                           
116300                                   KORD-IDPRODNR                          
116400                                   KORD-IDORDER                           
116500                                   GMT-IDDISTR                            
116600                                   GMT-IDKUNDNR                           
116700                                   SGMT-IDDISTR                           
116800                                   SGMT-IDKUNDNR                          
116900                                   RIM-IDORDNR                            
117000                                   RIM-IDTRPBON                           
117100     MOVE SPACES                TO BET-IDPARTNR                           
117200                                   BET-KDVALISO                           
117300                                   GMT-IDPARTNR                           
117400                                   ORAD-KDVALISO                          
117500                                   WS-KDVALISO                            
117600                                   KOLLI-IDLBBET                          
117700                                   RIM-IDTRPBOT                           
117800     MOVE ZERO                  TO BET-IDFTG                              
117900                                   GMT-IDFTG                              
118000                                                                          
118100     MOVE LOW-VALUE              TO W-IDGMT-MIN-X                         
118200     MOVE HIGH-VALUE             TO W-IDGMT-MAX-X                         
118300                                                                          
118400     PERFORM S20-NOLLA-RIL                                                
118500     .                                                                    
118600     EJECT                                                                
118700 C-LAES-WDE4 SECTION.                                                     
118800     PERFORM S10-SPARA-ID                                                 
118900                                                                          
119000     MOVE IN-IDDISTR            TO TEST-IDDISTR                           
119100                                                                          
119200     MOVE IN-IDPRODNR           TO W-IDPRODNR-E4                          
119300                                   W-IDPRODNR-E421                        
119400                                   W-IDPRODNR-ESEQ                        
119500     MOVE IN-IDPURAD            TO W-IDPURAD-E4                           
119600     MOVE IN-IDKOLLI            TO W-IDKOLLI-E421                         
119700     IF IN-IDPURAD  = ZERO   AND IN-IDKOLLI = ZERO                        
119800        PERFORM IMS-GU-WDE401-ESEQ                                        
119900        PERFORM CA-HAMTA-WDE401                                           
120000        PERFORM IMS-GNP-WDE411-ESEQ                                       
120100        PERFORM XX-FIXE411                                                
120200        MOVE ZERO         TO KOLLI-VKORDBTO-KOLLI                         
120300     ELSE                                                                 
120400       PERFORM IMS-GU-WDE411-BSEQ                                         
120500       PERFORM XX-FIXE411                                                 
120600       IF SEGMENT-FINNS                                                   
120700         PERFORM IMS-GNP-WDE401                                           
120800         IF SEGMENT-FINNS                                                 
120900           PERFORM IMS-GNP-WDE421                                         
121000         END-IF                                                           
121100       END-IF                                                             
121200     END-IF                                                               
121300     .                                                                    
121400     EJECT                                                                
121500 CA-HAMTA-WDE401  SECTION.                                                
121600                                                                          
121700     IF NOT DIST79-DEALER-PRICE AND                                       
121800        NOT DIST79-ECOM-PRICE                                             
121900      IF KORD-SUORDV = ZERO AND KORD-SUORDV-LEVPL = ZERO                  
122000        PERFORM IMS-GN-WDE401-ESEQ                                        
122100        IF SEGMENT-SAKNAS                                                 
122200          PERFORM IMS-GU-WDE401F-ESEQ                                     
122300        END-IF                                                            
122400      END-IF                                                              
122500     END-IF                                                               
122600     .                                                                    
122700     EJECT                                                                
122800 F-SKAPA-HUVUD-POSTER2   SECTION.                                         
122900     MOVE 'F-SKAPA-HUVUD-POSTER2' TO WS-SEKTION                           
123000                                                                          
123100     IF IN-IDFAKT NOT = WS2-IDFAKT                                        
123200        AND WS2-IDFAKT NOT = ZERO                                         
123300        PERFORM S22-SKRIV-RIL                                             
123400        PERFORM S20-NOLLA-RIL                                             
123500     END-IF                                                               
123600                                                                          
123700     IF IN-IDFAKT   = WS2-IDFAKT    AND                                   
123800        IN-IDDISTR  = WS2-IDDISTR   AND                                   
123900        IN-IDKUNDNR = WS2-IDKUNDNR  AND                                   
124000        IN-IDORDNR7 = WS2-IDORDNR7                                        
124100        IF IN-IDPRODNR = WS2-IDPRODNR  AND                                
124200           IN-IDKOLLI  = WS2-IDKOLLI                                      
124300           CONTINUE                                                       
124400*       ELSE                                                              
124500*          ADD KOLLI-VKORDBTO-KOLLI   TO W-VKORDBTO-ORDER                 
124600        END-IF                                                            
124700     ELSE                                                                 
124800       IF RIM-IDORDNR NOT = ZERO                                          
124900        PERFORM FA-SKRIV-RIM-POST                                         
125000        MOVE ZERO                TO W-VKORDBTO-ORDER                      
125100                                    SPAR-PRFRAKT-LOC                      
125200        MOVE NEJ                 TO W-RIM-SW                              
125300                                    W-RIM-US                              
125400       END-IF                                                             
125500     END-IF                                                               
125600                                                                          
125700     IF IN-IDPURAD = ZERO AND IN-IDKOLLI = ZERO                           
125800       CONTINUE                                                           
125900     ELSE                                                                 
126000      IF KORD-IDORDER NOT = OHUV-IDORDER                                  
126100        MOVE KORD-IDORDER           TO W-IDORDER                          
126200        PERFORM IMS-GU-WDQ201                                             
126300      END-IF                                                              
126400                                                                          
126500      IF IN-IDPRODNR NOT = VORD-IDPRODNR   OR                             
126600         IN-IDKOLLI NOT = KOLLI-IDKOLLI                                   
126700        MOVE IN-IDPRODNR            TO W-IDPRODNR-E6                      
126800        PERFORM IMS-GU-WDE601                                             
126900                                                                          
127000        MOVE IN-IDKOLLI             TO W-IDKOLLI-E6                       
127100                                       W-IDKOLLI-E1                       
127200        PERFORM IMS-GNP-WDE611                                            
127300        ADD KOLLI-VKORDBTO-KOLLI   TO W-VKORDBTO-ORDER                    
127400      END-IF                                                              
127500                                                                          
127600      IF IN-IDDC NOT = DCS-IDDC                                           
127700        MOVE IN-IDDC              TO W-IDDC-B6                            
127800        PERFORM IMS-GU-WDB601                                             
127900      END-IF                                                              
128000      MOVE IN-IDDISTR             TO W-IDDISTR-E1                         
128100                                     TEST-IDDISTR                         
128200      MOVE IN-IDKUNDNR            TO W-IDKUNDNR-E1                        
128300                                                                          
128400      MOVE 'RIK'                  TO RIK-IDPTYP                           
128500      MOVE KORD-IDDC              TO RIK-IDDC                             
128600      MOVE IN-IDDISTR             TO RIK-IDDISTR                          
128700      MOVE KORD-KDFAKTYP          TO RIK-KDFAKTYP                         
128800      MOVE IN-IDFAKT              TO RIK-IDFAKT                           
128900      MOVE IN-DAFINDOC            TO RIK-TIFAKT                           
129000      MOVE SPACE                  TO RIK-IDFRASED                         
129100      IF DIST92-ITALY-SEP                                                 
129200        MOVE IN-IDSHIPM           TO RIK-IDFRASED(1:7)                    
129300      END-IF                                                              
129400      MOVE IN-SUNTO-TOT           TO RIK-SUFKTBEL                         
129500      PERFORM S03-HAMTA-KUND                                              
129600      MOVE BET-IDMARKBO           TO WS-IDMARKBO                          
129700      PERFORM S09-BYT-KDVALISO-TILL-KDVALUTA                              
129800*     IF WS-PRKURS = ZERO OR W-KDVALISO NOT = IN-KDVALISO-FAKT            
129900        MOVE IN-KDVALISO-FAKT     TO W-KDVALISO                           
130000        MOVE IN-PRKURS-FAKT       TO WS-PRKURS                            
130100*       PERFORM S02-HAMTA-VALUTAKURS                                      
130200*     END-IF                                                              
130300      MOVE IN-PRKURS-BET          TO RIK-PRKURS                           
130400                                     WS-PRKURS                            
130500                                     EXCH-PRKURS                          
130600      MOVE IN-KDVALISO-BET        TO W-KDVALISO                           
130700                                     WS-KDVALISO                          
130800      MOVE IN-SUNTO-TOT           TO RIK-SUFKTUTL                         
130900      MOVE IN-IDDISTR             TO TEST-IDDISTR                         
131000      IF DIST79-DEALER-PRICE                                              
131100        MOVE IN-SUNTO-TOT         TO RIK-SUFKTUTL                         
131200        MOVE 1.0                  TO RIK-PRKURS                           
131300      ELSE                                                                
131400*       IF DIST79-LOCAL-CURRENCY                                          
131500*         IF  IN-KDVALISO-BET = IN-KDVALISO-FAKT                          
131600*           MOVE IN-SUNTO-TOT         TO RIK-SUFKTUTL                     
131700*           MOVE 1.0                  TO RIK-PRKURS                       
131800*         ELSE                                                            
131900*           MOVE IN-PRKURS-FIKTIV     TO RIK-PRKURS                       
132000*                                        EXCH-PRKURS                      
132100*           MOVE IN-SUNTO-TOT         TO WS-BEL                           
132200*           PERFORM S04-OMVANDLA-SEK-TILL-UTL                             
132300*           MOVE WS-BEL               TO RIK-SUFKTUTL                     
132400*         END-IF                                                          
132500*       ELSE                                                              
132600          IF IN-PRKURS-BET NOT = 1.0                                      
132700            MOVE IN-SUNTO-TOT         TO WS-BEL                           
132800            PERFORM S04-OMVANDLA-SEK-TILL-UTL                             
132900            MOVE WS-BEL               TO RIK-SUFKTUTL                     
133000* FIX JAPAN                                                               
133100            IF IN-KDVALISO-BET = 'JPY'                                    
133200              COMPUTE WS-BELJPY ROUNDED = WS-BEL * 1                      
133300              MOVE WS-BELJPY          TO RIK-SUFKTUTL                     
133400            END-IF                                                        
133500          END-IF                                                          
133600*       END-IF                                                            
133700      END-IF                                                              
133800      IF KORD-FLOVRLEV = JA                                               
133900        MOVE '01'                 TO RIK-KDFAKNOT                         
134000      ELSE                                                                
134100        MOVE SPACE                TO RIK-KDFAKNOT                         
134200      END-IF                                                              
134300      IF DIST79-DEALER-PRICE                                              
134400        MOVE IN-KDVALISO-FAKT     TO RIK-KDVALISO                         
134500      ELSE                                                                
134600        MOVE SPACE                TO RIK-KDVALISO                         
134700      END-IF                                                              
134800      MOVE RIK-W461RIK1           TO UT2-AREA                             
134900      MOVE SPACES                 TO RJX-FILLER                           
135000*                                                                         
135100      MOVE 'RIK'                TO RJX-IDPTYP                             
135200      MOVE RIK-W461RIK1         TO RJX-FILLER                             
135300      MOVE IN-IDFAKT            TO RJX-IDFAKT                             
135400      MOVE ZERO                 TO RJX-IDDISTR                            
135500      MOVE ZERO                 TO RJX-IDKUNDNR                           
135600      MOVE ZERO                 TO RJX-IDORDER                            
135700      MOVE ZERO                 TO RJX-IDKUNDNR-S                         
135800      MOVE ZERO                 TO RJX-IDPRODNR                           
135900      MOVE ZERO                 TO RJX-IDKOLLI                            
136000      MOVE ZERO                 TO RJX-IDPURAD                            
136100      MOVE ZERO                 TO RJX-IDTRPBON                           
136200*                                                                         
136300      PERFORM S12-SKRIV-W47653                                            
136400      PERFORM S10-SPARA-ID                                                
136500                                                                          
136600      IF IN-IDDC NOT = DCS-IDDC                                           
136700        MOVE IN-IDDC              TO W-IDDC-B6                            
136800        PERFORM IMS-GU-WDB601                                             
136900      END-IF                                                              
137000      MOVE IN-SUVAT-FAKT          TO RIL-PRMOMS                           
137100*     IF (KORD-KDFAKTYP = 'R' OR 'K') AND                                 
137200*       (SDC-GB OR LDC-GB-2A OR SDC-IT OR SDC-ES OR SDC-AT OR             
137300*        SDC-NL )                                                         
137400*       IF WS-PRKURS NOT = 1.0                                            
137500*         MOVE IN-SUVAT-FAKT      TO WS-BEL                               
137600*         PERFORM S04-OMVANDLA-SEK-TILL-UTL                               
137700*         MOVE WS-BEL             TO RIL-PRMOMS                           
137800*       END-IF                                                            
137900*     END-IF                                                              
138000      IF DCS-SDC AND DCS-HOLLAND AND                                      
138100         DIST34-HOLLAND-SDC                                               
138200         MOVE ZERO                TO RIL-PRMOMS                           
138300      END-IF                                                              
138400      IF DCS-DDC AND DCS-GERMANY AND                                      
138500         DIST34-TYSKLAND-DDC                                              
138600         MOVE ZERO                TO RIL-PRMOMS                           
138700      END-IF                                                              
138800**    IF DDC-NO AND DIST34-NORGE-DDC                                      
138900**       MOVE ZERO                TO RIL-PRMOMS                           
139000**    END-IF                                                              
139100**                                                                        
139200      IF DCS-NDC-PF                                                       
139300        IF DCS-INDIA                                                      
139400          CONTINUE                                                        
139500        ELSE                                                              
139600         COMPUTE WS-BEL = IN-SUVAT-FAKT / IN-PRKURS-BET                   
139700         IF IN-KDVALISO-BET = 'JPY'                                       
139800           COMPUTE WS-BELJPY ROUNDED = WS-BEL * 1                         
139900           MOVE WS-BELJPY          TO RIL-PRMOMS                          
140000         END-IF                                                           
140100        END-IF                                                            
140200      END-IF                                                              
140300                                                                          
140400      MOVE 'RIM'                  TO RIM-IDPTYP                           
140500      MOVE IN-IDKUNDNR            TO RIM-IDKUNDNR                         
140600      MOVE ZERO                   TO RIM-IDORDNR                          
140700      MOVE IN-IDORDNR7            TO RIM-IDORDNR                          
140800      MOVE IN-IDPRODNR            TO RIM-IDPRODNR                         
140900      MOVE KORD-TIORDREG          TO RIM-TIORDREG                         
141000      MOVE KORD-KDORDKL           TO RIM-KDORDKL                          
141100      MOVE OHUV-BEKUNDRF          TO RIM-BEKUNDRF                         
141200      MOVE OHUV-BEVARREF          TO RIM-BEVARREF                         
141300      IF KORD-FLOVRLEV = JA                                               
141400        MOVE '01'                 TO RIM-KDREFNOT                         
141500      ELSE                                                                
141600        MOVE '00'                 TO RIM-KDREFNOT                         
141700      END-IF                                                              
141800      MOVE OHUV-IDBILREG          TO RIM-IDBILREG                         
141900      MOVE OHUV-IDVIN             TO RIM-IDVIN                            
142000      MOVE OHUV-IDCISNR           TO RIM-IDCISNR                          
142100      MOVE OHUV-IDDEPT            TO RIM-IDDEPT                           
142200      MOVE KORD-KDFRAKT           TO RIM-KDFRAKT                          
142300                                     RIL-KDFRAKT                          
142400      PERFORM S06-HAMTA-BOLLA-INFO                                        
142500      IF RIM-IDTRPBON = ZERO                                              
142600        MOVE 4494-IDTRPBOT        TO RIM-IDTRPBOT                         
142700        MOVE 4494-IDTRPBON        TO RIM-IDTRPBON                         
142800      END-IF                                                              
142900      MOVE ZERO                   TO RIM-PRFRAKT-LOC                      
143000      MOVE JA                     TO W-RIM-SW                             
143100      IF  (DCS-NDC-NA AND DCS-USA)                                        
143200         AND (KORD-KDORDKL = 0 OR 1)                                      
143300         AND KOLLI-IDTRPTNR      = 901                                    
143400**       AND KOLLI-IDLBBET       = 'FEDXPO'                               
143500        MOVE JA     TO W-RIM-US                                           
143600      END-IF                                                              
143700      IF DCS-NDC-NA                                                       
143800        PERFORM S50-HAMTA-USA-FRAKT                                       
143900      END-IF                                                              
144000*        POST SKRIVS SENARE                                               
144100*                                                                         
144200      PERFORM S10-SPARA-ID                                                
144300                                                                          
144400      MOVE 'RIN'                  TO RIN-IDPTYP                           
144500      MOVE IN-IDKUNDNR            TO RIN-IDKUNDNR                         
144600      MOVE ZERO                   TO RIN-IDORDNR                          
144700      MOVE IN-IDORDNR7            TO RIN-IDORDNR                          
144800      MOVE IN-IDPRODNR            TO RIN-IDPRODNR                         
144900      MOVE IN-IDKOLLI             TO RIN-IDKOLLI                          
145000      MOVE KOLLI-VKORDBTO-KOLLI   TO RIN-VKORDBTO-KOLLI                   
145100      MOVE KOLLI-VLORDBTO-KOLLI   TO RIN-VLORDBTO-KOLLI                   
145200      MOVE KOLLI-IDLBBET          TO RIN-IDLBBET                          
145300      MOVE KOLLI-KDEMBTYP         TO RIN-KDEMBTYP                         
145400      MOVE SPACE                  TO RIN-IDFAKT-GNB                       
145500      MOVE SPACE                  TO RIN-FILLERX16                        
145600*                                                                         
145700      MOVE 'RIN'                TO RJX-IDPTYP                             
145800      MOVE RIN-W461RINN-CTX     TO RJX-FILLER                             
145900      MOVE IN-IDFAKT            TO RJX-IDFAKT                             
146000      MOVE IN-IDDISTR           TO RJX-IDDISTR                            
146100      MOVE IN-IDKUNDNR          TO RJX-IDKUNDNR                           
146200      MOVE IN-IDORDNR7          TO RJX-IDORDER                            
146300      MOVE ZERO                 TO RJX-IDKUNDNR-S                         
146400      MOVE IN-IDPRODNR          TO RJX-IDPRODNR                           
146500      MOVE IN-IDKOLLI           TO RJX-IDKOLLI                            
146600      MOVE ZERO                 TO RJX-IDPURAD                            
146700      MOVE ZERO                 TO RJX-IDTRPBON                           
146800*                                                                         
146900      PERFORM S12-SKRIV-W47653                                            
147000      PERFORM S10-SPARA-ID                                                
147100     END-IF                                                               
147200     .                                                                    
147300     EJECT                                                                
147400 FA-SKRIV-RIM-POST  SECTION.                                              
147500     MOVE 'FA-SKRIV-RIM'  TO WS-SEKTION                                   
147600                                                                          
147700     IF WS2-IDDISTR NOT = ZERO                                            
147800       MOVE WS2-IDDISTR          TO TEST-IDDISTR                          
147900       IF WS2-IDDC NOT = DCS-IDDC                                         
148000         MOVE WS2-IDDC           TO W-IDDC-B6                             
148100         PERFORM IMS-GU-WDB601                                            
148200       END-IF                                                             
148300       IF   RIM-US                                                        
148400        IF SPAR-PRFRAKT-LOC > ZERO                                        
148500          MOVE SPAR-PRFRAKT-LOC  TO RIM-PRFRAKT-LOC                       
148600        ELSE                                                              
148700          PERFORM FAA-BERAKNA-PRFRAKT-LOC                                 
148800        END-IF                                                            
148900       ELSE                                                               
149000         MOVE SPAR-PRFRAKT-LOC   TO RIM-PRFRAKT-LOC                       
149100         IF (DCS-CDC OR DCS-DDC) AND                                      
149200             DIST07-NA-CUSTOMERS OR                                       
149300             DIST07-KINA         OR                                       
149400             DIST07-INDIEN                                                
149500           MOVE ZERO             TO  RIM-PRFRAKT-LOC                      
149600         END-IF                                                           
149700       END-IF                                                             
149800*                                                                         
149900       MOVE 'RIM'                TO RJX-IDPTYP                            
150000       MOVE RIM-W461RIM2-CTX     TO RJX-FILLER                            
150100       MOVE WS2-IDFAKT           TO RJX-IDFAKT                            
150200       MOVE ZERO                 TO RJX-IDDISTR                           
150300       MOVE ZERO                 TO RJX-IDKUNDNR                          
150400       MOVE WS2-IDORDNR7         TO RJX-IDORDER                           
150500       MOVE RIM-IDKUNDNR         TO RJX-IDKUNDNR-S                        
150600       MOVE RIM-IDKUNDNR         TO RJX-IDPRODNR                          
150700       MOVE ZERO                 TO RJX-IDKOLLI                           
150800       MOVE ZERO                 TO RJX-IDPURAD                           
150900       MOVE RIM-IDTRPBON         TO RJX-IDTRPBON                          
151000*                                                                         
151100       PERFORM S12-SKRIV-W47653                                           
151200       MOVE ZERO               TO RIM-IDTRPBON                            
151300                                  RIM-IDKUNDNR                            
151400                                  RIM-IDORDNR                             
151500                                  RIM-IDPRODNR                            
151600                                  RIM-TIORDREG                            
151700                                  RIM-KDORDKL                             
151800                                  RIM-KDFRAKT                             
151900                                  RIM-PRFRAKT-LOC                         
152000                                  RIM-IDDEPT                              
152100       MOVE SPACE              TO RIM-BEKUNDRF                            
152200                                  RIM-BEVARREF                            
152300                                  RIM-KDREFNOT                            
152400                                  RIM-IDTRPBOT                            
152500                                  RIM-IDBILREG                            
152600                                  RIM-IDVIN                               
152700                                  RIM-IDCISNR                             
152800       IF IN-IDDC NOT = DCS-IDDC                                          
152900         MOVE IN-IDDC          TO W-IDDC-B6                               
153000         PERFORM IMS-GU-WDB601                                            
153100       END-IF                                                             
153200       IF IN-IDDISTR NUMERIC                                              
153300         MOVE IN-IDDISTR       TO TEST-IDDISTR                            
153400       END-IF                                                             
153500     END-IF                                                               
153600     MOVE NEJ                  TO W-RIM-SW                                
153700     .                                                                    
153800     EJECT                                                                
153900 FAA-BERAKNA-PRFRAKT-LOC  SECTION.                                        
154000     MOVE 'FAA-BERAKNA'  TO WS-SEKTION                                    
154100                                                                          
154200     MOVE ZERO                      TO RIM-PRFRAKT-LOC                    
154300     COMPUTE W-VKORDBTO-ORDER-LB ROUNDED =                                
154400             W-VKORDBTO-ORDER   *                                         
154500             CONV-KG-TO-LB                                                
154600                                                                          
154700     EVALUATE W-VKORDBTO-ORDER-LB                                         
154800       WHEN  0.1 THRU  2.9                                                
154900         MOVE 8.50                  TO RIM-PRFRAKT-LOC                    
155000       WHEN  3.0 THRU  5.9                                                
155100         MOVE 11.15                 TO RIM-PRFRAKT-LOC                    
155200       WHEN  6.0 THRU  9.9                                                
155300         MOVE 15.00                 TO RIM-PRFRAKT-LOC                    
155400       WHEN 10.0 THRU 15.9                                                
155500         MOVE 20.00                 TO RIM-PRFRAKT-LOC                    
155600       WHEN 16.0 THRU 20.9                                                
155700         MOVE 24.15                 TO RIM-PRFRAKT-LOC                    
155800       WHEN 21.0 THRU 25.9                                                
155900         MOVE 28.40                 TO RIM-PRFRAKT-LOC                    
156000       WHEN 26.0 THRU 30.9                                                
156100         MOVE 32.70                 TO RIM-PRFRAKT-LOC                    
156200       WHEN 31.0 THRU 40.9                                                
156300         MOVE 40.50                 TO RIM-PRFRAKT-LOC                    
156400       WHEN 41.0 THRU 50.9                                                
156500         MOVE 47.75                 TO RIM-PRFRAKT-LOC                    
156600       WHEN 51.0 THRU 60.9                                                
156700         MOVE 56.10                 TO RIM-PRFRAKT-LOC                    
156800       WHEN 61.0 THRU 70.9                                                
156900         MOVE 66.20                 TO RIM-PRFRAKT-LOC                    
157000       WHEN 71.0 THRU 80.9                                                
157100         MOVE 76.40                 TO RIM-PRFRAKT-LOC                    
157200       WHEN 81.0 THRU 90.9                                                
157300         MOVE 86.35                 TO RIM-PRFRAKT-LOC                    
157400       WHEN 91.0 THRU 99.9                                                
157500         MOVE 95.05                 TO RIM-PRFRAKT-LOC                    
157600       WHEN 100.0 THRU 110.9                                              
157700         MOVE 105.00                TO RIM-PRFRAKT-LOC                    
157800       WHEN 111.0 THRU 120.9                                              
157900         MOVE 114.50                TO RIM-PRFRAKT-LOC                    
158000       WHEN 121.0 THRU 130.9                                              
158100         MOVE 124.00                TO RIM-PRFRAKT-LOC                    
158200       WHEN 131.0 THRU 140.9                                              
158300         MOVE 133.60                TO RIM-PRFRAKT-LOC                    
158400       WHEN 141.0 THRU 150.9                                              
158500         MOVE 143.15                TO RIM-PRFRAKT-LOC                    
158600       WHEN OTHER                                                         
158700         MOVE SPAR-PRFRAKT-LOC      TO RIM-PRFRAKT-LOC                    
158800     END-EVALUATE                                                         
158900                                                                          
159000     .                                                                    
159100     EJECT                                                                
159200 G-SKAPA-RAD-POST2 SECTION.                                               
159300     MOVE 'G-SKAPA-RAD-POST2'   TO WS-SEKTION                             
159400*                                                                         
159500     IF IN-IDKOLLI NOT = ZERO AND IN-IDPURAD NOT = ZERO                   
159600       MOVE ORAD-IDARTNR         TO W-IDARTNR                             
159700       PERFORM IMS-GU-WDK601                                              
159800       PERFORM IMS-GNP-WDK611                                             
159900*                                                                         
160000       MOVE 'RIO'                TO RIO-IDPTYP                            
160100       MOVE ZERO                 TO RIO-IDORDNR                           
160200       MOVE IN-IDORDNR7          TO RIO-IDORDNR                           
160300       MOVE ORAD-IDARTNR         TO RIO-IDARTNR                           
160400       MOVE ORAD-REKSIFFR        TO RIO-REKSIFFR                          
160500       MOVE ORAD-BERADREF        TO RIO-BERADREF                          
160600       MOVE ORAD-KVBEART         TO RIO-KVBEART                           
160700       MOVE KKOLLI-KVLEVART      TO RIO-KVLEVART                          
160800*        SERVICEGRAD                                                      
160900       IF ORAD-KVBEART = ZERO                                             
161000          MOVE ZERO              TO RIO-RESERVG                           
161100       ELSE                                                               
161200          COMPUTE RIO-RESERVG = ORAD-KVLEVART /                           
161300                 ORAD-KVBEART * 100                                       
161400       END-IF                                                             
161500*      MOVE CLAG-PRARTBTO-EXP    TO RIO-PRARTBTO-EXP                      
161600       IF IN-IDDC NOT = DCS-IDDC                                          
161700         MOVE IN-IDDC              TO W-IDDC-B6                           
161800         PERFORM IMS-GU-WDB601                                            
161900       END-IF                                                             
162000       MOVE IN-IDDC   TO WS-IDDC                                          
162100       MOVE IN-IDDISTR TO TEST-IDDISTR                                    
162200       IF ((NDC-CN OR LDC-CN) AND DIS130-NOAC-KINA-C1) OR                 
162300           (NDC-IN            AND DIST07-INDIEN)       OR                 
162400           (NDC-KR            AND DIST07-KOREA)        OR                 
162500           (NDC-TR            AND DIST07-TURKEY)       OR                 
162600           (NDC-MY            AND DIST07-MALAYSIA)     OR                 
162700           (NDC-TH            AND DIST07-THAILAND)     OR                 
162800           (NDC-TW            AND DIST07-TAIWAN)       OR                 
162900           (NDC-MX            AND DIST07-MEXICO)       OR                 
163000           (NDC-BR            AND DIST07-BRAZIL)       OR                 
163100           (NDC-ZA            AND DIST07-S-AFRICA)                        
163200          PERFORM S08-W335PRIS                                            
163300          MOVE PRIS-PRARTNTO TO RIO-PRARTNTO                              
163400       ELSE                                                               
163500          MOVE ORAD-PRARTNTO        TO RIO-PRARTNTO                       
163600       END-IF                                                             
163700       MOVE IN-IDFKNGRP          TO RIO-IDFKNGRP                          
163800       MOVE ORAD-KDPRODSL        TO RIO-KDPRODSL                          
163900       MOVE ORAD-KDDSP           TO RIO-KDDSP                             
164000       MOVE CLAG-KDVVKL          TO RIO-KDVVKL                            
164100       MOVE ORAD-KDVRINFO        TO RIO-KDVRINFO                          
164200       MOVE ORAD-FLINVEST        TO RIO-FLINVEST                          
164300       MOVE ORAD-FLDIRLEV        TO RIO-FLDIRLEV                          
164400       IF ((DCS-CDC OR DCS-DDC)                                           
164500       AND (DIST07-NA-CUSTOMERS))                                         
164600       OR (DCS-NDC-NA)                                                    
164700          MOVE VORD-FLDIRLEV     TO RIO-FLDIRLEV                          
164800       END-IF                                                             
164900       MOVE ORAD-FLPRTILL        TO RIO-FLPRTILL                          
165000       IF ORAD-KDPRTYP = 'P'                                              
165100         MOVE 'M'                TO RIO-FLPRTILL                          
165200       END-IF                                                             
165300       PERFORM S07-W335PRIS                                               
165400       MOVE PRIS-KDARTRAB        TO RIO-KDRABATT                          
165500       MOVE PRIS-PRARTBTO-MARK   TO RIO-PRARTBTO-EXP                      
165600       MOVE KORD-IDDC            TO RIO-IDDC                              
165700       MOVE CLAG-KDPSLLOC        TO RIO-KDPSLLOC                          
165800       MOVE ORAD-PRAVCOST        TO IN-PRAVCOST                           
165900**** AVCOST SHOULD BE SAVED IN WDE411 IN PROGRAM 4637                     
166000       IF IN-PRAVCOST = ZERO                                              
166100         PERFORM S40-HAMTA-PRAVCOST                                       
166200       END-IF                                                             
166300       MOVE IN-PRAVCOST          TO RIO-PRAVCOST                          
166400       MOVE IN-PRAVCOST-CORE     TO RIO-PRAVCOST-CORE                     
166500       MOVE ORAD-IDBIL           TO RIO-IDBIL                             
166600*          (FAKT-BILLIT ..  PULS-PRISER)                                  
166700       MOVE ZERO                 TO RIO-PRARTNTO-LOC                      
166800       MOVE ZERO                 TO RIO-PRARTBTO-LOC                      
166900       MOVE ZERO                 TO RIO-RERAB                             
167000       MOVE SPACE                TO RIO-KDRAB                             
167100       MOVE SPACE                TO RIO-KDVAT                             
167200       MOVE ZERO                 TO RIO-PRARTSJK                          
167300       MOVE ZERO                 TO RIO-PRARTSTD                          
167400       MOVE IN-IDDISTR           TO TEST-IDDISTR                          
167500       IF DIST79-DEALER-PRICE                                             
167600         MOVE ORAD-PRARTNTO-LOC    TO RIO-PRARTNTO-LOC                    
167700         MOVE ORAD-PRARTBTO-LOC    TO RIO-PRARTBTO-LOC                    
167800         MOVE ORAD-RERAB           TO RIO-RERAB                           
167900         MOVE ORAD-KDRAB           TO RIO-KDRAB                           
168000         MOVE CLAG-PRARTSTD        TO RIO-PRARTSTD                        
168100*EÖ   -   NY SJÄLVKOST                                                    
168200         MOVE '11'                 TO COST-IDDC                           
168300         MOVE W-IDARTNR            TO COST-IDARTNR                        
168400         MOVE ZERO                 TO COST-PRARTBES-MON                   
168500                                      COST-PRARTBES-MONLOC                
168600                                      COST-PRARTSJK-MON                   
168700                                      COST-PRARTSJK-MONLOC                
168800         MOVE SPACE                TO COST-IDLEVNR                        
168900         MOVE IN-DAFINDOC(5:2)     TO COST-TIMM                           
169000         IF COST-TIMM = ZERO                                              
169100           MOVE W-TIAAMMDD(3:2)    TO COST-TIMM                           
169200         END-IF                                                           
169300                                                                          
169400         CALL W335COST USING COST-W335COST COST-WDK6-PCB                  
169500                                           COST-WDK7-PCB                  
169600                                           COST-WDF1-PCB                  
169700                                           COST-9305-PCB                  
169800                                           COST-WDK72-PCB                 
169900                                           COST-WDB6-PCB                  
170000         MOVE COST-PRARTSJK-MON    TO RIO-PRARTSJK                        
170100*EÖ                                                                       
170200         PERFORM GA-PRIS-TILL-KOSTNADSVALUTA                              
170300         MOVE ORAD-KDVAT           TO RIO-KDVAT                           
170400         IF RIO-PRARTNTO-LOC = ZERO                                       
170500           PERFORM XX-RIO                                                 
170600           MOVE XX-PRARTNTO        TO RIO-PRARTNTO-LOC                    
170700           IF RIO-PRARTBTO-LOC = ZERO                                     
170800            MOVE XX-PRARTBTO       TO RIO-PRARTBTO-LOC                    
170900           END-IF                                                         
171000         END-IF                                                           
171100       END-IF                                                             
171200*                                                                         
171300       MOVE ORAD-BEVOLREF          TO RIO-BEVOLREF                        
171400       MOVE ORAD-IDVIN             TO RIO-IDVIN                           
171500*                                                                         
171600*      FETCH TRACKING NO. FOR NDC-MX                                      
171700       IF NDC-MX                                                          
171800*--      KEYS WDE1                                                        
171900         MOVE IN-IDSHIPM           TO W-IDSHIPM                           
172000         MOVE IN-IDDISTR           TO W-IDDISTR-E1                        
172100         MOVE IN-IDKUNDNR          TO W-IDKUNDNR-E1                       
172200         MOVE IN-IDPURAD           TO W-IDPURAD-E1                        
172300         MOVE IN-IDPRODNR          TO W-IDPRODNR-E1                       
172400         MOVE IN-IDKOLLI           TO W-IDKOLLI-E1                        
172500                                                                          
172600         PERFORM IMS-GU-WDE131                                            
172700         PERFORM IMS-GNP-WDE141                                           
172800         IF SEGMENT-FINNS                                                 
172900           PERFORM UNTIL SEGMENT-SAKNAS                                   
173000             MOVE TLEV-IDTRACK(1:15)                                      
173100                                   TO RIO-IDTRACK                         
173200             MOVE TLEV-KVTRACK-LEV TO RIO-KVLEVART                        
173300             MOVE TLEV-DADATUM     TO RIO-DADATUM                         
173400                                                                          
173500             MOVE 'RIO'            TO RJX-IDPTYP                          
173600             MOVE RIO-W461RIO2     TO RJX-FILLER                          
173700             MOVE IN-IDFAKT        TO RJX-IDFAKT                          
173800             MOVE IN-IDDISTR       TO RJX-IDDISTR                         
173900             MOVE IN-IDKUNDNR      TO RJX-IDKUNDNR                        
174000             MOVE IN-IDORDNR7      TO RJX-IDORDER                         
174100             MOVE ZERO             TO RJX-IDKUNDNR-S                      
174200             MOVE IN-IDPRODNR      TO RJX-IDPRODNR                        
174300             MOVE IN-IDKOLLI       TO RJX-IDKOLLI                         
174400             MOVE IN-IDPURAD       TO RJX-IDPURAD                         
174500             MOVE ZERO             TO RJX-IDTRPBON                        
174600                                                                          
174700             PERFORM S12-SKRIV-W47653                                     
174800             PERFORM IMS-GNP-WDE141                                       
174900           END-PERFORM                                                    
175000         ELSE                                                             
175100           MOVE SPACE              TO RIO-IDTRACK                         
175200           MOVE ZERO               TO RIO-DADATUM                         
175300           MOVE 'RIO'              TO RJX-IDPTYP                          
175400           MOVE RIO-W461RIO2       TO RJX-FILLER                          
175500           MOVE IN-IDFAKT          TO RJX-IDFAKT                          
175600           MOVE IN-IDDISTR         TO RJX-IDDISTR                         
175700           MOVE IN-IDKUNDNR        TO RJX-IDKUNDNR                        
175800           MOVE IN-IDORDNR7        TO RJX-IDORDER                         
175900           MOVE ZERO               TO RJX-IDKUNDNR-S                      
176000           MOVE IN-IDPRODNR        TO RJX-IDPRODNR                        
176100           MOVE IN-IDKOLLI         TO RJX-IDKOLLI                         
176200           MOVE IN-IDPURAD         TO RJX-IDPURAD                         
176300           MOVE ZERO               TO RJX-IDTRPBON                        
176400                                                                          
176500           PERFORM S12-SKRIV-W47653                                       
176600         END-IF                                                           
176700       ELSE                                                               
176800         MOVE SPACE              TO RIO-IDTRACK                           
176900         MOVE ZERO               TO RIO-DADATUM                           
177000         MOVE 'RIO'              TO RJX-IDPTYP                            
177100         MOVE RIO-W461RIO2       TO RJX-FILLER                            
177200         MOVE IN-IDFAKT          TO RJX-IDFAKT                            
177300         MOVE IN-IDDISTR         TO RJX-IDDISTR                           
177400         MOVE IN-IDKUNDNR        TO RJX-IDKUNDNR                          
177500         MOVE IN-IDORDNR7        TO RJX-IDORDER                           
177600         MOVE ZERO               TO RJX-IDKUNDNR-S                        
177700         MOVE IN-IDPRODNR        TO RJX-IDPRODNR                          
177800         MOVE IN-IDKOLLI         TO RJX-IDKOLLI                           
177900         MOVE IN-IDPURAD         TO RJX-IDPURAD                           
178000         MOVE ZERO               TO RJX-IDTRPBON                          
178100*                                                                         
178200         PERFORM S12-SKRIV-W47653                                         
178300       END-IF                                                             
178400*                                                                         
178500*      FÖR DANMARK EV.RIZ-POST                                            
178600       MOVE IN-IDDISTR           TO DIS1-IDDISTR                          
178700       CALL W460DIS1 USING DIS1-W460DIS1                                  
178800       IF DIS1-IDLANDX2 = ISO-DANMARK                                     
178900         MOVE 'RIZ'                 TO RIZ-IDPTYP                         
179000         MOVE CLAG-KDSRA            TO RIZ-KDSRA                          
179100         MOVE ART-KDSORT            TO RIZ-KDSORT                         
179200         MOVE CLAG-KVQPACK-1        TO RIZ-KVQPACK-1                      
179300         MOVE CLAG-KDARTURS         TO ARTU-KDARTURS                      
179400         MOVE SPACE                 TO ARTU-IDDC                          
179500         MOVE ZERO                  TO ARTU-IDDISTR                       
179600         CALL W400ARTU USING ARTU-W400ARTU                                
179700         MOVE ARTU-KDARTURS-NUM     TO RIZ-KDARTURS                       
179800                                                                          
179900         MOVE CLAG-IDSTATNR (GMT-KDSTATNR)                                
180000                                    TO RIZ-IDSTATNR                       
180100         IF RIZ-IDSTATNR = ZERO                                           
180200            MOVE GEN-IDSTATNR       TO RIZ-IDSTATNR                       
180300         END-IF                                                           
180400         MOVE IN-BEART              TO RIZ-BEART                          
180500         MOVE CLAG-VKART            TO RIZ-VKART                          
180600*                                                                         
180700         MOVE 'RIP'                TO RJX-IDPTYP                          
180800*       RIP O RIZ BYTER PLATS I SORT.NYCKEL FÖR RÄTT SORTERING            
180900         MOVE RIZ-W461RIZN-CTX     TO RJX-FILLER                          
181000         MOVE IN-IDFAKT            TO RJX-IDFAKT                          
181100         MOVE IN-IDDISTR           TO RJX-IDDISTR                         
181200         MOVE IN-IDKUNDNR          TO RJX-IDKUNDNR                        
181300         MOVE IN-IDORDNR7          TO RJX-IDORDER                         
181400         MOVE ZERO                 TO RJX-IDKUNDNR-S                      
181500         MOVE IN-IDPRODNR          TO RJX-IDPRODNR                        
181600         MOVE IN-IDKOLLI           TO RJX-IDKOLLI                         
181700         MOVE IN-IDPURAD           TO RJX-IDPURAD                         
181800         MOVE ZERO                 TO RJX-IDTRPBON                        
181900*                                                                         
182000         PERFORM S12-SKRIV-W47653                                         
182100       END-IF                                                             
182200                                                                          
182300*                                                                         
182400      IF ORAD-IDKUNDRF-RO  = SPACES  OR                                   
182500        =  WRO-IDKUNDRF                                                   
182600        CONTINUE                                                          
182700      ELSE                                                                
182800        MOVE 'RIP'                  TO RIP-IDPTYP                         
182900        MOVE ORAD-IDKUNDRF-RO       TO W-KUNDREF2                         
183000        IF W-IDKUNDRF-RO2 > ZERO                                          
183100          MOVE ZERO                 TO RIP-IDRONR                         
183200          MOVE W-IDKUNDRF-RO2       TO RIP-IDRONR(3:5)                    
183300        ELSE                                                              
183400          MOVE ZERO                 TO RIP-IDRONR                         
183500        END-IF                                                            
183600        MOVE ORAD-TIRODAT           TO RIP-TIORDREG                       
183700        IF ORAD-TIRODAT = ZERO                                            
183800          MOVE JA                   TO RIP-FLIHOP                         
183900        ELSE                                                              
184000          MOVE NEJ                  TO RIP-FLIHOP                         
184100        END-IF                                                            
184200        MOVE ORAD-BEVOLREF          TO RIP-BEVOLREF                       
184300        MOVE ORAD-KDORDKL           TO RIP-KDORDKL                        
184400        MOVE SPACES                 TO RIP-FILLERX52                      
184500*                                                                         
184600        MOVE 'RIZ'                 TO RJX-IDPTYP                          
184700*       RIP O RIZ BYTER PLATS I SORT.NYCKEL FÖR RÄTT SORTERING            
184800        MOVE RIP-W461RIPN-CTX      TO RJX-FILLER                          
184900        MOVE IN-IDFAKT             TO RJX-IDFAKT                          
185000        MOVE IN-IDDISTR            TO RJX-IDDISTR                         
185100        MOVE IN-IDKUNDNR           TO RJX-IDKUNDNR                        
185200        MOVE IN-IDORDNR7           TO RJX-IDORDER                         
185300        MOVE ZERO                  TO RJX-IDKUNDNR-S                      
185400        MOVE IN-IDPRODNR           TO RJX-IDPRODNR                        
185500        MOVE IN-IDKOLLI            TO RJX-IDKOLLI                         
185600        MOVE IN-IDPURAD            TO RJX-IDPURAD                         
185700        MOVE ZERO                  TO RJX-IDTRPBON                        
185800*                                                                         
185900        PERFORM S12-SKRIV-W47653                                          
186000      END-IF                                                              
186100     ELSE                                                                 
186200       MOVE IN-IDDISTR             TO TEST-IDDISTR                        
186300       IF DIST35-CDC-AU-REFILL OR DIST07-AUSTRALIEN                       
186400*        SKALL EJ HA T/A I VIPS-FILEN                                     
186500         CONTINUE                                                         
186600       ELSE                                                               
186700        IF IN-BEART = 'FREIGHT'                                           
186800          MOVE IN-SUNTO-LINE     TO RIL-PRFRAKT                           
186900                                    SPAR-PRFRAKT-LOC                      
187000        END-IF                                                            
187100        IF IN-BEART = 'INSURANCE'                                         
187200          MOVE IN-SUNTO-LINE     TO RIL-PRFOERS                           
187300        END-IF                                                            
187400        IF IN-BEART = 'PACKING & HANDLING'                                
187500          MOVE IN-SUNTO-LINE     TO RIL-PREMBHNT                          
187600        END-IF                                                            
187700        IF IN-BEART = 'SERVICE FEE'                                       
187800          MOVE IN-SUNTO-LINE     TO RIL-PREMBHNT                          
187900        END-IF                                                            
188000        IF IN-BEART = 'LEGAL'                                             
188100          MOVE IN-SUNTO-LINE     TO RIL-PRLEGKST                          
188200        END-IF                                                            
188300        IF IN-BEART = 'REDUCTION'                                         
188400          MOVE IN-SUNTO-LINE     TO RIL-PRAVDRAG                          
188500        END-IF                                                            
188600       END-IF                                                             
188700                                                                          
188800     END-IF                                                               
188900     .                                                                    
189000     EJECT                                                                
189100 GA-PRIS-TILL-KOSTNADSVALUTA  SECTION.                                    
189200                                                                          
189300     PERFORM S25-MARKNADSBOLAGS-VALUTA                                    
189400     .                                                                    
189500     EJECT                                                                
189600 XX-RIO SECTION.                                                          
189700     COMPUTE XX-PRARTNTO = IN-SUNTO-LINE /                                
189800                           KKOLLI-KVLEVART                                
189900     COMPUTE XX-PRARTBTO = IN-SUBTO-LINE /                                
190000                           KKOLLI-KVLEVART                                
190100     .                                                                    
190200     EJECT                                                                
190300 H-SKAPA-STATISTIK-POST3 SECTION.                                         
190400     MOVE 'H-SKAPA-STATISTIK-POST3'  TO WS-SEKTION                        
190500*      SALES STATISTICS  (W330099)                                        
190600                                                                          
190700     MOVE IN-W476FAKT           TO BILL-W4760001                          
190800     MOVE BILL-IDPRODNR         TO W-IDPRODNR-E4                          
190900     MOVE BILL-IDPURAD          TO W-IDPURAD-E4                           
191000     IF IN-IDPURAD = ZERO AND IN-IDKOLLI = ZERO                           
191100       CONTINUE                                                           
191200     ELSE                                                                 
191300                                                                          
191400*    PERFORM IMS-GU-WDE411-BSEQ                                           
191500*    IF SEGMENT-FINNS                                                     
191600       MOVE SPACE                 TO UT3-IDPTYP                           
191700                                     W-KDVALISO                           
191800       MOVE BILL-DAFINDOC         TO UT3-TIFAKT                           
191900       MOVE BILL-IDDISTR          TO UT3-IDDISTR                          
192000                                     TEST-IDDISTR                         
192100       MOVE ORAD-IDARTNR          TO UT3-IDARTNR                          
192200       MOVE KKOLLI-KVLEVART       TO UT3-KVLEVART                         
192300       MOVE ORAD-PRARTNTO         TO UT3-PRARTNTO                         
192400       IF DIST79-DEALER-PRICE                                             
192500         MOVE ORAD-PRARTNTO-LOC   TO UT3-PRARTNTO                         
192600         IF UT3-PRARTNTO = ZERO                                           
192700           MOVE ORAD-PRARTNTO-LOCPREL TO UT3-PRARTNTO                     
192800         END-IF                                                           
192900         IF UT3-PRARTNTO = ZERO                                           
193000           PERFORM XX-UT3                                                 
193100           MOVE XX-PRARTNTO        TO UT3-PRARTNTO                        
193200         END-IF                                                           
193300*      OMVANDLA TILL SEK ?                                                
193400         IF BILL-KDVALISO-FAKT = 'SEK' OR '   '                           
193500           CONTINUE                                                       
193600         ELSE                                                             
193700           PERFORM S01-OMVANDLA-TILL-SEK                                  
193800         END-IF                                                           
193900       ELSE                                                               
194000         IF DIST79-ECOM-PRICE                                             
194100           MOVE ORAD-PRARTNTO-LOC TO UT3-PRARTNTO                         
194200           IF BILL-KDVALISO-FAKT = 'SEK' OR '   '                         
194300             CONTINUE                                                     
194400           ELSE                                                           
194500             PERFORM S01-OMVANDLA-TILL-SEK                                
194600           END-IF                                                         
194700         END-IF                                                           
194800       END-IF                                                             
194900       MOVE ORAD-KDPRTYP          TO UT3-KDPRTYP                          
195000       MOVE ORAD-KDORDKL          TO UT3-KDORDKL                          
195100                                                                          
195200       PERFORM S13-SKRIV-W47654                                           
195300     END-IF                                                               
195400     .                                                                    
195500     EJECT                                                                
195600 XX-UT3 SECTION.                                                          
195700     COMPUTE XX-PRARTNTO = BILL-SUNTO-LINE /                              
195800                           KKOLLI-KVLEVART                                
195900     COMPUTE XX-PRARTBTO = BILL-SUBTO-LINE /                              
196000                           KKOLLI-KVLEVART                                
196100     .                                                                    
196200     EJECT                                                                
196300 I-SKAPA-INFO-POST4 SECTION.                                              
196400     MOVE 'I-SKAPA-INFO-POST4'  TO WS-SEKTION                             
196500*      DDGS-INFO  (W46341)                                                
196600                                                                          
196700     MOVE IN-W476FAKT           TO BILL-W4760001                          
196800     MOVE BILL-IDPRODNR         TO W-IDPRODNR-E4                          
196900     MOVE BILL-IDPURAD          TO W-IDPURAD-E4                           
197000     IF IN-IDPURAD = ZERO AND IN-IDKOLLI = ZERO                           
197100       CONTINUE                                                           
197200     ELSE                                                                 
197300                                                                          
197400       IF VORD-IDPRODNR NOT = BILL-IDPRODNR OR                            
197500          KOLLI-IDKOLLI NOT = BILL-IDKOLLI                                
197600         MOVE BILL-IDPRODNR          TO W-IDPRODNR-E6                     
197700         PERFORM IMS-GU-WDE601                                            
197800         MOVE BILL-IDKOLLI           TO W-IDKOLLI-E6                      
197900         PERFORM IMS-GNP-WDE611                                           
198000       END-IF                                                             
198100                                                                          
198200       INITIALIZE                    UT4-W46341                           
198300       MOVE BILL-IDPRODNR         TO UT4-IDPRODNR                         
198400       MOVE BILL-IDDC             TO UT4-IDDC                             
198500       MOVE BILL-DAFINDOC         TO UT4-DAFAKT                           
198600       MOVE 20                    TO UT4-DASKEPPN(1:2)                    
198700       MOVE BILL-TISKEPPN         TO W-DATUM                              
198800       MOVE W-DATUM6              TO UT4-DASKEPPN(3:6)                    
198900       MOVE BILL-IDDISTR          TO UT4-IDDISTR                          
199000       MOVE BILL-IDKUNDNR         TO UT4-IDKUNDNR                         
199100       MOVE BILL-IDORDNR7         TO UT4-IDORDNR7                         
199200       MOVE BILL-IDKOLLI          TO UT4-IDKOLLI                          
199300       MOVE BILL-IDPURAD          TO UT4-IDRADNR                          
199400*                                                                         
199500       MOVE ORAD-IDARTNR          TO UT4-IDARTNR                          
199600       MOVE ORAD-BERADREF         TO UT4-BERADREF                         
199700       IF ORAD-TISLULEV NOT = ZERO                                        
199800         MOVE 20                  TO UT4-DALEVDAT(1:2)                    
199900         MOVE ORAD-TISLULEV       TO W-DATUM                              
200000         MOVE W-DATUM6            TO UT4-DALEVDAT(3:6)                    
200100       END-IF                                                             
200200       MOVE ORAD-KDORDKL          TO UT4-KDORDKL                          
200300       MOVE KKOLLI-KVLEVART       TO UT4-KVANTAL                          
200400*                                                                         
200500       MOVE KOLLI-IDLEVNR         TO UT4-IDLEVNR                          
200600       MOVE KOLLI-IDSUPREF        TO UT4-IDSUPREF                         
200700       IF KOLLI-TIPACKN NOT = ZERO                                        
200800         MOVE 20                  TO UT4-DAPACKN(1:2)                     
200900         MOVE KOLLI-TIPACKN       TO W-DATUM                              
201000         MOVE W-DATUM6            TO UT4-DAPACKN(3:6)                     
201100       END-IF                                                             
201200       IF KOLLI-DASUPREF  NOT = ZERO                                      
201300         MOVE 20                  TO UT4-DASUPREF(1:2)                    
201400         MOVE KOLLI-DASUPREF      TO W-DATUM                              
201500         MOVE W-DATUM6            TO UT4-DASUPREF(3:6)                    
201600       END-IF                                                             
201700       MOVE KOLLI-TIPACTID        TO UT4-TIPACTID                         
201800       MOVE KOLLI-TISUPTID        TO UT4-TISUPTID                         
201900       MOVE KOLLI-KDVIA           TO UT4-KDVIA                            
202000*                                                                         
202100       MOVE 20                    TO UT4-DAREGDAT(1:2)                    
202200       MOVE KORD-TIORDREG         TO W-DATUM                              
202300       MOVE W-DATUM6              TO UT4-DAREGDAT(3:6)                    
202400                                                                          
202500       PERFORM S14-SKRIV-W47658                                           
202600     END-IF                                                               
202700     .                                                                    
202800     EJECT                                                                
202900 J-SKAPA-HUVUD-POSTER5   SECTION.                                         
203000     MOVE 'J-SKAPA-HUVUD-POSTER5'  TO WS-SEKTION                          
203100*      EKONOMI-TRANSAR   (W51060)                                         
203200                                                                          
203300     MOVE IN-W476FAKT            TO BILL-W4760001                         
203400**** SENDING DC ON ORDERROW                                               
203500*    IF IN-IDDC NOT = DCS-IDDC                                            
203600       MOVE IN-IDDC              TO W-IDDC-B6                             
203700       PERFORM IMS-GU-WDB601                                              
203800       MOVE DCS-IDFTG            TO WS-WDB6-IDFTG                         
203900*    END-IF                                                               
204000     MOVE IN-IDDISTR             TO TEST-IDDISTR                          
204100     MOVE SPACE                  TO WS-KDVALISO                           
204200     IF WS5-IDFAKT NOT = WS-IDFAKT                                        
204300                                                                          
204400       IF DIST35-NONVCC-VCC-TRANSFER                                      
204500       OR DIST35-VCC-NONVCC-TRANSFER                                      
204600         PERFORM S03-HAMTA-KUNDC                                          
204700       ELSE                                                               
204800         IF IN-IDKUNDNR NOT = 0                                           
204900           PERFORM S03-HAMTA-KUND                                         
205000         ELSE                                                             
205100           MOVE SPACE             TO BET-KDTRADP                          
205200           PERFORM S03-HAMTA-KUNDA                                        
205300           IF BET-KDTRADP = SPACE                                         
205400             PERFORM S03-HAMTA-KUNDB                                      
205500           END-IF                                                         
205600         END-IF                                                           
205700       END-IF                                                             
205800       MOVE BET-KDVALISO TO UT5-EKHT-KDVALISO                             
205900                                                                          
206000**** HERE WE CREATE THE FIRST POST                                        
206100       PERFORM S05-BUILD-COMMON                                           
206200                                                                          
206300       PERFORM JA-BUILD-WRITE-INVOICE                                     
206400       IF  (IN-SUVAT-FAKT > ZERO)                                         
206500         PERFORM JB-F01-BUILD-WRITE-VAT                                   
206600       END-IF                                                             
206700                                                                          
206800     END-IF                                                               
206900     .                                                                    
207000     EJECT                                                                
207100                                                                          
207200 JA-BUILD-WRITE-INVOICE     SECTION.                                      
207300     MOVE 'JA-BUILD'  TO WS-SEKTION                                       
207400     MOVE 'SUM'                TO UT5-EKHT-KDEKNIVA                       
207500     MOVE IN-IDDC              TO UT5-EKHT-IDDC-SEND                      
207600     MOVE IN-SUBTO-TOT         TO UT5-EKHT-SUBEL                          
207700     MOVE IN-SUVAT-FAKT        TO UT5-EKHT-SUVAT                          
207800     MOVE WS-WDB2-IDFTG        TO WS-IDFTG                                
207900     IF DIST79-DEALER-PRICE                                               
208000       PERFORM S02-HAMTA-VALUTAKURS                                       
208100       MOVE WS-PRKURS          TO UT5-EKHT-PRKURS                         
208200     ELSE                                                                 
208300      IF KORD-SUORDV NOT = ZERO OR                                        
208400        (KORD-SUORDV-LEVPL NOT = ZERO AND ORAD-FLDIRLEV = JA)             
208500        IF BET-KDVALISO NOT = IN-KDVALISO-FAKT                            
208600          MOVE BET-KDVALISO    TO UT5-EKHT-KDVALISO                       
208700          IF BET-KDVALISO NOT = WS-KDVALISO                               
208800            PERFORM S02-BET-HAMTA-VALUTAKURS                              
208900          END-IF                                                          
209000          MOVE WS-PRKURS       TO UT5-EKHT-PRKURS                         
209100          IF UT5-EKHT-KDVALISO = SPACE OR 'SEK'                           
209200             MOVE 'SEK'        TO UT5-EKHT-KDVALISO                       
209300             MOVE 1.0          TO UT5-EKHT-PRKURS                         
209400          END-IF                                                          
209500        ELSE                                                              
209600          MOVE 'SEK'           TO W-KDVALISO                              
209700                                  WS-KDVALISO                             
209800                                  ORAD-KDVALISO                           
209900          MOVE 1.0             TO WS-PRKURS                               
210000                                  UT5-EKHT-PRKURS                         
210100        END-IF                                                            
210200        MOVE IN-KDVALISO-FAKT  TO UT5-EKHT-KDVALISO                       
210300        MOVE IN-PRKURS-FAKT    TO UT5-EKHT-PRKURS                         
210400      ELSE                                                                
210500        PERFORM S02-HAMTA-VALUTAKURS                                      
210600        MOVE WS-PRKURS         TO UT5-EKHT-PRKURS                         
210700      END-IF                                                              
210800     END-IF                                                               
210900     MOVE ORAD-IDKONTO         TO UT5-EKHT-IDKONTO                        
211000     MOVE ORAD-IDANALYS        TO UT5-EKHT-IDANALYS                       
211100     MOVE ORAD-IDKST           TO UT5-EKHT-IDKST                          
211200     MOVE ORAD-KDVAT           TO UT5-EKHT-BEVAT                          
211300     MOVE SPACE                TO UT5-EKHT-FLDCET                         
211400     MOVE SPACE                TO UT5-EKHT-IDKUNDRF                       
211500     MOVE SPACE                TO UT5-EKHT-IDFAKT-EXP                     
211600                                                                          
211700     IF (DIST35-NONVCC-NONVCC-REFILL AND NOT DCS-CDC)                     
211800     OR (DIST35-NONVCC-NONVCC-TRANSFER AND NOT DCS-CDC)                   
211900     OR DIST35-NONVCC-CDC-REFILL                                          
212000     OR DIST35-NONVCC-VCC-REFILL                                          
212100     OR DIST35-NONVCC-VCC-TRANSFER                                        
212200       EVALUATE TRUE                                                      
212300         WHEN DCS-CHINA                                                   
212400**** HÄR SKRIVER VI FÖRSTA POSTEN FÖR KINA EXPORT                         
212500**** HÄR SKRIVER VI FÖRSTA POSTEN FÖR FAKTURA 1 FRÅN KINA                 
212600            MOVE DCS-KDVALISO TO UT5-EKHT-KDVALISO                        
212700            MOVE DCS-KDTRADP TO UT5-EKHT-KDTRADP                          
212800            MOVE 'W570EKHA'  TO UT5-EKHT-IDCPYTXT                         
212900            PERFORM S16-SKRIV-W4768E                                      
213000         WHEN DCS-USA                                                     
213100**** HÄR SKRIVER VI FÖRSTA POSTEN FÖR US EXPORT                           
213200**** HÄR SKRIVER VI FÖRSTA POSTEN FÖR FAKTURA 1 FRÅN USA                  
213300            MOVE DCS-KDVALISO TO UT5-EKHT-KDVALISO                        
213400            MOVE DCS-KDTRADP TO UT5-EKHT-KDTRADP                          
213500            MOVE 'W561EKHA'  TO UT5-EKHT-IDCPYTXT                         
213600            PERFORM S21-SKRIV-W4768G                                      
213700         WHEN DCS-INDIA                                                   
213800            MOVE DCS-KDVALISO TO UT5-EKHT-KDVALISO                        
213900            MOVE DCS-KDTRADP TO UT5-EKHT-KDTRADP                          
214000            MOVE 'W515EKHA'  TO UT5-EKHT-IDCPYTXT                         
214100            PERFORM S17-SKRIV-W4768F                                      
214200         WHEN OTHER                                                       
214300            MOVE DCS-KDTRADP TO UT5-EKHT-IDCPYTXT(1:4)                    
214400                                UT5-EKHT-KDTRADP                          
214500            MOVE 'EKHA'     TO UT5-EKHT-IDCPYTXT(5:4)                     
214600            MOVE DCS-KDVALISO TO UT5-EKHT-KDVALISO                        
214700            PERFORM S16-SKRIV-W4768E                                      
214800       END-EVALUATE                                                       
214900     ELSE                                                                 
215000**** SKALL INTE SKRIVAS NÅGON SUMMARAD FÖR INLEVERANS                     
215100**** SKROT SOM KOMMER FRÅN KINA                                           
215200       IF DIST18-SKROT                                                    
215300         IF WS-IDKUNDNR-CHECK(1:5) = ' '                                  
215400         AND WS-IDKUNDNR-CHECK(6:2) = IN-IDDC                             
215500            MOVE 'SEPV' TO UT5-EKHT-KDTRADP                               
215600         ELSE                                                             
215700            MOVE 'SEPV' TO UT5-EKHT-KDTRADP                               
215800            PERFORM S15-SKRIV-W47660                                      
215900         END-IF                                                           
216000       ELSE                                                               
216100**** HÄR SKRIVER VI FÖRSTA POSTEN FÖR FAKTURA 2 FRÅN SEPV                 
216200          IF DIST35-NONVCC-NONVCC-REFILL AND DCS-CDC                      
216300          OR DIST35-NONVCC-NONVCC-TRANSFER AND DCS-CDC                    
216400                                                                          
216500            IF IN-IDPURAD = ZERO AND IN-IDKOLLI = ZERO                    
216600               MOVE SPACE          TO UT5-EKHT-IDFAKT-EXP                 
216700               CONTINUE                                                   
216800            ELSE                                                          
216900               IF IN-IDPRODNR NOT = VORD-IDPRODNR OR                      
217000                  IN-IDKOLLI NOT  = KOLLI-IDKOLLI                         
217100                 MOVE IN-IDPRODNR  TO W-IDPRODNR-E6                       
217200                 PERFORM IMS-GU-WDE601                                    
217300                 MOVE IN-IDKOLLI   TO W-IDKOLLI-E6                        
217400                 PERFORM IMS-GNP-WDE611                                   
217500               END-IF                                                     
217600                                                                          
217700               MOVE KOLLI-IDFAKT-EXP TO UT5-EKHT-IDFAKT-EXP               
217800            END-IF                                                        
217900                                                                          
218000            MOVE 'SEPV' TO UT5-EKHT-KDTRADP                               
218100            PERFORM S15-SKRIV-W47660                                      
218200          ELSE                                                            
218300**** NON VCC RADER SKALL INTE SKAPA EN SUMMARAD I SEPV BÖCKER             
218400             MOVE IN-IDDC TO WS-IDDC                                      
218500             IF XDC-NON-VCC-OWNED                                         
218600             OR NDC-US                                                    
218700               IF IN-KDFAKSTA-EXP = 1                                     
218800                 IF NDC-US                                                
218900                   MOVE 'USD'       TO UT5-EKHT-KDVALISO                  
219000                   MOVE 'US01'      TO UT5-EKHT-KDTRADP                   
219100                   MOVE 'W561EKHA'  TO UT5-EKHT-IDCPYTXT                  
219200                   PERFORM S21-SKRIV-W4768G                               
219300                 ELSE                                                     
219400                   IF NDC-IN                                              
219500                     MOVE 'INR'       TO UT5-EKHT-KDVALISO                
219600                     MOVE 'IN07'      TO UT5-EKHT-KDTRADP                 
219700                     MOVE 'W515EKHA'  TO UT5-EKHT-IDCPYTXT                
219800                     PERFORM S17-SKRIV-W4768F                             
219900                   ELSE                                                   
220000                     IF NDC-CN                                            
220100                       MOVE 'W570'  TO UT5-EKHT-IDCPYTXT(1:4)             
220200                       MOVE 'CN05'  TO UT5-EKHT-KDTRADP                   
220300                       MOVE 'CNY'   TO UT5-EKHT-KDVALISO                  
220400                     ELSE                                                 
220500                       MOVE BET-KDTRADP  TO UT5-EKHT-IDCPYTXT(1:4)        
220600                                            UT5-EKHT-KDTRADP              
220700                       MOVE BET-KDVALISO TO UT5-EKHT-KDVALISO             
220800                       IF NDC-AE                                          
220900                         MOVE 'AE01'  TO UT5-EKHT-IDCPYTXT(1:4)           
221000                                         UT5-EKHT-KDTRADP                 
221100                       END-IF                                             
221200                       IF NDC-TR                                          
221300                         MOVE 'TR02'  TO UT5-EKHT-IDCPYTXT(1:4)           
221400                                         UT5-EKHT-KDTRADP                 
221500                       END-IF                                             
221600                       IF NDC-KR                                          
221700                         MOVE 'KR02'  TO UT5-EKHT-IDCPYTXT(1:4)           
221800                                         UT5-EKHT-KDTRADP                 
221900                       END-IF                                             
222000                       IF NDC-MY                                          
222100                         MOVE 'MY04'  TO UT5-EKHT-IDCPYTXT(1:4)           
222200                                         UT5-EKHT-KDTRADP                 
222300                       END-IF                                             
222400                       IF NDC-TH                                          
222500                         MOVE 'TH01'  TO UT5-EKHT-IDCPYTXT(1:4)           
222600                                         UT5-EKHT-KDTRADP                 
222700                       END-IF                                             
222800                       IF NDC-TW                                          
222900                         MOVE 'TW01'  TO UT5-EKHT-IDCPYTXT(1:4)           
223000                                         UT5-EKHT-KDTRADP                 
223100                       END-IF                                             
223200                       IF NDC-MX                                          
223300                         MOVE 'MX10'  TO UT5-EKHT-IDCPYTXT(1:4)           
223400                                         UT5-EKHT-KDTRADP                 
223500                       END-IF                                             
223600                       IF NDC-BR                                          
223700                         MOVE 'BR12'  TO UT5-EKHT-IDCPYTXT(1:4)           
223800                                         UT5-EKHT-KDTRADP                 
223900                       END-IF                                             
224000                       IF NDC-ZA                                          
224100                         MOVE 'ZA04'  TO UT5-EKHT-IDCPYTXT(1:4)           
224200                                         UT5-EKHT-KDTRADP                 
224300                       END-IF                                             
224400                     END-IF                                               
224500                     MOVE 'EKHA'         TO UT5-EKHT-IDCPYTXT(5:4)        
224600                     PERFORM S16-SKRIV-W4768E                             
224700                   END-IF                                                 
224800                 END-IF                                                   
224900               ELSE                                                       
225000                 CONTINUE                                                 
225100               END-IF                                                     
225200             ELSE                                                         
225300               MOVE 'SEPV' TO UT5-EKHT-KDTRADP                            
225400               PERFORM S15-SKRIV-W47660                                   
225500             END-IF                                                       
225600          END-IF                                                          
225700       END-IF                                                             
225800     END-IF                                                               
225900                                                                          
226000     MOVE WS-IDDISTR           TO TEST-IDDISTR                            
226100     EVALUATE KORD-KDFAKTYP                                               
226200                                                                          
226300     WHEN 'R'                                                             
226400**** REFILLER TO NON VCC SECOND POST FROM CDC                             
226500         IF DIST35-CDC-NONVCC-REFILL                                      
226600         OR DIST35-VCC-NONVCC-REFILL                                      
226700         OR DIST35-VCC-NONVCC-TRANSFER                                    
226800           MOVE '102'            TO UT5-EKHT-KDEKHHT                      
226900           IF KORD-FLOVRLEV = JA                                          
227000**** ADDITIONAL INVOICE                                                   
227100             MOVE '124'          TO UT5-EKHT-KDEKSHT                      
227200             MOVE ORAD-BEVOLREF  TO UT5-EKHT-IDFAKT-EXP                   
227300           ELSE                                                           
227400**** GIT POST                                                             
227500             MOVE '120'          TO UT5-EKHT-KDEKSHT                      
227600           END-IF                                                         
227700           MOVE WC-CDC-SE        TO UT5-EKHT-IDDC-SEND                    
227800           MOVE '1441'           TO UT5-EKHT-IDLEVNR                      
227900           IF DIST35-JP-NONVCC-REFILL                                     
228000             MOVE WC-NDC-JP-61   TO UT5-EKHT-IDDC-SEND                    
228100           END-IF                                                         
228200           IF DIST35-VCC-NONVCC-TRANSFER                                  
228300             MOVE IN-IDDC TO UT5-EKHT-IDDC-SEND                           
228400           END-IF                                                         
228500**** IF WE START SENDING FROM AU                                          
228600*          IF DIST35-AU-NONVCC-REFILL                                     
228700*            MOVE WC-NDC-AU-62   TO UT5-EKHT-IDDC-SEND                    
228800*          END-IF                                                         
228900         END-IF                                                           
229000**** CREATING THE POST                                                    
229100         IF DIST35-CDC-NONVCC-REFILL                                      
229200         OR DIST35-VCC-NONVCC-REFILL                                      
229300         OR DIST35-VCC-NONVCC-TRANSFER                                    
229400           IF DIST35-CDC-IN-REFILL                                        
229500           OR DIST35-JP-NDC67-REFILL                                      
229600             PERFORM JAC-SKAPA-SUM-POST5                                  
229700           ELSE                                                           
229800**** CDC TO US FLOW IS IN LAB.WHEN COMING TO THIS, ADD JAD-SKAPA          
229900             PERFORM JAA-SKAPA-SUM-POST5                                  
230000           END-IF                                                         
230100         END-IF                                                           
230200**** END-REFILL OTHER MARKETS                                             
230300                                                                          
230400**** HERE WE SEND THE SECOND POST FOR EXPORT                              
230500         IF DIST35-NONVCC-REFILL                                          
230600         OR DIST35-NONVCC-VCC-TRANSFER                                    
230700         OR DIST35-NONVCC-NONVCC-TRANSFER                                 
230800           MOVE '102'            TO UT5-EKHT-KDEKHHT                      
230900           MOVE DCS-IDDC         TO UT5-EKHT-IDDC-SEND                    
231000           MOVE DCS-IDLEVNR-DC   TO UT5-EKHT-IDLEVNR                      
231100           IF DIST35-NONVCC-CDC-REFILL                                    
231200           OR DIST35-NONVCC-VCC-REFILL                                    
231300           OR DIST35-NONVCC-VCC-TRANSFER                                  
231400**** SINGLE FLOW TO CDC GIT POST                                          
231500             MOVE '120'          TO UT5-EKHT-KDEKSHT                      
231600           END-IF                                                         
231700           IF (DIST35-NONVCC-NONVCC-REFILL AND NOT DCS-CDC)               
231800           OR (DIST35-NONVCC-NONVCC-TRANSFER AND NOT DCS-CDC)             
231900*          AND (DCS-USA OR DCS-CHINA))                                    
232000**** BOUNCE FLOW FIRST INVOICE                                            
232100                                                                          
232200             IF IN-IDPURAD = ZERO AND IN-IDKOLLI = ZERO                   
232300               MOVE SPACE        TO UT5-EKHT-IDFAKT-EXP                   
232400               CONTINUE                                                   
232500             ELSE                                                         
232600               IF IN-IDPRODNR NOT = VORD-IDPRODNR                         
232700               OR IN-IDKOLLI NOT  = KOLLI-IDKOLLI                         
232800                 MOVE IN-IDPRODNR TO W-IDPRODNR-E6                        
232900                 PERFORM IMS-GU-WDE601                                    
233000                 MOVE IN-IDKOLLI TO W-IDKOLLI-E6                          
233100                 PERFORM IMS-GNP-WDE611                                   
233200               END-IF                                                     
233300                                                                          
233400               MOVE KOLLI-IDFAKT-EXP TO UT5-EKHT-IDFAKT-EXP               
233500             END-IF                                                       
233600                                                                          
233700             MOVE '135'          TO UT5-EKHT-KDEKSHT                      
233800             MOVE WC-CDC-SE      TO UT5-EKHT-IDDC-REC                     
233900           END-IF                                                         
234000           IF (DIST35-NONVCC-NONVCC-REFILL AND DCS-CDC)                   
234100           OR (DIST35-NONVCC-NONVCC-TRANSFER AND DCS-CDC)                 
234200**** BOUNCE FLOW SECOND INVOICE GIT POST                                  
234300             MOVE '130'          TO UT5-EKHT-KDEKSHT                      
234400             MOVE IN-IDDC        TO UT5-EKHT-IDDC-SEND                    
234500*            MOVE IN-IDPARTNR    TO UT5-EKHT-IDLEVNR                      
234600             MOVE '1441'         TO UT5-EKHT-IDLEVNR                      
234700             IF KORD-FLOVRLEV = JA                                        
234800               MOVE '134'        TO UT5-EKHT-KDEKSHT                      
234900               MOVE ORAD-BEVOLREF TO UT5-EKHT-IDFAKT-EXP                  
235000             END-IF                                                       
235100           END-IF                                                         
235200**** CREATING THE POST                                                    
235300           IF DIST35-NONVCC-CDC-REFILL                                    
235400           OR DIST35-NONVCC-VCC-REFILL                                    
235500           OR DIST35-NONVCC-VCC-TRANSFER                                  
235600           OR (DIST35-NONVCC-NONVCC-REFILL AND NOT DCS-CDC)               
235700           OR (DIST35-NONVCC-NONVCC-TRANSFER AND NOT DCS-CDC)             
235800**** FOR SINGLE FLOW TO CDC OR BOUNCE FLOW FIRST INVOICE                  
235900             PERFORM JAB-SKAPA-SUM-POST5                                  
236000           END-IF                                                         
236100           IF (DIST35-NONVCC-NONVCC-REFILL AND DCS-CDC)                   
236200           OR (DIST35-NONVCC-NONVCC-TRANSFER AND DCS-CDC)                 
236300             IF DIST35-NDCCN-NDCUS-REFILL                                 
236400**** HERE WE NEED TO ADD THE TRANSFERS TO US                              
236500               PERFORM JAD-SKAPA-SUM-POST5                                
236600             ELSE                                                         
236700               PERFORM JAA-SKAPA-SUM-POST5                                
236800             END-IF                                                       
236900           END-IF                                                         
237000         END-IF                                                           
237100                                                                          
237200         IF DIST35-REFILL                                                 
237300         OR DIST35-NONVCC-REFILL                                          
237400         OR DIST35-VCC-NONVCC-TRANSFER                                    
237500         OR DIST35-NONVCC-VCC-TRANSFER                                    
237600         OR DIST35-NONVCC-NONVCC-TRANSFER                                 
237700           CONTINUE                                                       
237800         ELSE                                                             
237900           IF WS-WDB2-IDFTG = WS-WDB6-IDFTG                               
238000             CONTINUE                                                     
238100           ELSE                                                           
238200             MOVE WS-WDB6-IDFTG TO WS-IDFTG                               
238300**** SENDING DC IS DC11                                                   
238400             IF IDFTG-PV                                                  
238500               MOVE WS-WDB2-IDFTG TO WS-IDFTG                             
238600               IF IDFTG-NON-VCC                                           
238700                 IF IN-KDFAKSTA-EXP = 1                                   
238800****   FOR VOR BOUNCE FLOW FIRST INVOICE                                  
238900                   MOVE '102'    TO UT5-EKHT-KDEKHHT                      
239000                   MOVE '125'    TO UT5-EKHT-KDEKSHT                      
239100                   MOVE '1441'   TO UT5-EKHT-IDLEVNR                      
239200                                                                          
239300                   IF IN-IDPURAD = ZERO AND IN-IDKOLLI = ZERO             
239400                     MOVE SPACE  TO UT5-EKHT-IDFAKT-EXP                   
239500                     CONTINUE                                             
239600                   ELSE                                                   
239700                     IF IN-IDPRODNR NOT = VORD-IDPRODNR                   
239800                     OR IN-IDKOLLI NOT = KOLLI-IDKOLLI                    
239900                       MOVE IN-IDPRODNR TO W-IDPRODNR-E6                  
240000                       PERFORM IMS-GU-WDE601                              
240100                       MOVE IN-IDKOLLI TO W-IDKOLLI-E6                    
240200                       PERFORM IMS-GNP-WDE611                             
240300                     END-IF                                               
240400                     MOVE KOLLI-IDFAKT   TO UT5-EKHT-IDFAKT-EXP           
240500                   END-IF                                                 
240600                                                                          
240700                   IF IDFTG-NON-VCC                                       
240800                     IF IDFTG-IN                                          
240900                       PERFORM JAC-SKAPA-SUM-POST5                        
241000                     ELSE                                                 
241100                       PERFORM JAA-SKAPA-SUM-POST5                        
241200                     END-IF                                               
241300                   END-IF                                                 
241400                 END-IF                                                   
241500               END-IF                                                     
241600             ELSE                                                         
241700**** FOR GLOBAL EXPORT IMPORTER BOUNCE FLOW FIRST INVOICE                 
241800               MOVE WS-WDB6-IDFTG   TO WS-IDFTG                           
241900               IF IDFTG-NON-VCC OR                                        
242000                  IDFTG-US                                                
242100                 MOVE WS-WDB2-IDFTG TO WS-IDFTG                           
242200                 IF IDFTG-PV                                              
242300                   IF IN-KDFAKSTA-EXP = 1                                 
242400                     MOVE '102'  TO UT5-EKHT-KDEKHHT                      
242500                     MOVE '145'  TO UT5-EKHT-KDEKSHT                      
242600                     MOVE '1441'           TO UT5-EKHT-IDLEVNR            
242700                                                                          
242800                     IF IN-IDPURAD = ZERO AND IN-IDKOLLI = ZERO           
242900                       MOVE SPACE TO UT5-EKHT-IDFAKT-EXP                  
243000                     ELSE                                                 
243100                       IF IN-IDPRODNR NOT = VORD-IDPRODNR                 
243200                       OR IN-IDKOLLI NOT = KOLLI-IDKOLLI                  
243300                         MOVE IN-IDPRODNR TO W-IDPRODNR-E6                
243400                         PERFORM IMS-GU-WDE601                            
243500                         MOVE IN-IDKOLLI TO W-IDKOLLI-E6                  
243600                         PERFORM IMS-GNP-WDE611                           
243700                       END-IF                                             
243800                       MOVE KOLLI-IDFAKT TO UT5-EKHT-IDFAKT-EXP           
243900                     END-IF                                               
244000                     MOVE IN-KDVALISO-FAKT  TO UT5-EKHT-KDVALISO          
244100                     PERFORM JAB-SKAPA-SUM-POST5                          
244200                   END-IF                                                 
244300                 END-IF                                                   
244400               END-IF                                                     
244500             END-IF                                                       
244600           END-IF                                                         
244700         END-IF                                                           
244800     END-EVALUATE                                                         
244900     .                                                                    
245000     EJECT                                                                
245100                                                                          
245200 JAA-SKAPA-SUM-POST5 SECTION.                                             
245300     MOVE 'JAA-SKAPA-SUM-POST5' TO WS-SEKTION                             
245400                                                                          
245500     ACCEPT UT5-EKHT-DAREGDAT FROM DATE                                   
245600     ACCEPT UT5-EKHT-TIKLOCK FROM TIME                                    
245700                                                                          
245800     IF IDFTG-CN                                                          
245900     OR BET-KDTRADP = 'CN05'                                              
246000       MOVE 'W570'             TO UT5-EKHT-IDCPYTXT(1:4)                  
246100       MOVE 'CN05'             TO UT5-EKHT-KDTRADP                        
246200       MOVE 'CNY'              TO UT5-EKHT-KDVALISO                       
246300     ELSE                                                                 
246400       IF BET-KDTRADP = 'IN07'                                            
246500         MOVE 'W515'             TO UT5-EKHT-IDCPYTXT(1:4)                
246600         MOVE 'IN07'             TO UT5-EKHT-KDTRADP                      
246700         MOVE 'INR'              TO UT5-EKHT-KDVALISO                     
246800       ELSE                                                               
246900         MOVE BET-KDTRADP        TO UT5-EKHT-IDCPYTXT(1:4)                
247000                                    UT5-EKHT-KDTRADP                      
247100         MOVE BET-KDVALISO       TO UT5-EKHT-KDVALISO                     
247200       END-IF                                                             
247300     END-IF                                                               
247400     MOVE 'EKHA'               TO UT5-EKHT-IDCPYTXT(5:4)                  
247500     MOVE 1                    TO UT5-EKHT-PRKURS                         
247600     MOVE IN-IDKUNDNR          TO UT5-EKHT-IDKUNDNR                       
247700     MOVE SPACE                TO UT5-EKHT-FLDCET                         
247800     MOVE SPACE                TO UT5-EKHT-IDKUNDRF                       
247900******** HAVE TO ADD FIRST INVOICE NO TO SECOND INVOICE                   
248000                                                                          
248100     IF BET-KDTRADP = 'IN07'                                              
248200       PERFORM S17-SKRIV-W4768F                                           
248300     ELSE                                                                 
248400       PERFORM S16-SKRIV-W4768E                                           
248500     END-IF                                                               
248600     .                                                                    
248700     EJECT                                                                
248800                                                                          
248900 JAB-SKAPA-SUM-POST5 SECTION.                                             
249000     MOVE 'JAB-SKAPA-SUM-POST5' TO WS-SEKTION                             
249100                                                                          
249200     MOVE FUNCTION CURRENT-DATE (1:8) TO UT5-EKHT-DAREGDAT                
249300     ACCEPT UT5-EKHT-TIKLOCK FROM TIME                                    
249400                                                                          
249500     MOVE 'SEPV'               TO UT5-EKHT-KDTRADP                        
249600     MOVE 'W510EKHA'           TO UT5-EKHT-IDCPYTXT                       
249700     MOVE IN-IDKUNDNR          TO UT5-EKHT-IDKUNDNR                       
249800     MOVE SPACE                TO UT5-EKHT-FLDCET                         
249900     MOVE SPACE                TO UT5-EKHT-IDKUNDRF                       
250000                                                                          
250100     PERFORM S15-SKRIV-W47660                                             
250200     .                                                                    
250300     EJECT                                                                
250400                                                                          
250500 JAC-SKAPA-SUM-POST5 SECTION.                                             
250600     MOVE 'JAC-SKAPA-SUM-POST5' TO WS-SEKTION                             
250700                                                                          
250800     ACCEPT UT5-EKHT-DAREGDAT FROM DATE                                   
250900     ACCEPT UT5-EKHT-TIKLOCK FROM TIME                                    
251000                                                                          
251100     MOVE 'INR'                TO UT5-EKHT-KDVALISO                       
251200     MOVE 1                    TO UT5-EKHT-PRKURS                         
251300     MOVE 'IN07'               TO UT5-EKHT-KDTRADP                        
251400     MOVE IN-IDKUNDNR          TO UT5-EKHT-IDKUNDNR                       
251500     MOVE SPACE                TO UT5-EKHT-FLDCET                         
251600     MOVE SPACE                TO UT5-EKHT-IDKUNDRF                       
251700     MOVE 'W515EKHA'           TO UT5-EKHT-IDCPYTXT                       
251800                                                                          
251900     PERFORM S17-SKRIV-W4768F                                             
252000                                                                          
252100     IF KORD-FLOVRLEV = JA                                                
252200       CONTINUE                                                           
252300     ELSE                                                                 
252400**** EN FIKTIV SUMMAPOST FÖR INDIEN                                       
252500       MOVE 'W515EKHB'           TO UT5-EKHT-IDCPYTXT                     
252600       PERFORM S17-SKRIV-W4768F                                           
252700     END-IF                                                               
252800     .                                                                    
252900     EJECT                                                                
253000                                                                          
253100 JAD-SKAPA-SUM-POST5 SECTION.                                             
253200     MOVE 'JAD-SKAPA-SUM-POST5' TO WS-SEKTION                             
253300                                                                          
253400     ACCEPT UT5-EKHT-DAREGDAT FROM DATE                                   
253500     ACCEPT UT5-EKHT-TIKLOCK FROM TIME                                    
253600                                                                          
253700     MOVE 'USD'                TO UT5-EKHT-KDVALISO                       
253800     MOVE 1                    TO UT5-EKHT-PRKURS                         
253900     MOVE 'US01'               TO UT5-EKHT-KDTRADP                        
254000     MOVE 'W561EKHA'           TO UT5-EKHT-IDCPYTXT                       
254100     MOVE IN-IDKUNDNR          TO UT5-EKHT-IDKUNDNR                       
254200     MOVE SPACE                TO UT5-EKHT-FLDCET                         
254300     MOVE SPACE                TO UT5-EKHT-IDKUNDRF                       
254400     MOVE SPACE                TO UT5-EKHT-IDFAKT-EXP                     
254500******** HAVE TO ADD FIRST INVOICE NO TO SECOND INVOICE                   
254600                                                                          
254700     PERFORM S21-SKRIV-W4768G                                             
254800     .                                                                    
254900     EJECT                                                                
255000                                                                          
255100 JB-F01-BUILD-WRITE-VAT SECTION.                                          
255200     MOVE 'JB-'  TO WS-SEKTION                                            
255300     MOVE 'MOMS'               TO UT5-EKHT-KDEKNIVA                       
255400     MOVE IN-SUVAT-FAKT        TO UT5-EKHT-SUBEL                          
255500     IF DIST79-DEALER-PRICE                                               
255600       PERFORM S02-HAMTA-VALUTAKURS                                       
255700       MOVE WS-PRKURS          TO UT5-EKHT-PRKURS                         
255800     ELSE                                                                 
255900      IF KORD-SUORDV NOT = ZERO OR                                        
256000        (KORD-SUORDV-LEVPL NOT = ZERO AND ORAD-FLDIRLEV = JA)             
256100        IF BET-KDVALISO NOT = IN-KDVALISO-FAKT                            
256200          MOVE BET-KDVALISO    TO UT5-EKHT-KDVALISO                       
256300          IF BET-KDVALISO NOT = WS-KDVALISO                               
256400            PERFORM S02-BET-HAMTA-VALUTAKURS                              
256500          END-IF                                                          
256600          MOVE WS-PRKURS       TO UT5-EKHT-PRKURS                         
256700          IF UT5-EKHT-KDVALISO = SPACE OR 'SEK'                           
256800             MOVE 'SEK'        TO UT5-EKHT-KDVALISO                       
256900             MOVE 1.0          TO UT5-EKHT-PRKURS                         
257000          END-IF                                                          
257100        ELSE                                                              
257200          MOVE 'SEK'           TO W-KDVALISO                              
257300                                  WS-KDVALISO                             
257400                                  ORAD-KDVALISO                           
257500          MOVE 1.0             TO WS-PRKURS                               
257600                                  UT5-EKHT-PRKURS                         
257700        END-IF                                                            
257800        MOVE IN-KDVALISO-FAKT  TO UT5-EKHT-KDVALISO                       
257900        MOVE IN-PRKURS-FAKT    TO UT5-EKHT-PRKURS                         
258000      ELSE                                                                
258100        PERFORM S02-HAMTA-VALUTAKURS                                      
258200        MOVE WS-PRKURS         TO UT5-EKHT-PRKURS                         
258300      END-IF                                                              
258400     END-IF                                                               
258500     MOVE ORAD-KDVAT           TO UT5-EKHT-BEVAT                          
258600     MOVE SPACE                TO UT5-EKHT-FLDCET                         
258700     MOVE SPACE                TO UT5-EKHT-IDKUNDRF                       
258800     MOVE SPACE                TO UT5-EKHT-IDFAKT-EXP                     
258900                                                                          
259000     MOVE IN-IDDC TO WS-IDDC                                              
259100     IF XDC-NON-VCC-OWNED OR NDC-US                                       
259200       MOVE ORAD-PRAVCOST        TO IN-PRAVCOST                           
259300**** AVCOST SHOULD BE SAVED IN WDE411 IN PROGRAM 4637                     
259400       IF IN-PRAVCOST = ZERO                                              
259500         PERFORM S40-HAMTA-PRAVCOST                                       
259600       END-IF                                                             
259700       MOVE IN-PRAVCOST        TO UT5-EKHT-PRARTSTD                       
259800       MOVE BET-KDTRADP        TO UT5-EKHT-KDTRADP                        
259900       MOVE BET-KDVALISO       TO UT5-EKHT-KDVALISO                       
260000       EVALUATE TRUE                                                      
260100         WHEN NDC-CN OR LDC-CN                                            
260200           MOVE 'W570EKHA'      TO UT5-EKHT-IDCPYTXT                      
260300           PERFORM S16-SKRIV-W4768E                                       
260400         WHEN NDC-IN                                                      
260500           MOVE 'W515EKHA'      TO UT5-EKHT-IDCPYTXT                      
260600           PERFORM S17-SKRIV-W4768F                                       
260700         WHEN NDC-US                                                      
260800           MOVE 'W561EKHA'      TO UT5-EKHT-IDCPYTXT                      
260900           PERFORM S21-SKRIV-W4768G                                       
261000         WHEN OTHER                                                       
261100           MOVE BET-KDTRADP    TO UT5-EKHT-IDCPYTXT(1:4)                  
261200           MOVE 'EKHA'         TO UT5-EKHT-IDCPYTXT(5:4)                  
261300           PERFORM S16-SKRIV-W4768E                                       
261400       END-EVALUATE                                                       
261500     ELSE                                                                 
261600       IF IN-IDPURAD = ZERO AND IN-IDKOLLI = ZERO                         
261700         MOVE SPACE                TO UT5-EKHT-IDFAKT-EXP                 
261800         CONTINUE                                                         
261900       ELSE                                                               
262000         IF VORD-IDPRODNR NOT = IN-IDPRODNR OR                            
262100            KOLLI-IDKOLLI NOT = IN-IDKOLLI                                
262200           MOVE IN-IDPRODNR        TO W-IDPRODNR-E6                       
262300           PERFORM IMS-GU-WDE601                                          
262400           MOVE IN-IDKOLLI         TO W-IDKOLLI-E6                        
262500           PERFORM IMS-GNP-WDE611                                         
262600         END-IF                                                           
262700                                                                          
262800         MOVE KOLLI-IDFAKT-EXP     TO UT5-EKHT-IDFAKT-EXP                 
262900       END-IF                                                             
263000                                                                          
263100       MOVE 'SEPV' TO UT5-EKHT-KDTRADP                                    
263200       PERFORM S15-SKRIV-W47660                                           
263300     END-IF                                                               
263400     .                                                                    
263500     EJECT                                                                
263600                                                                          
263700 K-SKAPA-RAD-POST5  SECTION.                                              
263800     MOVE 'K-SKAPA-RAD-POST5'   TO WS-SEKTION                             
263900                                                                          
264000     MOVE ORAD-IDARTNR         TO W-IDARTNR                               
264100     PERFORM IMS-GU-WDK601                                                
264200     PERFORM IMS-GNP-WDK611                                               
264300                                                                          
264400**** HÄR SKAPAS RADERNA FÖR EKONOMISKA BOKNINGARNA FÖRSTA POSTEN          
264500     PERFORM S05-BUILD-COMMON                                             
264600     IF DIST35-NONVCC-VCC-TRANSFER                                        
264700     OR DIST35-VCC-NONVCC-TRANSFER                                        
264800       PERFORM S03-HAMTA-KUNDC                                            
264900     ELSE                                                                 
265000       IF IN-IDKUNDNR NOT = 0                                             
265100         PERFORM S03-HAMTA-KUND                                           
265200       ELSE                                                               
265300         MOVE SPACE             TO BET-KDTRADP                            
265400         PERFORM S03-HAMTA-KUNDB                                          
265500       END-IF                                                             
265600     END-IF                                                               
265700     MOVE WS-WDB2-IDFTG        TO WS-IDFTG                                
265800     MOVE SPACE                TO WS-KDVALISO                             
265900     IF IN-IDKOLLI = ZERO AND IN-IDPURAD = ZERO                           
266000       EVALUATE IN-BEART                                                  
266100         WHEN 'INSURANCE '                                                
266200           MOVE 'FÖRS'         TO UT5-EKHT-KDEKNIVA                       
266300           MOVE IN-SUNTO-LINE  TO UT5-EKHT-SUBEL                          
266400         WHEN 'FREIGHT   '                                                
266500           MOVE 'FRAKT'        TO UT5-EKHT-KDEKNIVA                       
266600           MOVE KORD-KDFRAKT   TO UT5-EKHT-KDFRAKT                        
266700           MOVE IN-SUNTO-LINE  TO UT5-EKHT-SUBEL                          
266800         WHEN 'PACKING & HANDLING'                                        
266900           MOVE 'EMB'          TO UT5-EKHT-KDEKNIVA                       
267000           MOVE IN-SUNTO-LINE  TO UT5-EKHT-SUBEL                          
267100         WHEN 'SERVICE FEE'                                               
267200           MOVE 'EMB'          TO UT5-EKHT-KDEKNIVA                       
267300           MOVE IN-SUNTO-LINE  TO UT5-EKHT-SUBEL                          
267400         WHEN 'LEGAL     '                                                
267500           MOVE 'LEG'          TO UT5-EKHT-KDEKNIVA                       
267600           MOVE IN-SUNTO-LINE  TO UT5-EKHT-SUBEL                          
267700         WHEN 'REDUCTION '                                                
267800           MOVE 'AVDR'         TO UT5-EKHT-KDEKNIVA                       
267900           MOVE IN-SUNTO-LINE  TO UT5-EKHT-SUBEL                          
268000        END-EVALUATE                                                      
268100       MOVE IN-IDDISTR           TO TEST-IDDISTR                          
268200       IF DIST79-DEALER-PRICE                                             
268300         PERFORM S02-HAMTA-VALUTAKURS                                     
268400         MOVE WS-PRKURS          TO UT5-EKHT-PRKURS                       
268500       ELSE                                                               
268600         IF KORD-SUORDV NOT = ZERO OR                                     
268700         (KORD-SUORDV-LEVPL NOT = ZERO AND ORAD-FLDIRLEV = JA)            
268800           IF BET-KDVALISO NOT = IN-KDVALISO-FAKT                         
268900            MOVE BET-KDVALISO    TO UT5-EKHT-KDVALISO                     
269000            IF BET-KDVALISO NOT = WS-KDVALISO                             
269100              PERFORM S02-BET-HAMTA-VALUTAKURS                            
269200            END-IF                                                        
269300            MOVE WS-PRKURS       TO UT5-EKHT-PRKURS                       
269400            IF UT5-EKHT-KDVALISO = SPACE OR 'SEK'                         
269500               MOVE 'SEK'        TO UT5-EKHT-KDVALISO                     
269600               MOVE 1.0          TO UT5-EKHT-PRKURS                       
269700            END-IF                                                        
269800           ELSE                                                           
269900            MOVE 'SEK'           TO UT5-EKHT-KDVALISO                     
270000            MOVE 1.0             TO UT5-EKHT-PRKURS                       
270100           END-IF                                                         
270200           MOVE IN-KDVALISO-FAKT TO UT5-EKHT-KDVALISO                     
270300           MOVE IN-PRKURS-FAKT   TO UT5-EKHT-PRKURS                       
270400         ELSE                                                             
270500           MOVE 'SEK'            TO UT5-EKHT-KDVALISO                     
270600           MOVE 1.0              TO UT5-EKHT-PRKURS                       
270700         END-IF                                                           
270800       END-IF                                                             
270900       MOVE BET-KDTRADP          TO UT5-EKHT-KDTRADP                      
271000       MOVE IN-IDKUNDNR          TO UT5-EKHT-IDKUNDNR                     
271100       MOVE ZERO                 TO UT5-EKHT-IDORDNR5                     
271200       MOVE IN-IDDC              TO UT5-EKHT-IDDC-SEND                    
271300     ELSE                                                                 
271400       MOVE 'DET'                TO UT5-EKHT-KDEKNIVA                     
271500       MOVE IN-IDDC              TO UT5-EKHT-IDDC-SEND                    
271600       MOVE IN-IDKUNDNR          TO UT5-EKHT-IDKUNDNR                     
271700       MOVE ORAD-IDARTNR         TO UT5-EKHT-IDARTNR                      
271800       IF KORD-FLLSBOK = 'J'                                              
271900         MOVE 'Y'                TO UT5-EKHT-FLLSBOK                      
272000       ELSE                                                               
272100         MOVE KORD-FLLSBOK       TO UT5-EKHT-FLLSBOK                      
272200       END-IF                                                             
272300       MOVE ORAD-KDPRODSL        TO UT5-EKHT-KDPRODSL                     
272400       MOVE ORAD-KDFRAKT         TO UT5-EKHT-KDFRAKT                      
272500       MOVE ORAD-IDKONTO         TO UT5-EKHT-IDKONTO                      
272600       IF WS-IDKST NOT = SPACES                                           
272700         MOVE WS-IDKST           TO UT5-EKHT-IDKST                        
272800       ELSE                                                               
272900         MOVE ORAD-IDKST         TO UT5-EKHT-IDKST                        
273000       END-IF                                                             
273100       MOVE ORAD-IDANALYS        TO UT5-EKHT-IDANALYS                     
273200       MOVE ORAD-PRARTNTO        TO UT5-EKHT-PRARTNTO                     
273300       MOVE IN-IDDISTR           TO TEST-IDDISTR                          
273400       IF DIST79-DEALER-PRICE OR                                          
273500          DIST79-ECOM-PRICE                                               
273600         MOVE ORAD-PRARTNTO-LOC  TO UT5-EKHT-PRARTNTO                     
273700         IF ORAD-PRARTNTO-LOC = ZERO                                      
273800           PERFORM XX-UT5                                                 
273900           MOVE XX-PRARTNTO        TO UT5-EKHT-PRARTNTO                   
274000         END-IF                                                           
274100         PERFORM S02-HAMTA-VALUTAKURS                                     
274200         MOVE WS-PRKURS          TO UT5-EKHT-PRKURS                       
274300       ELSE                                                               
274400         IF KORD-SUORDV NOT = ZERO OR                                     
274500         (KORD-SUORDV-LEVPL NOT = ZERO AND ORAD-FLDIRLEV = JA)            
274600           IF BET-KDVALISO NOT = IN-KDVALISO-FAKT                         
274700            MOVE BET-KDVALISO    TO UT5-EKHT-KDVALISO                     
274800            IF BET-KDVALISO NOT = WS-KDVALISO                             
274900              PERFORM S02-BET-HAMTA-VALUTAKURS                            
275000            END-IF                                                        
275100            MOVE WS-PRKURS       TO UT5-EKHT-PRKURS                       
275200            IF UT5-EKHT-KDVALISO = SPACE OR 'SEK'                         
275300               MOVE 'SEK'        TO UT5-EKHT-KDVALISO                     
275400               MOVE 1.0          TO UT5-EKHT-PRKURS                       
275500            END-IF                                                        
275600           ELSE                                                           
275700            MOVE 'SEK'           TO W-KDVALISO                            
275800                                    WS-KDVALISO                           
275900                                    ORAD-KDVALISO                         
276000                                    UT5-EKHT-KDVALISO                     
276100            MOVE 1.0             TO WS-PRKURS                             
276200                                    UT5-EKHT-PRKURS                       
276300           END-IF                                                         
276400           MOVE IN-KDVALISO-FAKT TO UT5-EKHT-KDVALISO                     
276500           MOVE IN-PRKURS-FAKT   TO UT5-EKHT-PRKURS                       
276600         ELSE                                                             
276700           MOVE 'SEK'            TO UT5-EKHT-KDVALISO                     
276800           MOVE 1.0              TO UT5-EKHT-PRKURS                       
276900         END-IF                                                           
277000       END-IF                                                             
277100       IF IN-IDKOLLI NOT = ZERO                                           
277200         MOVE KKOLLI-KVLEVART    TO UT5-EKHT-KVANTAL                      
277300       ELSE                                                               
277400         MOVE +1                 TO UT5-EKHT-KVANTAL                      
277500       END-IF                                                             
277600       IF KORD-KDFAKTYP = 'N'                                             
277700         MOVE IN-IDORDNR7        TO W-FILL7                               
277800         MOVE W-FILL7(3:5)       TO UT5-EKHT-IDORDNR5                     
277900       ELSE                                                               
278000         MOVE ZERO               TO UT5-EKHT-IDORDNR5                     
278100       END-IF                                                             
278200       MOVE BET-KDTRADP          TO UT5-EKHT-KDTRADP                      
278300       MOVE ORAD-KDVAT           TO UT5-EKHT-BEVAT                        
278400     END-IF                                                               
278500     MOVE SPACE                TO UT5-EKHT-FLDCET                         
278600     MOVE SPACE                TO UT5-EKHT-IDKUNDRF                       
278700     MOVE SPACE                TO UT5-EKHT-IDFAKT-EXP                     
278800                                                                          
278900     MOVE IN-IDDISTR           TO TEST-IDDISTR                            
279000     MOVE IN-IDDC TO WS-IDDC                                              
279100                                                                          
279200     IF DIST35-NONVCC-CDC-REFILL                                          
279300     OR DIST35-NONVCC-VCC-REFILL                                          
279400     OR DIST35-NONVCC-VCC-TRANSFER                                        
279500     OR (DIST35-NONVCC-NONVCC-REFILL AND NOT DCS-CDC)                     
279600     OR (DIST35-NONVCC-NONVCC-TRANSFER AND NOT DCS-CDC)                   
279700       MOVE ORAD-PRAVCOST        TO IN-PRAVCOST                           
279800**** AVCOST SHOULD BE SAVED IN WDE411 IN PROGRAM 4637                     
279900       IF IN-PRAVCOST = ZERO                                              
280000         PERFORM S40-HAMTA-PRAVCOST                                       
280100       END-IF                                                             
280200       MOVE IN-PRAVCOST     TO UT5-EKHT-PRARTSTD                          
280300       IF CLAG-IDPROJ = 'OBJ'                                             
280400         MOVE 'OBJ'         TO UT5-EKHT-IDKUNDRF                          
280500       END-IF                                                             
280600       IF DIST35-NDCUS-CDC-REFILL                                         
280700       OR DIST35-NDCUS-JP-REFILL                                          
280800       OR DIST35-NDCUS-AU-REFILL                                          
280900       OR DCS-USA                                                         
281000         MOVE 'USD'         TO UT5-EKHT-KDVALISO                          
281100         MOVE 'US01'        TO UT5-EKHT-KDTRADP                           
281200         MOVE 'W561EKHA'    TO UT5-EKHT-IDCPYTXT                          
281300         PERFORM S21-SKRIV-W4768G                                         
281400       ELSE                                                               
281500         IF DIST35-NDCCN-CDC-REFILL                                       
281600         OR DIST35-NDCCN-JP-REFILL                                        
281700         OR DIST35-NDCCN-AU-REFILL                                        
281800         OR DCS-CHINA                                                     
281900           MOVE 'CNY'         TO UT5-EKHT-KDVALISO                        
282000           MOVE 'CN05'        TO UT5-EKHT-KDTRADP                         
282100           MOVE 'W570EKHA'    TO UT5-EKHT-IDCPYTXT                        
282200           PERFORM S16-SKRIV-W4768E                                       
282300         ELSE                                                             
282400           IF DCS-INDIA                                                   
282500             MOVE 'INR'       TO UT5-EKHT-KDVALISO                        
282600             MOVE 'IN07'      TO UT5-EKHT-KDTRADP                         
282700             MOVE 'W515EKHA'  TO UT5-EKHT-IDCPYTXT                        
282800             PERFORM S17-SKRIV-W4768F                                     
282900           ELSE                                                           
283000             MOVE DCS-KDTRADP TO UT5-EKHT-IDCPYTXT(1:4)                   
283100             MOVE 'EKHA'      TO UT5-EKHT-IDCPYTXT(5:4)                   
283200             MOVE DCS-KDVALISO TO UT5-EKHT-KDVALISO                       
283300             MOVE DCS-KDTRADP TO UT5-EKHT-KDTRADP                         
283400             PERFORM S16-SKRIV-W4768E                                     
283500           END-IF                                                         
283600         END-IF                                                           
283700       END-IF                                                             
283800     ELSE                                                                 
283900       IF XDC-NON-VCC-OWNED                                               
284000       OR NDC-US                                                          
284100         IF  NOT DIST35-CDC-NONVCC-REFILL                                 
284200         AND NOT DIST35-VCC-NONVCC-REFILL                                 
284300         AND NOT DIST35-VCC-NONVCC-TRANSFER                               
284400         AND NOT DIST35-CDC-RETURNS-NON-VCC                               
284500**** DONE THIS FOR SECOND INVOCE FOR VOR BOUNCE                           
284600           IF ORAD-KDVALISO-EXP > ' '                                     
284700             MOVE ORAD-PRAVCOST TO IN-PRAVCOST                            
284800             IF IN-PRAVCOST = ZERO                                        
284900               PERFORM S40-HAMTA-PRAVCOST                                 
285000             END-IF                                                       
285100****                                                                      
285200             IF IN-IDPURAD = ZERO AND IN-IDKOLLI = ZERO                   
285300               MOVE SPACE             TO UT5-EKHT-IDFAKT-EXP              
285400               IF VORD-IDPRODNR NOT = IN-IDPRODNR                         
285500                 MOVE IN-IDPRODNR     TO W-IDPRODNR-E6                    
285600                 PERFORM IMS-GU-WDE601                                    
285700               END-IF                                                     
285800             ELSE                                                         
285900               IF VORD-IDPRODNR NOT = IN-IDPRODNR OR                      
286000                  KOLLI-IDKOLLI NOT = IN-IDKOLLI                          
286100                 MOVE IN-IDPRODNR     TO W-IDPRODNR-E6                    
286200                 PERFORM IMS-GU-WDE601                                    
286300                 MOVE IN-IDKOLLI      TO W-IDKOLLI-E6                     
286400                 PERFORM IMS-GNP-WDE611                                   
286500               END-IF                                                     
286600               MOVE KOLLI-IDFAKT      TO UT5-EKHT-IDFAKT-EXP              
286700             END-IF                                                       
286800           ELSE                                                           
286900             MOVE ORAD-PRAVCOST        TO IN-PRAVCOST                     
287000**** AVCOST SHOULD BE SAVED IN WDE411 IN PROGRAM 4637                     
287100             IF IN-PRAVCOST = ZERO                                        
287200               PERFORM S40-HAMTA-PRAVCOST                                 
287300             END-IF                                                       
287400           END-IF                                                         
287500                                                                          
287600           MOVE IN-PRAVCOST TO UT5-EKHT-PRARTSTD                          
287700           IF CLAG-IDPROJ = 'OBJ'                                         
287800             MOVE 'OBJ'       TO UT5-EKHT-IDKUNDRF                        
287900           END-IF                                                         
288000           MOVE BET-KDVALISO   TO UT5-EKHT-KDVALISO                       
288100           MOVE BET-KDTRADP    TO UT5-EKHT-KDTRADP                        
288200**** ONLY VOR AND IMPORTÖR STUDS                                          
288300           IF  VORD-IDDC-EXP > SPACE                                      
288400**** VOR STUDS                                                            
288500             IF VORD-IDDC-EXP NOT = WC-CDC-SE                             
288600               IF NDC-CN OR LDC-CN                                        
288700                 MOVE 'W570EKHA'   TO UT5-EKHT-IDCPYTXT                   
288800                 PERFORM S16-SKRIV-W4768E                                 
288900               ELSE                                                       
289000                 IF NDC-IN                                                
289100                   MOVE 'W515EKHA'  TO UT5-EKHT-IDCPYTXT                  
289200                   PERFORM S17-SKRIV-W4768F                               
289300                 ELSE                                                     
289400                   MOVE BET-KDTRADP TO UT5-EKHT-IDCPYTXT(1:4)             
289500                   MOVE 'EKHA'     TO UT5-EKHT-IDCPYTXT(5:4)              
289600                   PERFORM S16-SKRIV-W4768E                               
289700                 END-IF                                                   
289800               END-IF                                                     
289900             END-IF                                                       
290000**** IMPORTÖR STUDS                                                       
290100             IF VORD-IDDC-EXP = WC-CDC-SE                                 
290200               IF NDC-CN OR LDC-CN                                        
290300                 MOVE 'CNY'       TO UT5-EKHT-KDVALISO                    
290400                 MOVE 'CN05'      TO UT5-EKHT-KDTRADP                     
290500                 MOVE 'W570EKHA'   TO UT5-EKHT-IDCPYTXT                   
290600                 PERFORM S16-SKRIV-W4768E                                 
290700               END-IF                                                     
290800               IF NDC-IN                                                  
290900                 MOVE 'INR'       TO UT5-EKHT-KDVALISO                    
291000                 MOVE 'IN07'      TO UT5-EKHT-KDTRADP                     
291100                 MOVE 'W515EKHA'  TO UT5-EKHT-IDCPYTXT                    
291200                 PERFORM S17-SKRIV-W4768F                                 
291300               END-IF                                                     
291400               IF NDC-US                                                  
291500                 MOVE 'USD'       TO UT5-EKHT-KDVALISO                    
291600                 MOVE 'US01'      TO UT5-EKHT-KDTRADP                     
291700                 MOVE 'W561EKHA' TO UT5-EKHT-IDCPYTXT                     
291800                 PERFORM S21-SKRIV-W4768G                                 
291900               END-IF                                                     
292000               IF NDC-KR                                                  
292100                 MOVE 'KRW' TO UT5-EKHT-KDVALISO                          
292200                 MOVE 'KR02' TO UT5-EKHT-KDTRADP                          
292300                 MOVE 'KR02' TO UT5-EKHT-IDCPYTXT(1:4)                    
292400                 MOVE 'EKHA'   TO UT5-EKHT-IDCPYTXT(5:4)                  
292500                 PERFORM S16-SKRIV-W4768E                                 
292600               END-IF                                                     
292700               IF NDC-AE                                                  
292800                 MOVE 'USD' TO UT5-EKHT-KDVALISO                          
292900                 MOVE 'AE01' TO UT5-EKHT-KDTRADP                          
293000                 MOVE 'AE01' TO UT5-EKHT-IDCPYTXT(1:4)                    
293100                 MOVE 'EKHA'   TO UT5-EKHT-IDCPYTXT(5:4)                  
293200                 PERFORM S16-SKRIV-W4768E                                 
293300               END-IF                                                     
293400               IF NDC-TR                                                  
293500                 MOVE 'TRY' TO UT5-EKHT-KDVALISO                          
293600                 MOVE 'TR02' TO UT5-EKHT-KDTRADP                          
293700                 MOVE 'TR02' TO UT5-EKHT-IDCPYTXT(1:4)                    
293800                 MOVE 'EKHA'   TO UT5-EKHT-IDCPYTXT(5:4)                  
293900                 PERFORM S16-SKRIV-W4768E                                 
294000               END-IF                                                     
294100               IF NDC-MY                                                  
294200                 MOVE 'MYR' TO UT5-EKHT-KDVALISO                          
294300                 MOVE 'MY04' TO UT5-EKHT-KDTRADP                          
294400                 MOVE 'MY04' TO UT5-EKHT-IDCPYTXT(1:4)                    
294500                 MOVE 'EKHA'   TO UT5-EKHT-IDCPYTXT(5:4)                  
294600                 PERFORM S16-SKRIV-W4768E                                 
294700               END-IF                                                     
294800               IF NDC-TH                                                  
294900                 MOVE 'THB' TO UT5-EKHT-KDVALISO                          
295000                 MOVE 'TH01' TO UT5-EKHT-KDTRADP                          
295100                 MOVE 'TH01' TO UT5-EKHT-IDCPYTXT(1:4)                    
295200                 MOVE 'EKHA'   TO UT5-EKHT-IDCPYTXT(5:4)                  
295300                 PERFORM S16-SKRIV-W4768E                                 
295400               END-IF                                                     
295500               IF NDC-TW                                                  
295600                 MOVE 'TWD' TO UT5-EKHT-KDVALISO                          
295700                 MOVE 'TW01' TO UT5-EKHT-KDTRADP                          
295800                 MOVE 'TW01' TO UT5-EKHT-IDCPYTXT(1:4)                    
295900                 MOVE 'EKHA'   TO UT5-EKHT-IDCPYTXT(5:4)                  
296000                 PERFORM S16-SKRIV-W4768E                                 
296100               END-IF                                                     
296200               IF NDC-MX                                                  
296300                 MOVE 'MXN' TO UT5-EKHT-KDVALISO                          
296400                 MOVE 'MX10' TO UT5-EKHT-KDTRADP                          
296500                 MOVE 'MX10' TO UT5-EKHT-IDCPYTXT(1:4)                    
296600                 MOVE 'EKHA'   TO UT5-EKHT-IDCPYTXT(5:4)                  
296700                 PERFORM S16-SKRIV-W4768E                                 
296800               END-IF                                                     
296900               IF NDC-BR                                                  
297000                 MOVE 'BRN' TO UT5-EKHT-KDVALISO                          
297100                 MOVE 'BR12' TO UT5-EKHT-KDTRADP                          
297200                 MOVE 'BR12' TO UT5-EKHT-IDCPYTXT(1:4)                    
297300                 MOVE 'EKHA'   TO UT5-EKHT-IDCPYTXT(5:4)                  
297400                 PERFORM S16-SKRIV-W4768E                                 
297500               END-IF                                                     
297600               IF NDC-ZA                                                  
297700                 MOVE 'ZAR' TO UT5-EKHT-KDVALISO                          
297800                 MOVE 'ZA04' TO UT5-EKHT-KDTRADP                          
297900                 MOVE 'ZA04' TO UT5-EKHT-IDCPYTXT(1:4)                    
298000                 MOVE 'EKHA'   TO UT5-EKHT-IDCPYTXT(5:4)                  
298100                 PERFORM S16-SKRIV-W4768E                                 
298200               END-IF                                                     
298300             END-IF                                                       
298400           ELSE                                                           
298500             IF NDC-CN OR LDC-CN                                          
298600               MOVE 'W570EKHA'       TO UT5-EKHT-IDCPYTXT                 
298700               PERFORM S16-SKRIV-W4768E                                   
298800             ELSE                                                         
298900               IF NDC-IN                                                  
299000                 MOVE 'W515EKHA'      TO UT5-EKHT-IDCPYTXT                
299100                 PERFORM S17-SKRIV-W4768F                                 
299200               ELSE                                                       
299300                 MOVE BET-KDTRADP    TO UT5-EKHT-IDCPYTXT(1:4)            
299400                 MOVE 'EKHA'         TO UT5-EKHT-IDCPYTXT(5:4)            
299500                 PERFORM S16-SKRIV-W4768E                                 
299600               END-IF                                                     
299700             END-IF                                                       
299800           END-IF                                                         
299900         END-IF                                                           
300000       ELSE                                                               
300100****     OM DET ÄR VISS SKROT SÅ SKALL ANTALET VARA NEGATIVT              
300200           MOVE WS-IDDISTR     TO TEST-IDDISTR                            
300300           EVALUATE KORD-KDFAKTYP                                         
300400           WHEN 'N'                                                       
300500             IF DIST18-SKROT OR DIST18-SCRAP-NDC                          
300600               MOVE SPACE TO WS-IDKUNDNR-CHECK                            
300700               MOVE IN-IDKUNDNR TO WS-IDKUNDNR                            
300800               MOVE WS-IDKUNDNR TO WS-IDKUNDNR-CHECK                      
300900         INSPECT WS-IDKUNDNR-CHECK REPLACING LEADING ZERO BY SPACE        
301000               IF WS-IDKUNDNR-CHECK(1:5) = ' '                            
301100               AND WS-IDKUNDNR-CHECK(6:2) = IN-IDDC                       
301200                 IF UT5-EKHT-KVANTAL > ZERO                               
301300                  COMPUTE UT5-EKHT-KVANTAL = UT5-EKHT-KVANTAL * -1        
301400                 END-IF                                                   
301500               END-IF                                                     
301600             END-IF                                                       
301700           END-EVALUATE                                                   
301800****                                                                      
301900           IF IN-IDPURAD = ZERO AND IN-IDKOLLI = ZERO                     
302000             MOVE SPACE               TO UT5-EKHT-IDFAKT-EXP              
302100             CONTINUE                                                     
302200           ELSE                                                           
302300             IF VORD-IDPRODNR NOT = IN-IDPRODNR OR                        
302400                KOLLI-IDKOLLI NOT = IN-IDKOLLI                            
302500               MOVE IN-IDPRODNR       TO W-IDPRODNR-E6                    
302600               PERFORM IMS-GU-WDE601                                      
302700               MOVE IN-IDKOLLI        TO W-IDKOLLI-E6                     
302800               PERFORM IMS-GNP-WDE611                                     
302900             END-IF                                                       
303000             MOVE KOLLI-IDFAKT-EXP    TO UT5-EKHT-IDFAKT-EXP              
303100           END-IF                                                         
303200**** FOR ADDITIONAL INVOICE GENERATED FROM A BOUNCE FLOW,                 
303300**** WRITE THE 2ND INVOICE REF                                            
303400*          IF DIST35-NONVCC-NONVCC-REFILL AND DCS-CDC                     
303500*          OR DIST35-NONVCC-NONVCC-TRANSFER AND DCS-CDC                   
303600           IF KORD-FLOVRLEV = JA                                          
303700**** ADDITIONAL INVOICE                                                   
303800             MOVE ORAD-BEVOLREF  TO UT5-EKHT-IDFAKT-EXP                   
303900           END-IF                                                         
304000*          END-IF                                                         
304100                                                                          
304200           MOVE 'SEPV' TO UT5-EKHT-KDTRADP                                
304300           PERFORM S15-SKRIV-W47660                                       
304400       END-IF                                                             
304500     END-IF                                                               
304600                                                                          
304700     MOVE WS-IDDISTR           TO TEST-IDDISTR                            
304800     EVALUATE KORD-KDFAKTYP                                               
304900                                                                          
305000**** HÄR SKAPAS RADERNA FÖR EKONOMISKA BOKNINGARNA ANDRA POSTEN           
305100     WHEN 'R'                                                             
305200**** REFILLER TILL KINA, INDIEN OCH ANDRA FAKTURAN FÖR BOUNCE             
305300         MOVE IN-IDDC TO WS-IDDC                                          
305400         IF DIST35-CDC-NONVCC-REFILL                                      
305500         OR DIST35-VCC-NONVCC-REFILL                                      
305600         OR DIST35-VCC-NONVCC-TRANSFER                                    
305700         OR (DIST35-NONVCC-NONVCC-REFILL AND DCS-CDC)                     
305800         OR (DIST35-NONVCC-NONVCC-TRANSFER AND DCS-CDC)                   
305900           MOVE '102'            TO UT5-EKHT-KDEKHHT                      
306000           IF KORD-FLOVRLEV = JA                                          
306100**** ADDITIONAL INVOICE                                                   
306200             MOVE '124'          TO UT5-EKHT-KDEKSHT                      
306300             IF (DIST35-NONVCC-NONVCC-REFILL AND DCS-CDC)                 
306400             OR (DIST35-NONVCC-NONVCC-TRANSFER AND DCS-CDC)               
306500               MOVE '134'        TO UT5-EKHT-KDEKSHT                      
306600             END-IF                                                       
306700             MOVE ORAD-BEVOLREF  TO UT5-EKHT-IDFAKT-EXP                   
306800           ELSE                                                           
306900**** GIT POST                                                             
307000             MOVE '120'          TO UT5-EKHT-KDEKSHT                      
307100             IF (DIST35-NONVCC-NONVCC-REFILL AND DCS-CDC)                 
307200             OR (DIST35-NONVCC-NONVCC-TRANSFER AND DCS-CDC)               
307300**** BOUNCE FLOW SECOND INVOICE GIT POST                                  
307400               MOVE '130'        TO UT5-EKHT-KDEKSHT                      
307500               MOVE '1441'       TO UT5-EKHT-IDLEVNR                      
307600             END-IF                                                       
307700           END-IF                                                         
307800           MOVE IN-IDDC          TO UT5-EKHT-IDDC-SEND                    
307900           IF UT5-EKHT-KDEKNIVA = 'DET'                                   
308000             PERFORM KA-SKAPA-RAD-POST5                                   
308100           ELSE                                                           
308200             PERFORM KB-SKAPA-TLG-POST5                                   
308300           END-IF                                                         
308400         END-IF                                                           
308500                                                                          
308600         MOVE IN-IDDC TO WS-IDDC                                          
308700         IF DIST35-CDC-RETURNS-NON-VCC                                    
308800           MOVE '102'          TO UT5-EKHT-KDEKHHT                        
308900           MOVE '123'          TO UT5-EKHT-KDEKSHT                        
309000           MOVE WC-CDC-SE      TO UT5-EKHT-IDDC-REC                       
309100           IF UT5-EKHT-KDEKNIVA = 'DET'                                   
309200             PERFORM KA-SKAPA-RAD-POST5                                   
309300           END-IF                                                         
309400         END-IF                                                           
309500                                                                          
309600**** REFILLER TILL CDC ELLER ANDRA FAKTURAN I BOUNCE FLÖDET               
309700         MOVE IN-IDDC TO WS-IDDC                                          
309800         IF DIST35-NONVCC-CDC-REFILL                                      
309900         OR DIST35-NONVCC-VCC-REFILL                                      
310000         OR DIST35-NONVCC-VCC-TRANSFER                                    
310100         OR (DIST35-NONVCC-NONVCC-REFILL AND NOT DCS-CDC)                 
310200         OR (DIST35-NONVCC-NONVCC-TRANSFER AND NOT DCS-CDC)               
310300           MOVE '102'            TO UT5-EKHT-KDEKHHT                      
310400**** GIT POST TO CDC                                                      
310500           MOVE '120'            TO UT5-EKHT-KDEKSHT                      
310600           MOVE DCS-IDLEVNR-DC   TO UT5-EKHT-IDLEVNR                      
310700           IF (DIST35-NONVCC-NONVCC-REFILL AND NOT DCS-CDC)               
310800           OR (DIST35-NONVCC-NONVCC-TRANSFER AND NOT DCS-CDC)             
310900**** WE NEED CDC STD PRIS                                                 
311000             MOVE ORAD-IDARTNR         TO W-IDARTNR                       
311100             PERFORM IMS-GU-WDK601                                        
311200             PERFORM IMS-GNP-WDK611                                       
311300             MOVE CLAG-PRARTSTD  TO UT5-EKHT-PRARTNTO                     
311400             MOVE '135'          TO UT5-EKHT-KDEKSHT                      
311500**** WE NEED TO HAVE RIGHT RECIEVING DC FOR BOUNCE FLOW                   
311600             MOVE WC-CDC-SE      TO UT5-EKHT-IDDC-REC                     
311700**** ADDITIONAL INVOICE                                                   
311800             IF KORD-FLOVRLEV = JA                                        
311900               MOVE '134'        TO UT5-EKHT-KDEKSHT                      
312000               MOVE ORAD-BEVOLREF TO UT5-EKHT-IDFAKT-EXP                  
312100             ELSE                                                         
312200               MOVE SPACE         TO UT5-EKHT-IDFAKT-EXP                  
312300             END-IF                                                       
312400           ELSE                                                           
312500             MOVE SPACE         TO UT5-EKHT-IDFAKT-EXP                    
312600           END-IF                                                         
312700           MOVE DCS-IDDC         TO UT5-EKHT-IDDC-SEND                    
312800           IF UT5-EKHT-KDEKNIVA = 'DET'                                   
312900             PERFORM KC-SKAPA-RAD-POST5                                   
313000           ELSE                                                           
313100             PERFORM KD-SKAPA-TLG-POST5                                   
313200           END-IF                                                         
313300         END-IF                                                           
313400                                                                          
313500**** SENDING DC IS DC11 AND IT'S VOR                                      
313600         MOVE WS-WDB6-IDFTG TO WS-IDFTG                                   
313700         IF IDFTG-PV                                                      
313800           MOVE WS-WDB2-IDFTG TO WS-IDFTG                                 
313900           IF IDFTG-NON-VCC                                               
314000             IF IN-KDFAKSTA-EXP = 1                                       
314100****   FOR VOR BOUNCE FLOW FIRST INVOICE                                  
314200               MOVE '102'    TO UT5-EKHT-KDEKHHT                          
314300               MOVE '125'    TO UT5-EKHT-KDEKSHT                          
314400               IF IN-IDPURAD = ZERO AND IN-IDKOLLI = ZERO                 
314500                 MOVE SPACE             TO UT5-EKHT-IDFAKT-EXP            
314600               ELSE                                                       
314700                 IF VORD-IDPRODNR NOT = IN-IDPRODNR OR                    
314800                    KOLLI-IDKOLLI NOT = IN-IDKOLLI                        
314900                   MOVE IN-IDPRODNR     TO W-IDPRODNR-E6                  
315000                   PERFORM IMS-GU-WDE601                                  
315100                   MOVE IN-IDKOLLI      TO W-IDKOLLI-E6                   
315200                   PERFORM IMS-GNP-WDE611                                 
315300                 END-IF                                                   
315400                                                                          
315500                 MOVE KOLLI-IDFAKT-EXP  TO UT5-EKHT-IDFAKT-EXP            
315600               END-IF                                                     
315700               IF UT5-EKHT-KDEKNIVA = 'DET'                               
315800                 PERFORM KA-SKAPA-RAD-POST5                               
315900               ELSE                                                       
316000                 PERFORM KB-SKAPA-TLG-POST5                               
316100               END-IF                                                     
316200             END-IF                                                       
316300           END-IF                                                         
316400         ELSE                                                             
316500           IF DIST35-REFILL                                               
316600           OR DIST35-NONVCC-REFILL                                        
316700           OR DIST35-VCC-NONVCC-TRANSFER                                  
316800           OR DIST35-NONVCC-VCC-TRANSFER                                  
316900           OR DIST35-NONVCC-NONVCC-TRANSFER                               
317000             CONTINUE                                                     
317100           ELSE                                                           
317200             IF IDFTG-NON-VCC OR                                          
317300                IDFTG-US                                                  
317400               MOVE WS-WDB2-IDFTG TO WS-IDFTG                             
317500               IF IDFTG-PV                                                
317600                 IF IN-KDFAKSTA-EXP = 1                                   
317700****     FOR IMPORTERS BOUNCE FLOW FIRST INVOICE                          
317800                   MOVE '102'  TO UT5-EKHT-KDEKHHT                        
317900                   MOVE '145'  TO UT5-EKHT-KDEKSHT                        
318000                   MOVE '1441'           TO UT5-EKHT-IDLEVNR              
318100                   IF IN-IDPURAD = ZERO AND IN-IDKOLLI = ZERO             
318200                     MOVE SPACE           TO UT5-EKHT-IDFAKT-EXP          
318300                   ELSE                                                   
318400                     IF VORD-IDPRODNR NOT = IN-IDPRODNR OR                
318500                        KOLLI-IDKOLLI NOT = IN-IDKOLLI                    
318600                       MOVE IN-IDPRODNR   TO W-IDPRODNR-E6                
318700                       PERFORM IMS-GU-WDE601                              
318800                       MOVE IN-IDKOLLI    TO W-IDKOLLI-E6                 
318900                       PERFORM IMS-GNP-WDE611                             
319000                     END-IF                                               
319100                                                                          
319200                     MOVE KOLLI-IDFAKT-EXP TO UT5-EKHT-IDFAKT-EXP         
319300                   END-IF                                                 
319400                   IF UT5-EKHT-KDEKNIVA = 'DET'                           
319500                     PERFORM KC-SKAPA-RAD-POST5                           
319600                   ELSE                                                   
319700                     PERFORM KD-SKAPA-TLG-POST5                           
319800                   END-IF                                                 
319900                 END-IF                                                   
320000               END-IF                                                     
320100             END-IF                                                       
320200           END-IF                                                         
320300         END-IF                                                           
320400                                                                          
320500     END-EVALUATE                                                         
320600     .                                                                    
320700     EJECT                                                                
320800                                                                          
320900 KA-SKAPA-RAD-POST5  SECTION.                                             
321000     MOVE 'KA-SKAPA-RAD-POST5'   TO WS-SEKTION                            
321100                                                                          
321200     MOVE 'W4765100'           TO UT5-EKHT-IDPGM                          
321300     ACCEPT UT5-EKHT-DAREGDAT FROM DATE                                   
321400     ACCEPT UT5-EKHT-TIKLOCK FROM TIME                                    
321500     MOVE IN-DAFINDOC          TO UT5-EKHT-DAVERDAT                       
321600     MOVE 1                    TO UT5-EKHT-IDSEKVNR                       
321700     MOVE IN-IDDISTR           TO UT5-EKHT-IDDISTR                        
321800     MOVE IN-IDKUNDNR          TO UT5-EKHT-IDKUNDNR                       
321900     MOVE ORAD-IDARTNR         TO UT5-EKHT-IDARTNR                        
322000                                                                          
322100     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
322200     MOVE WS-IDFAKT            TO CIA-IDARTBET-IN                         
322300     CALL W009CIA USING           CIA-W009CIA                             
322400     MOVE CIA-IDARTBET-UT      TO UT5-EKHT-IDVERGL                        
322500                                                                          
322600     MOVE 1                    TO UT5-EKHT-PRKURS                         
322700     MOVE KORD-FLOVRLEV        TO UT5-EKHT-FLOVRLEV                       
322800     MOVE ORAD-PRARTNTO        TO UT5-EKHT-PRARTNTO                       
322900     MOVE ZERO                 TO UT5-EKHT-PRARTSJK                       
323000     MOVE ORAD-PRAVCOST        TO IN-PRAVCOST                             
323100**** AVCOST SHOULD BE SAVED IN WDE411 IN PROGRAM 4637                     
323200     IF IN-PRAVCOST = ZERO                                                
323300       PERFORM S40-HAMTA-PRAVCOST                                         
323400     END-IF                                                               
323500     MOVE IN-PRAVCOST           TO UT5-EKHT-PRARTSTD                      
323600     MOVE ZERO                  TO UT5-EKHT-PRHEMTAG                      
323700                                   UT5-EKHT-PRLANDCO                      
323800                                   UT5-EKHT-PRINK                         
323900                                   UT5-EKHT-PRDIRLON                      
324000                                   UT5-EKHT-PRDMTRL                       
324100                                   UT5-EKHT-PROVRPAL                      
324200     IF IN-IDKOLLI NOT = ZERO                                             
324300       MOVE KKOLLI-KVLEVART    TO UT5-EKHT-KVANTAL                        
324400     ELSE                                                                 
324500       MOVE +1                 TO UT5-EKHT-KVANTAL                        
324600     END-IF                                                               
324700                                                                          
324800     IF KORD-FLLSBOK = 'J'                                                
324900       MOVE 'Y'                TO UT5-EKHT-FLLSBOK                        
325000     ELSE                                                                 
325100       MOVE KORD-FLLSBOK       TO UT5-EKHT-FLLSBOK                        
325200     END-IF                                                               
325300                                                                          
325400     MOVE KORD-KDFRAKT         TO UT5-EKHT-KDFRAKT                        
325500     MOVE ORAD-KDPRODSL        TO UT5-EKHT-KDPRODSL                       
325600     MOVE CLAG-KDPSLLOC        TO UT5-EKHT-KDPSLLOC                       
325700     MOVE ART-KDSORT           TO UT5-EKHT-KDSORT                         
325800                                                                          
325900     MOVE ZERO                 TO UT5-EKHT-SUBEL                          
326000                                  UT5-EKHT-BEVAT                          
326100                                  UT5-EKHT-IDKONTO                        
326200                                  UT5-EKHT-KDANMORS                       
326300                                  UT5-EKHT-SUVAT                          
326400                                  UT5-EKHT-IDORDNR5                       
326500     MOVE SPACE                TO UT5-EKHT-IDTRANS                        
326600                                  UT5-EKHT-IDANALYS                       
326700                                  UT5-EKHT-IDKST                          
326800                                  UT5-EKHT-IDUSER                         
326900     MOVE ZERO                 TO UT5-EKHT-DAAVIDAT                       
327000                                  UT5-EKHT-IDAVINR                        
327100                                  UT5-EKHT-KDAVVTYP                       
327200                                  UT5-EKHT-KDRT                           
327300                                  UT5-EKHT-KVANTMOT                       
327400                                  UT5-EKHT-KVAVIS                         
327500     MOVE SPACE                TO UT5-EKHT-FLDCET                         
327600     MOVE SPACE                TO UT5-EKHT-IDKUNDRF                       
327700     MOVE BET-KDTRADP          TO UT5-EKHT-KDTRADP                        
327800     MOVE BET-KDVALISO         TO UT5-EKHT-KDVALISO                       
327900     IF DIST35-CDC-NONVCC-REFILL                                          
328000     OR DIST35-VCC-NONVCC-REFILL                                          
328100     OR DIST35-VCC-NONVCC-TRANSFER                                        
328200     OR DIST35-CDC-RETURNS-NON-VCC                                        
328300     OR IDFTG-NON-VCC                                                     
328400     OR IDFTG-US                                                          
328500       IF IDFTG-CN                                                        
328600       OR DIST35-CN-CDC-RETURNS                                           
328700       OR DIST35-JP-NDC71-REFILL                                          
328800       OR DIST35-REFILL-CN                                                
328900         MOVE 'W570EKHA'       TO UT5-EKHT-IDCPYTXT                       
329000         PERFORM S16-SKRIV-W4768E                                         
329100       ELSE                                                               
329200         IF IDFTG-US                                                      
329300           MOVE 'W561EKHA'     TO UT5-EKHT-IDCPYTXT                       
329400           PERFORM S21-SKRIV-W4768G                                       
329500         ELSE                                                             
329600           IF IDFTG-IN                                                    
329700           OR DIST35-CDC-IN-REFILL                                        
329800           OR DIST35-JP-NDC67-REFILL                                      
329900           OR DIST35-IN-CDC-RETURNS                                       
330000             MOVE 'W515EKHA'     TO UT5-EKHT-IDCPYTXT                     
330100             PERFORM S17-SKRIV-W4768F                                     
330200           ELSE                                                           
330300             MOVE BET-KDTRADP    TO UT5-EKHT-IDCPYTXT(1:4)                
330400             MOVE 'EKHA'         TO UT5-EKHT-IDCPYTXT(5:4)                
330500             PERFORM S16-SKRIV-W4768E                                     
330600           END-IF                                                         
330700         END-IF                                                           
330800       END-IF                                                             
330900     ELSE                                                                 
331000       IF (DIST35-NONVCC-NONVCC-REFILL AND DCS-CDC)                       
331100       OR (DIST35-NONVCC-NONVCC-TRANSFER AND DCS-CDC)                     
331200         EVALUATE TRUE                                                    
331300         WHEN BET-KDTRADP = 'US01'                                        
331400           MOVE 'W561EKHA'       TO UT5-EKHT-IDCPYTXT                     
331500           PERFORM S21-SKRIV-W4768G                                       
331600         WHEN BET-KDTRADP = 'CN05'                                        
331700           MOVE 'W570EKHA'       TO UT5-EKHT-IDCPYTXT                     
331800           PERFORM S16-SKRIV-W4768E                                       
331900         WHEN BET-KDTRADP = 'IN07'                                        
332000           MOVE 'W515EKHA'       TO UT5-EKHT-IDCPYTXT                     
332100           PERFORM S17-SKRIV-W4768F                                       
332200         WHEN OTHER                                                       
332300           MOVE BET-KDTRADP    TO UT5-EKHT-IDCPYTXT(1:4)                  
332400           MOVE 'EKHA'         TO UT5-EKHT-IDCPYTXT(5:4)                  
332500           PERFORM S16-SKRIV-W4768E                                       
332600         END-EVALUATE                                                     
332700                                                                          
332800       ELSE                                                               
332900         IF IDFTG-US                                                      
333000           MOVE 'W561EKHA'       TO UT5-EKHT-IDCPYTXT                     
333100           PERFORM S21-SKRIV-W4768G                                       
333200         ELSE                                                             
333300           IF IDFTG-CN                                                    
333400             MOVE 'W570EKHA'       TO UT5-EKHT-IDCPYTXT                   
333500             PERFORM S16-SKRIV-W4768E                                     
333600           ELSE                                                           
333700             IF IDFTG-IN                                                  
333800               MOVE 'W515EKHA'       TO UT5-EKHT-IDCPYTXT                 
333900               PERFORM S17-SKRIV-W4768F                                   
334000             ELSE                                                         
334100               MOVE BET-KDTRADP    TO UT5-EKHT-IDCPYTXT(1:4)              
334200               MOVE 'EKHA'         TO UT5-EKHT-IDCPYTXT(5:4)              
334300               PERFORM S16-SKRIV-W4768E                                   
334400             END-IF                                                       
334500           END-IF                                                         
334600         END-IF                                                           
334700       END-IF                                                             
334800     END-IF                                                               
334900                                                                          
335000     .                                                                    
335100     EJECT                                                                
335200 KB-SKAPA-TLG-POST5  SECTION.                                             
335300     MOVE 'KB-SKAPA-TLG-POST5'   TO WS-SEKTION                            
335400                                                                          
335500     ACCEPT UT5-EKHT-DAREGDAT FROM DATE                                   
335600     ACCEPT UT5-EKHT-TIKLOCK FROM TIME                                    
335700                                                                          
335800     MOVE IN-IDDISTR           TO UT5-EKHT-IDDISTR                        
335900     MOVE IN-IDKUNDNR          TO UT5-EKHT-IDKUNDNR                       
336000     MOVE 1                    TO UT5-EKHT-PRKURS                         
336100     MOVE BET-KDTRADP          TO UT5-EKHT-KDTRADP                        
336200     MOVE BET-KDVALISO         TO UT5-EKHT-KDVALISO                       
336300     IF DIST35-CDC-NONVCC-REFILL                                          
336400     OR DIST35-VCC-NONVCC-REFILL                                          
336500     OR DIST35-VCC-NONVCC-TRANSFER                                        
336600     OR DIST35-CDC-RETURNS-NON-VCC                                        
336700     OR IDFTG-NON-VCC                                                     
336800       IF IDFTG-CN OR DIST35-REFILL-CN                                    
336900       OR DIST35-JP-NDC71-REFILL                                          
337000         MOVE 'W570EKHA'       TO UT5-EKHT-IDCPYTXT                       
337100         PERFORM S16-SKRIV-W4768E                                         
337200       ELSE                                                               
337300         IF IDFTG-US                                                      
337400           MOVE 'W561EKHA'     TO UT5-EKHT-IDCPYTXT                       
337500           PERFORM S21-SKRIV-W4768G                                       
337600         ELSE                                                             
337700           IF IDFTG-IN OR DIST35-CDC-IN-REFILL                            
337800           OR DIST35-JP-NDC67-REFILL                                      
337900             MOVE 'W515EKHA'     TO UT5-EKHT-IDCPYTXT                     
338000             PERFORM S17-SKRIV-W4768F                                     
338100           ELSE                                                           
338200             MOVE BET-KDTRADP   TO UT5-EKHT-IDCPYTXT(1:4)                 
338300             MOVE 'EKHA'         TO UT5-EKHT-IDCPYTXT(5:4)                
338400             PERFORM S16-SKRIV-W4768E                                     
338500           END-IF                                                         
338600         END-IF                                                           
338700       END-IF                                                             
338800     ELSE                                                                 
338900       IF (DIST35-NONVCC-NONVCC-REFILL AND DCS-CDC)                       
339000       OR (DIST35-NONVCC-NONVCC-TRANSFER AND DCS-CDC)                     
339100         EVALUATE TRUE                                                    
339200         WHEN BET-KDTRADP = 'US01'                                        
339300           MOVE 'W561EKHA'     TO UT5-EKHT-IDCPYTXT                       
339400           PERFORM S21-SKRIV-W4768G                                       
339500         WHEN BET-KDTRADP = 'CN05'                                        
339600           MOVE 'W570EKHA'     TO UT5-EKHT-IDCPYTXT                       
339700           PERFORM S16-SKRIV-W4768E                                       
339800         WHEN BET-KDTRADP = 'IN07'                                        
339900           MOVE 'W515EKHA'     TO UT5-EKHT-IDCPYTXT                       
340000           PERFORM S17-SKRIV-W4768F                                       
340100         WHEN OTHER                                                       
340200           MOVE BET-KDTRADP  TO UT5-EKHT-IDCPYTXT(1:4)                    
340300           MOVE 'EKHA'       TO UT5-EKHT-IDCPYTXT(5:4)                    
340400           PERFORM S16-SKRIV-W4768E                                       
340500         END-EVALUATE                                                     
340600       ELSE                                                               
340700         IF IDFTG-US                                                      
340800           MOVE 'W561EKHA'       TO UT5-EKHT-IDCPYTXT                     
340900           PERFORM S21-SKRIV-W4768G                                       
341000         ELSE                                                             
341100           IF IDFTG-CN                                                    
341200             MOVE 'W570EKHA'       TO UT5-EKHT-IDCPYTXT                   
341300             PERFORM S16-SKRIV-W4768E                                     
341400           ELSE                                                           
341500             IF IDFTG-IN                                                  
341600               MOVE 'W515EKHA'       TO UT5-EKHT-IDCPYTXT                 
341700               PERFORM S17-SKRIV-W4768F                                   
341800             ELSE                                                         
341900               MOVE BET-KDTRADP    TO UT5-EKHT-IDCPYTXT(1:4)              
342000               MOVE 'EKHA'         TO UT5-EKHT-IDCPYTXT(5:4)              
342100               PERFORM S16-SKRIV-W4768E                                   
342200             END-IF                                                       
342300           END-IF                                                         
342400         END-IF                                                           
342500       END-IF                                                             
342600     END-IF                                                               
342700                                                                          
342800**** ROW FOR WAITING ON INBOUND                                           
342900     IF UT5-EKHT-KDEKHHT = '102'  AND                                     
343000       (UT5-EKHT-KDEKSHT = '120'  OR                                      
343100        UT5-EKHT-KDEKSHT = '130')                                         
343200**** 102-120 POST                                                         
343300       IF DIST35-CDC-NONVCC-REFILL                                        
343400       OR DIST35-VCC-NONVCC-REFILL                                        
343500       OR DIST35-VCC-NONVCC-TRANSFER                                      
343600         IF IDFTG-CN OR DIST35-REFILL-CN                                  
343700         OR DIST35-JP-NDC71-REFILL                                        
343800           MOVE 'W570EKHB'     TO UT5-EKHT-IDCPYTXT                       
343900           PERFORM S16-SKRIV-W4768E                                       
344000         ELSE                                                             
344100           IF IDFTG-IN OR DIST35-CDC-IN-REFILL                            
344200           OR DIST35-JP-NDC67-REFILL                                      
344300             MOVE 'W515EKHB'   TO UT5-EKHT-IDCPYTXT                       
344400             PERFORM S17-SKRIV-W4768F                                     
344500           ELSE                                                           
344600             MOVE BET-KDTRADP  TO UT5-EKHT-IDCPYTXT(1:4)                  
344700             MOVE 'EKHB'       TO UT5-EKHT-IDCPYTXT(5:4)                  
344800             PERFORM S16-SKRIV-W4768E                                     
344900           END-IF                                                         
345000         END-IF                                                           
345100       ELSE                                                               
345200**** 102-130 POST                                                         
345300         IF DIST35-NONVCC-NONVCC-REFILL                                   
345400         OR DIST35-NONVCC-NONVCC-TRANSFER                                 
345500           EVALUATE TRUE                                                  
345600           WHEN BET-KDTRADP = 'US01'                                      
345700             MOVE 'W561EKHB'     TO UT5-EKHT-IDCPYTXT                     
345800             PERFORM S21-SKRIV-W4768G                                     
345900           WHEN BET-KDTRADP = 'CN05'                                      
346000             MOVE 'W570EKHB'     TO UT5-EKHT-IDCPYTXT                     
346100             PERFORM S16-SKRIV-W4768E                                     
346200           WHEN BET-KDTRADP = 'IN07'                                      
346300             MOVE 'W515EKHB'     TO UT5-EKHT-IDCPYTXT                     
346400             PERFORM S17-SKRIV-W4768F                                     
346500           WHEN OTHER                                                     
346600             MOVE BET-KDTRADP  TO UT5-EKHT-IDCPYTXT(1:4)                  
346700             MOVE 'EKHB'       TO UT5-EKHT-IDCPYTXT(5:4)                  
346800             PERFORM S16-SKRIV-W4768E                                     
346900           END-EVALUATE                                                   
347000         END-IF                                                           
347100       END-IF                                                             
347200     END-IF                                                               
347300     .                                                                    
347400     EJECT                                                                
347500 KC-SKAPA-RAD-POST5  SECTION.                                             
347600     MOVE 'KC-SKAPA-RAD-POST5'   TO WS-SEKTION                            
347700                                                                          
347800     MOVE 'W4765100'           TO UT5-EKHT-IDPGM                          
347900     MOVE FUNCTION CURRENT-DATE (1:8) TO UT5-EKHT-DAREGDAT                
348000     ACCEPT UT5-EKHT-TIKLOCK FROM TIME                                    
348100     MOVE IN-DAFINDOC          TO UT5-EKHT-DAVERDAT                       
348200     MOVE 1                    TO UT5-EKHT-IDSEKVNR                       
348300     MOVE IN-IDDISTR           TO UT5-EKHT-IDDISTR                        
348400     MOVE IN-IDKUNDNR          TO UT5-EKHT-IDKUNDNR                       
348500     MOVE ORAD-IDARTNR         TO UT5-EKHT-IDARTNR                        
348600                                                                          
348700     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
348800     MOVE WS-IDFAKT            TO CIA-IDARTBET-IN                         
348900     CALL W009CIA USING           CIA-W009CIA                             
349000     MOVE CIA-IDARTBET-UT      TO UT5-EKHT-IDVERGL                        
349100                                                                          
349200     MOVE DCS-KDVALISO         TO UT5-EKHT-KDVALISO                       
349300     MOVE 1                    TO UT5-EKHT-PRKURS                         
349400     MOVE KORD-FLOVRLEV        TO UT5-EKHT-FLOVRLEV                       
349500**** SKALL TILL SEPV OCH VISA ATT DEN KOMMER FRÅN EXPORT                  
349600     MOVE 'W510EKHA'           TO UT5-EKHT-IDCPYTXT                       
349700     IF (DIST35-NONVCC-NONVCC-REFILL AND NOT DCS-CDC)                     
349800     OR (DIST35-NONVCC-NONVCC-TRANSFER AND NOT DCS-CDC)                   
349900**** WE SHALL HAVE STD AS PRICE HERE                                      
350000       CONTINUE                                                           
350100     ELSE                                                                 
350200       IF   UT5-EKHT-KDEKHHT = '102'                                      
350300       AND UT5-EKHT-KDEKSHT = '145'                                       
350400**** WE SHALL HAVE STD AS PRICE HERE                                      
350500         MOVE CLAG-PRARTSTD    TO UT5-EKHT-PRARTNTO                       
350600       ELSE                                                               
350700         MOVE ORAD-PRARTNTO    TO UT5-EKHT-PRARTNTO                       
350800       END-IF                                                             
350900     END-IF                                                               
351000     MOVE ZERO                 TO UT5-EKHT-PRARTSJK                       
351100     MOVE ORAD-PRAVCOST        TO IN-PRAVCOST                             
351200**** AVCOST SHOULD BE SAVED IN WDE411 IN PROGRAM 4637                     
351300     IF IN-PRAVCOST = ZERO                                                
351400       PERFORM S40-HAMTA-PRAVCOST                                         
351500     END-IF                                                               
351600     MOVE IN-PRAVCOST          TO UT5-EKHT-PRARTSTD                       
351700     MOVE ZERO                 TO UT5-EKHT-PRHEMTAG                       
351800                                  UT5-EKHT-PRLANDCO                       
351900                                  UT5-EKHT-PRINK                          
352000                                  UT5-EKHT-PRDIRLON                       
352100                                  UT5-EKHT-PRDMTRL                        
352200                                  UT5-EKHT-PROVRPAL                       
352300     IF IN-IDKOLLI NOT = ZERO                                             
352400       MOVE KKOLLI-KVLEVART    TO UT5-EKHT-KVANTAL                        
352500     ELSE                                                                 
352600       MOVE +1                 TO UT5-EKHT-KVANTAL                        
352700     END-IF                                                               
352800                                                                          
352900     IF KORD-FLLSBOK = 'J'                                                
353000       MOVE 'Y'                TO UT5-EKHT-FLLSBOK                        
353100     ELSE                                                                 
353200       MOVE KORD-FLLSBOK       TO UT5-EKHT-FLLSBOK                        
353300     END-IF                                                               
353400                                                                          
353500     MOVE KORD-KDFRAKT         TO UT5-EKHT-KDFRAKT                        
353600     MOVE ORAD-KDPRODSL        TO UT5-EKHT-KDPRODSL                       
353700     MOVE CLAG-KDPSLLOC        TO UT5-EKHT-KDPSLLOC                       
353800     MOVE ART-KDSORT           TO UT5-EKHT-KDSORT                         
353900                                                                          
354000     MOVE ZERO                 TO UT5-EKHT-SUBEL                          
354100                                  UT5-EKHT-BEVAT                          
354200                                  UT5-EKHT-IDKONTO                        
354300                                  UT5-EKHT-KDANMORS                       
354400                                  UT5-EKHT-SUVAT                          
354500                                  UT5-EKHT-IDORDNR5                       
354600     MOVE SPACE                TO UT5-EKHT-IDTRANS                        
354700                                  UT5-EKHT-IDANALYS                       
354800                                  UT5-EKHT-IDUSER                         
354900     IF WS-IDKST NOT = SPACES                                             
355000       MOVE WS-IDKST           TO UT5-EKHT-IDKST                          
355100     ELSE                                                                 
355200       MOVE SPACES             TO UT5-EKHT-IDKST                          
355300     END-IF                                                               
355400     MOVE 0                    TO UT5-EKHT-DAAVIDAT                       
355500                                  UT5-EKHT-IDAVINR                        
355600                                  UT5-EKHT-KDAVVTYP                       
355700                                  UT5-EKHT-KDRT                           
355800                                  UT5-EKHT-KVANTMOT                       
355900                                  UT5-EKHT-KVAVIS                         
356000     MOVE 'SEPV'               TO UT5-EKHT-KDTRADP                        
356100     MOVE SPACE                TO UT5-EKHT-FLDCET                         
356200     MOVE SPACE                TO UT5-EKHT-IDKUNDRF                       
356300                                                                          
356400     IF IN-IDPURAD = ZERO AND IN-IDKOLLI = ZERO                           
356500       CONTINUE                                                           
356600     ELSE                                                                 
356700       IF VORD-IDPRODNR NOT = IN-IDPRODNR OR                              
356800          KOLLI-IDKOLLI NOT = IN-IDKOLLI                                  
356900         MOVE IN-IDPRODNR      TO W-IDPRODNR-E6                           
357000         PERFORM IMS-GU-WDE601                                            
357100         MOVE IN-IDKOLLI       TO W-IDKOLLI-E6                            
357200         PERFORM IMS-GNP-WDE611                                           
357300       END-IF                                                             
357400                                                                          
357500       MOVE KOLLI-IDFAKT-EXP TO UT5-EKHT-IDFAKT-EXP                       
357600     END-IF                                                               
357700                                                                          
357800     PERFORM S15-SKRIV-W47660                                             
357900     .                                                                    
358000     EJECT                                                                
358100 KD-SKAPA-TLG-POST5  SECTION.                                             
358200     MOVE 'KD-SKAPA-TLG-POST5'   TO WS-SEKTION                            
358300                                                                          
358400     MOVE FUNCTION CURRENT-DATE (1:8) TO UT5-EKHT-DAREGDAT                
358500     ACCEPT UT5-EKHT-TIKLOCK FROM TIME                                    
358600                                                                          
358700     MOVE IN-IDDISTR           TO UT5-EKHT-IDDISTR                        
358800     MOVE IN-IDKUNDNR          TO UT5-EKHT-IDKUNDNR                       
358900     MOVE DCS-KDVALISO         TO UT5-EKHT-KDVALISO                       
359000     MOVE 1                    TO UT5-EKHT-PRKURS                         
359100     MOVE 'SEPV'               TO UT5-EKHT-KDTRADP                        
359200     MOVE 'W510EKHA'           TO UT5-EKHT-IDCPYTXT                       
359300                                                                          
359400     IF IN-IDPURAD = ZERO AND IN-IDKOLLI = ZERO                           
359500       CONTINUE                                                           
359600     ELSE                                                                 
359700       IF VORD-IDPRODNR NOT = IN-IDPRODNR OR                              
359800          KOLLI-IDKOLLI NOT = IN-IDKOLLI                                  
359900         MOVE IN-IDPRODNR      TO W-IDPRODNR-E6                           
360000         PERFORM IMS-GU-WDE601                                            
360100         MOVE IN-IDKOLLI       TO W-IDKOLLI-E6                            
360200         PERFORM IMS-GNP-WDE611                                           
360300       END-IF                                                             
360400                                                                          
360500       MOVE KOLLI-IDFAKT-EXP   TO UT5-EKHT-IDFAKT-EXP                     
360600     END-IF                                                               
360700                                                                          
360800     PERFORM S15-SKRIV-W47660                                             
360900                                                                          
361000**** SKALL TILL SEPV OCH MARKERAR ATT DEN KOMMER FRÅN EXPORT              
361100**** THIS IS A COPY FOR THE 102-121 EVENT                                 
361200**** 102-135 OR 102-125 OR 102-145 SHALL NOT HAVE THIS                    
361300     IF  UT5-EKHT-KDEKHHT = '102'                                         
361400     AND UT5-EKHT-KDEKSHT = '120'                                         
361500       IF DIST35-NONVCC-CDC-REFILL                                        
361600       OR DIST35-NONVCC-VCC-REFILL                                        
361700       OR DIST35-NONVCC-VCC-TRANSFER                                      
361800         MOVE SPACE TO WS-IDKUNDNR-CHECK                                  
361900         MOVE IN-IDKUNDNR TO WS-IDKUNDNR                                  
362000         MOVE WS-IDKUNDNR TO WS-IDKUNDNR-CHECK                            
362100         IF DIST35-NDCCN-CDC-REFILL                                       
362200         OR DIST35-NDCCN-JP-REFILL                                        
362300         OR DIST35-NDCCN-AU-REFILL                                        
362400         OR (DIST35-NONVCC-VCC-TRANSFER                                   
362500         AND (WS-IDKUNDNR-CHECK(6:2) = '71'                               
362600         OR  WS-IDKUNDNR-CHECK(6:2) = '72'                                
362700         OR  WS-IDKUNDNR-CHECK(6:2) = '73'                                
362800         OR  WS-IDKUNDNR-CHECK(6:2) = '74'))                              
362900           MOVE 'W570EKHA'         TO UT5-EKHT-IDCPYTXT                   
363000         ELSE                                                             
363100           IF DIST35-NDCUS-CDC-REFILL                                     
363200           OR DIST35-NDCUS-JP-REFILL                                      
363300           OR DIST35-NDCUS-AU-REFILL                                      
363400**** HERE WE NEED TO ADD TRANSFER FROM US TO JP/AU                        
363500             MOVE 'W561EKHA'       TO UT5-EKHT-IDCPYTXT                   
363600           ELSE                                                           
363700**** IF NDCIN TO CDC OR JP/AU MOVE W515EKHA                               
363800             MOVE BET-KDTRADP      TO UT5-EKHT-IDCPYTXT(1:4)              
363900             MOVE 'EKHA'           TO UT5-EKHT-IDCPYTXT(5:4)              
364000           END-IF                                                         
364100         END-IF                                                           
364200       END-IF                                                             
364300       PERFORM S15-SKRIV-W47660                                           
364400     END-IF                                                               
364500                                                                          
364600     .                                                                    
364700     EJECT                                                                
364800 XX-UT5 SECTION.                                                          
364900     COMPUTE XX-PRARTNTO = IN-SUNTO-LINE /                                
365000                           KKOLLI-KVLEVART                                
365100     COMPUTE XX-PRARTBTO = IN-SUBTO-LINE /                                
365200                           KKOLLI-KVLEVART                                
365300     .                                                                    
365400     EJECT                                                                
365500 L-SKAPA-HUVUD-POSTER6  SECTION.                                          
365600     MOVE 'L-SKAPA-HUVUD-POSTER6'  TO WS-SEKTION                          
365700*      VR-TRANSAR                                                         
365800                                                                          
365900     MOVE IN-W476FAKT               TO BILL-W4760001                      
366000     PERFORM LA-SKAPA-VR1                                                 
366100     PERFORM LB-SKAPA-VR2                                                 
366200     PERFORM LC-SKAPA-VR3                                                 
366300     PERFORM LD-SKAPA-VR4                                                 
366400     .                                                                    
366500     EJECT                                                                
366600 L-SKAPA-RAD-POST6  SECTION.                                              
366700     MOVE 'L-SKAPA-RAD-POST6'    TO WS-SEKTION                            
366800                                                                          
366900     MOVE IN-W476FAKT               TO BILL-W4760001                      
367000     PERFORM LE-SKAPA-VR5                                                 
367100     .                                                                    
367200     EJECT                                                                
367300 LA-SKAPA-VR1  SECTION.                                                   
367400     MOVE 'LA-SKAPA-VR1'  TO WS-SEKTION                                   
367500                                                                          
367600     IF WS6-IDFAKT NOT = BILL-IDFAKT  AND                                 
367700        FL-SKRIV-VR1 = JA                                                 
367800        WRITE VR-POST-1 FROM VR1-UTAREA                                   
367900        MOVE SPACE                 TO VR-DATA-AREA                        
368000                                                                          
368100        MOVE 'W47662'              TO POSTSUM-FDNAMN                      
368200        MOVE 'VR      '            TO POSTSUM-DDNAMN2                     
368300        MOVE 'VR1'                 TO POSTSUM-TRANSTYP                    
368400        CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                     
368500                                                                          
368600        MOVE NEJ TO FL-SKRIV-VR1                                          
368700                                                                          
368800     END-IF                                                               
368900     IF WS6-IDFAKT NOT = BILL-IDFAKT                                      
369000       MOVE 'VR1'                   TO VR-IDPTYP                          
369100       MOVE BILL-IDDISTR            TO VR-IDDISTR                         
369200                                       TEST-IDDISTR                       
369300       MOVE BILL-IDKUNDNR           TO VR-IDKUNDNR                        
369400       MOVE BILL-IDDC               TO VR-IDDC                            
369500       MOVE BILL-IDFAKT             TO VR-IDFAKT                          
369600       MOVE KORD-KDFAKTYP           TO VR-KDFAKTYP                        
369700       MOVE SPACE                   TO VR-IDKUNDRF                        
369800       MOVE ZERO                    TO VR-IDPRODNR                        
369900                                       VR-IDKOLLI                         
370000                                       VR-SORTARG                         
370100                                       VR-IDARTNR                         
370200       MOVE W-TIAAMMDD              TO VR-TIAAMMDD                        
370300       MOVE W-TIKLOCK               TO VR-TIKLOCK                         
370400                                                                          
370500       MOVE KORD-KDFRAKT            TO VR1-KDFRAKT                        
370600       MOVE ZERO                    TO VR1-PREMBHNT                       
370700       MOVE ZERO                    TO VR1-PRFRAKT                        
370800       MOVE ZERO                    TO VR1-PRFOERS                        
370900       MOVE BILL-SUBTO-TOT          TO VR1-SUFKTBEL                       
371000       MOVE BILL-DAFINDOC(3:6)      TO VR1-TIFAKT                         
371100       MOVE BILL-PRKURS-FIKTIV      TO VR1-PRKURS                         
371200       MOVE BILL-KDVALISO-FAKT      TO VR1-KDVALISO                       
371300       IF KORD-FLOVRLEV = JA                                              
371400         MOVE '01'                  TO VR1-KDREFNOT                       
371500       ELSE                                                               
371600         MOVE '00'                  TO VR1-KDREFNOT                       
371700       END-IF                                                             
371800       MOVE VR-UTAREA               TO VR1-UTAREA                         
371900       MOVE JA                      TO FL-SKRIV-VR1                       
372000     END-IF                                                               
372100                                                                          
372200     .                                                                    
372300     EJECT                                                                
372400 LB-SKAPA-VR2  SECTION.                                                   
372500     MOVE 'LB-SKAPA-VR2'  TO WS-SEKTION                                   
372600                                                                          
372700     IF WS6-IDFAKT NOT   = IN-IDFAKT   OR                                 
372800        WS6-IDPRODNR NOT = IN-IDPRODNR                                    
372900       IF IN-IDPRODNR NOT = ZERO                                          
373000         MOVE KORD-KDORDKL          TO VR2-KDORDKL                        
373100                                                                          
373200         MOVE 'VR2'                 TO VR-IDPTYP                          
373300         MOVE BILL-IDPRODNR         TO VR-IDPRODNR                        
373400         MOVE KORD-IDKUNDRF         TO VR-IDKUNDRF                        
373500         MOVE +99999                TO VR-IDKOLLI                         
373600         MOVE '0'                   TO VR-SORTARG                         
373700         MOVE ZERO                  TO VR-IDARTNR                         
373800         MOVE BILL-IDKUNDNR         TO VR-IDKUNDNR                        
373900         MOVE W-TIAAMMDD            TO VR-TIAAMMDD                        
374000         MOVE W-TIKLOCK             TO VR-TIKLOCK                         
374100                                                                          
374200         WRITE VR-POST-2 FROM VR-UTAREA                                   
374300         MOVE SPACE TO VR-DATA-AREA                                       
374400                                                                          
374500         MOVE 'W47662'              TO POSTSUM-FDNAMN                     
374600         MOVE 'VR      '            TO POSTSUM-DDNAMN2                    
374700         MOVE 'VR2'                 TO POSTSUM-TRANSTYP                   
374800                                                                          
374900         CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                    
375000       END-IF                                                             
375100     END-IF                                                               
375200     .                                                                    
375300     EJECT                                                                
375400 LC-SKAPA-VR3 SECTION.                                                    
375500      MOVE 'LC-SKAPA-VR3'  TO WS-SEKTION                                  
375600                                                                          
375700      IF WS6-IDFAKT NOT   = IN-IDFAKT   OR                                
375800         WS6-IDPRODNR NOT = IN-IDPRODNR OR                                
375900         WS6-IDKOLLI NOT  = IN-IDKOLLI                                    
376000        IF IN-IDKOLLI NOT = ZERO                                          
376100          MOVE KORD-IDKUNDRF         TO VR-IDKUNDRF                       
376200          MOVE KORD-KDORDKL          TO VR3-KDORDKL                       
376300                                                                          
376400          MOVE 'VR3'                 TO VR-IDPTYP                         
376500          MOVE BILL-IDPRODNR         TO VR-IDPRODNR                       
376600          MOVE BILL-IDKOLLI          TO VR-IDKOLLI                        
376700          MOVE '0'                   TO VR-SORTARG                        
376800          MOVE ZERO                  TO VR-IDARTNR                        
376900          MOVE BILL-IDKUNDNR         TO VR-IDKUNDNR                       
377000          MOVE W-TIAAMMDD            TO VR-TIAAMMDD                       
377100          MOVE W-TIKLOCK             TO VR-TIKLOCK                        
377200                                                                          
377300          MOVE KOLLI-IDLBBET         TO VR3-IDLBBET                       
377400                                                                          
377500          WRITE VR-POST-3 FROM VR-UTAREA                                  
377600          MOVE SPACE TO VR-DATA-AREA                                      
377700                                                                          
377800          MOVE 'W47662'              TO POSTSUM-FDNAMN                    
377900          MOVE 'VR      '            TO POSTSUM-DDNAMN2                   
378000          MOVE 'VR3'                 TO POSTSUM-TRANSTYP                  
378100                                                                          
378200          CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                   
378300       END-IF                                                             
378400     END-IF                                                               
378500     .                                                                    
378600     EJECT                                                                
378700 LD-SKAPA-VR4  SECTION.                                                   
378800     MOVE 'LD-SKAPA-VR4'   TO WS-SEKTION                                  
378900                                                                          
379000     IF WS6-IDFAKT NOT   = IN-IDFAKT   OR                                 
379100        WS6-IDPRODNR NOT = IN-IDPRODNR OR                                 
379200        WS6-IDKOLLI NOT  = IN-IDKOLLI                                     
379300        IF IN-IDKOLLI NOT = ZERO                                          
379400         MOVE 'VR4'                    TO VR-IDPTYP                       
379500         MOVE BILL-IDPRODNR            TO VR-IDPRODNR                     
379600         MOVE KORD-IDKUNDRF            TO VR-IDKUNDRF                     
379700         MOVE BILL-IDKOLLI             TO VR-IDKOLLI                      
379800         MOVE '1'                      TO VR-SORTARG                      
379900         MOVE ZERO                     TO VR-IDARTNR                      
380000         MOVE BILL-IDKUNDNR            TO VR-IDKUNDNR                     
380100         MOVE W-TIAAMMDD               TO VR-TIAAMMDD                     
380200         MOVE W-TIKLOCK                TO VR-TIKLOCK                      
380300         IF VORD-IDPRODNR NOT = BILL-IDPRODNR OR                          
380400           KOLLI-IDKOLLI NOT = BILL-IDKOLLI                               
380500           MOVE BILL-IDPRODNR      TO W-IDPRODNR-E6                       
380600           MOVE BILL-IDKOLLI       TO W-IDKOLLI-E6                        
380700           PERFORM IMS-GU-WDE601                                          
380800           PERFORM IMS-GNP-WDE611                                         
380900         END-IF                                                           
381000                                                                          
381100         MOVE KOLLI-VKORDBTO-KOLLI     TO VR4-VKORDBTO                    
381200         MOVE KOLLI-VLORDBTO-KOLLI     TO VR4-VLORDBTO                    
381300                                                                          
381400         WRITE VR-POST-4 FROM VR-UTAREA                                   
381500         MOVE SPACE TO VR-DATA-AREA                                       
381600                                                                          
381700         MOVE 'W47662'                 TO POSTSUM-FDNAMN                  
381800         MOVE 'VR      '               TO POSTSUM-DDNAMN2                 
381900         MOVE 'VR4'                    TO POSTSUM-TRANSTYP                
382000                                                                          
382100         CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                    
382200       END-IF                                                             
382300     END-IF                                                               
382400     .                                                                    
382500     EJECT                                                                
382600 LE-SKAPA-VR5  SECTION.                                                   
382700     MOVE 'LE-SKAPA-VR5'    TO WS-SEKTION                                 
382800                                                                          
382900     MOVE IN-IDDISTR            TO TEST-IDDISTR                           
383000     IF IN-IDKOLLI = ZERO AND IN-IDPURAD  = ZERO                          
383100*        TILLÄGG/AVDRAG                                                   
383200                                                                          
383300       IF IN-BEART = 'FREIGHT'                                            
383400         ADD  IN-SUNTO-LINE     TO VRSPAR1-PRFRAKT                        
383500*        ADD  IN-SUNTO-LINE     TO  WSPAR1-PRFRAKT                        
383600       END-IF                                                             
383700       IF IN-BEART = 'INSURANCE'                                          
383800         ADD  IN-SUNTO-LINE     TO VRSPAR1-PRFOERS                        
383900*        ADD  IN-SUNTO-LINE     TO  WSPAR1-PRFOERS                        
384000       END-IF                                                             
384100       IF IN-BEART = 'PACKING & HANDLING'                                 
384200         ADD  IN-SUNTO-LINE     TO VRSPAR1-PREMBHNT                       
384300*        ADD  IN-SUNTO-LINE     TO  WSPAR1-PREMBHNT                       
384400       END-IF                                                             
384500       IF IN-BEART = 'SERVICE FEE'                                        
384600         ADD  IN-SUNTO-LINE     TO VRSPAR1-PREMBHNT                       
384700*        ADD  IN-SUNTO-LINE     TO  WSPAR1-PREMBHNT                       
384800       END-IF                                                             
384900     ELSE                                                                 
385000       MOVE 'VR5'                    TO VR-IDPTYP                         
385100       MOVE KORD-IDKUNDRF            TO VR-IDKUNDRF                       
385200       MOVE BILL-IDPRODNR            TO VR-IDPRODNR                       
385300       MOVE BILL-IDKOLLI             TO VR-IDKOLLI                        
385400       MOVE '0'                      TO VR-SORTARG                        
385500       MOVE ORAD-IDARTNR             TO VR-IDARTNR                        
385600       MOVE BILL-IDKUNDNR            TO VR-IDKUNDNR                       
385700       MOVE W-TIAAMMDD               TO VR-TIAAMMDD                       
385800       MOVE W-TIKLOCK                TO VR-TIKLOCK                        
385900                                                                          
386000       MOVE ORAD-IDKUNDRF-RO         TO VR5-IDKUNDRF-RO                   
386100       MOVE KKOLLI-KVLEVART          TO VR5-KVLEVART                      
386200       MOVE ORAD-KDVRINFO            TO VR5-KDVRINFO                      
386300**     IF ORAD-IDARTNR NOT = W-IDARTNR                                    
386400         MOVE ORAD-IDARTNR           TO W-IDARTNR                         
386500         PERFORM IMS-GU-WDK601                                            
386600         PERFORM IMS-GNP-WDK611                                           
386700**     END-IF                                                             
386800       MOVE CLAG-KDSRA               TO VR5-KDSRA                         
386900       MOVE CLAG-KDARTURS            TO VR5-KDARTURS                      
387000       MOVE BILL-IDFKNGRP            TO VR5-IDFKNGRP                      
387100       EVALUATE   ORAD-KDQPACK                                            
387200         WHEN 0                                                           
387300           MOVE CLAG-KVQPACK-0       TO VR5-KVQPACK                       
387400         WHEN 1                                                           
387500           MOVE CLAG-KVQPACK-1       TO VR5-KVQPACK                       
387600         WHEN 2                                                           
387700           MOVE CLAG-KVQPACK-2       TO VR5-KVQPACK                       
387800         WHEN 3                                                           
387900           MOVE CLAG-KVQPACK-3       TO VR5-KVQPACK                       
388000         WHEN 4                                                           
388100           MOVE CLAG-KVQPACK-4       TO VR5-KVQPACK                       
388200       END-EVALUATE                                                       
388300       COMPUTE VR5-VKART = BILL-VKARTNTO * 1000                           
388400       MOVE ORAD-PRARTNTO            TO VR5-PRARTNTO                      
388500       MOVE BILL-IDDISTR             TO TEST-IDDISTR                      
388600       IF DIST79-DEALER-PRICE                                             
388700         MOVE ORAD-PRARTNTO-LOC      TO VR5-PRARTNTO                      
388800         IF VR5-PRARTNTO = ZERO                                           
388900          MOVE ORAD-PRARTNTO-LOCPREL  TO VR5-PRARTNTO                     
389000         END-IF                                                           
389100       END-IF                                                             
389200       MOVE ORAD-PRARTULL            TO VR5-PRARTULL                      
389300       PERFORM S07-W335PRIS                                               
389400       MOVE PRIS-KDARTRAB            TO VR5-KDRABATT                      
389500       MOVE BILL-BEART               TO VR5-BEART                         
389600                                                                          
389700       IF ORAD-TIPRIS  > ZERO                                             
389800           MOVE ORAD-TIPRIS          TO VR5-TIPRIS                        
389900       ELSE                                                               
390000           MOVE KORD-TIORDREG        TO VR5-TIPRIS                        
390100       END-IF                                                             
390200       MOVE ORAD-BERADREF            TO VR5-BERADREF                      
390300       IF KORD-IDORDER NOT = OHUV-IDORDER                                 
390400         AND KORD-IDORDER NOT = ZERO                                      
390500         MOVE KORD-IDORDER           TO W-IDORDER                         
390600         PERFORM IMS-GU-WDQ201                                            
390700       END-IF                                                             
390800       MOVE OHUV-KDTULLVE            TO VR5-KDTULLVE                      
390900       MOVE ZERO                     TO VR5-REBPRIS                       
391000*      MOVE CLAG-PRARTBTO-EXP        TO VR5-PRARTBTO-EXP                  
391100       MOVE PRIS-PRARTBTO-MARK       TO VR5-PRARTBTO-EXP                  
391200                                                                          
391300       IF ORAD-KDPRTYP = 'P'                                              
391400          MOVE 'M'                   TO VR5-FLPRTILL                      
391500       ELSE                                                               
391600         IF ORAD-FLPRTILL = 'J'                                           
391700           MOVE 'Y'                  TO VR5-FLPRTILL                      
391800          ELSE                                                            
391900            MOVE 'N'                 TO VR5-FLPRTILL                      
392000          END-IF                                                          
392100       END-IF                                                             
392200                                                                          
392300       MOVE ORAD-IDKLIENT            TO VR5-IDKLIENT                      
392400       MOVE ORAD-IDARBREF            TO VR5-IDARBREF                      
392500       MOVE ORAD-IDBIL               TO VR5-IDBIL                         
392600       MOVE ORAD-IDVIN               TO VR5-IDVIN                         
392700       MOVE ART-KDSORT               TO VR5-KDSORT                        
392800                                                                          
392900       WRITE VR-POST-5 FROM VR-UTAREA                                     
393000       MOVE SPACE TO VR-DATA-AREA                                         
393100                                                                          
393200       MOVE 'W47662'                 TO POSTSUM-FDNAMN                    
393300       MOVE 'VR      '               TO POSTSUM-DDNAMN2                   
393400       MOVE 'VR5'                    TO POSTSUM-TRANSTYP                  
393500                                                                          
393600       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
393700                                                                          
393800     END-IF                                                               
393900     .                                                                    
394000     EJECT                                                                
394100 M-SKAPA-HUVUD-POST7  SECTION.                                            
394200     MOVE 'M-SKAPA-HUVUD-POST7' TO WS-SEKTION                             
394300*      W33 INLEVERANSER USA / CANADA                                      
394400                                                                          
394500     IF IN-IDKOLLI NOT = ZERO                                             
394600       IF WS7-IDKOLLI  NOT = IN-IDKOLLI OR                                
394700          WS7-IDPRODNR NOT = IN-IDPRODNR                                  
394800*      FÖR VARJE KOLLI                                                    
394900         IF IN-IDKOLLI NOT = KOLLI-IDKOLLI OR                             
395000            IN-IDPRODNR NOT = VORD-IDPRODNR                               
395100           MOVE IN-IDPRODNR         TO W-IDPRODNR-E6                      
395200           PERFORM IMS-GU-WDE601                                          
395300           MOVE IN-IDKOLLI          TO W-IDKOLLI-E6                       
395400           PERFORM IMS-GNP-WDE611                                         
395500         END-IF                                                           
395600       END-IF                                                             
395700     END-IF                                                               
395800     IF IN-IDSHIPM NOT = W-IDSHIPM                                        
395900       MOVE IN-IDSHIPM              TO W-IDSHIPM                          
396000       MOVE IN-IDDISTR              TO W-IDDISTR-E1                       
396100       MOVE IN-IDKUNDNR             TO W-IDKUNDNR-E1                      
396200       PERFORM IMS-GU-WDE111                                              
396300     ELSE                                                                 
396400       IF IN-IDDISTR NOT = SGMT-IDDISTR OR                                
396500          IN-IDKUNDNR NOT = SGMT-IDKUNDNR                                 
396600          MOVE IN-IDDISTR           TO W-IDDISTR-E1                       
396700          MOVE IN-IDKUNDNR          TO W-IDKUNDNR-E1                      
396800          PERFORM IMS-GU-WDE111                                           
396900       END-IF                                                             
397000     END-IF                                                               
397100*                                                                         
397200     .                                                                    
397300     EJECT                                                                
397400 M-SKAPA-RAD-POST7  SECTION.                                              
397500*    NDC GOODS RECEIVING, HISTORY, W33                                    
397600                                                                          
397700     IF IN-IDKOLLI = ZERO AND IN-IDPURAD = ZERO                           
397800       CONTINUE                                                           
397900     ELSE                                                                 
398000       IF IN-IDDC NOT = DCS-IDDC                                          
398100         MOVE IN-IDDC              TO W-IDDC-B6                           
398200         PERFORM IMS-GU-WDB601                                            
398300       END-IF                                                             
398400       MOVE IN-IDDISTR             TO TEST-IDDISTR                        
398500       IF ((DCS-CDC OR DCS-DDC) AND                                       
398600           (DIST07-USA-RETAILER OR DIST07-CAN-RETAILER))                  
398700                 OR                                                       
398800          (DCS-NDC-NA AND ORAD-FLDIRLEV = JA)                             
398900         MOVE 'W33'                  TO W33-IDPTYP                        
399000         MOVE ORAD-IDARTNR           TO W33-IDARTNR                       
399100         MOVE IN-IDDC                TO W33-IDDC-SEND                     
399200         IF DCS-NDC-NA                                                    
399300           MOVE IN-IDDC              TO W33-IDDC-REC                      
399400         ELSE                                                             
399500           IF DIST07-USA-RETAILER                                         
399600             MOVE WC-NDC-US-RU       TO W33-IDDC-REC                      
399700           ELSE                                                           
399800             MOVE WC-NDC-CA          TO W33-IDDC-REC                      
399900           END-IF                                                         
400000         END-IF                                                           
400100         MOVE IN-IDFAKT              TO W33-IDFAKT                        
400200         MOVE IN-DAFINDOC(3:6)       TO W33-TIFAKT                        
400300         MOVE IN-IDDISTR             TO W33-IDDISTR                       
400400         MOVE IN-IDKUNDNR            TO W33-IDKUNDNR                      
400500         MOVE ORAD-KDFRAKT           TO W33-KDFRAKT                       
400600         MOVE IN-KDVALISO-BET        TO W33-KDVALISO                      
400700         MOVE IN-PRKURS-BET          TO W33-PRKURS                        
400800         PERFORM S50-LAS-WDE111                                           
400900         MOVE SGMT-KDORDKL-MAX       TO W33-KDORDKL-MAX                   
401000         MOVE IN-IDORDNR7            TO W-FILL7                           
401100         MOVE W-FILL7(3:5)           TO W33-IDKUNDRF                      
401200         MOVE IN-IDKOLLI             TO W33-IDKOLLI                       
401300         MOVE KOLLI-KDKOLLI          TO W33-KDKOLLI                       
401400         MOVE KKOLLI-KVLEVART        TO W33-KVAVIS                        
401500         MOVE ORAD-PRARTNTO          TO W33-PRARTNTO                      
401600         IF DIST79-DEALER-PRICE                                           
401700           MOVE ORAD-PRARTNTO-LOC    TO W33-PRARTNTO                      
401800           IF IN-KDVALISO-FAKT = WS-KDVALISO-MC                           
401900             MOVE 1.0                TO W33-PRKURS                        
402000           END-IF                                                         
402100         END-IF                                                           
402200         MOVE ORAD-PRAVCOST        TO IN-PRAVCOST                         
402300**** AVCOST SHOULD BE SAVED IN WDE411 IN PROGRAM 4637                     
402400         IF IN-PRAVCOST = ZERO                                            
402500           PERFORM S40-HAMTA-PRAVCOST                                     
402600         END-IF                                                           
402700         MOVE IN-PRAVCOST            TO W33-PRAVCOST                      
402800                                                                          
402900         WRITE UT8-POST              FROM W33-AREA                        
403000                                                                          
403100         MOVE 'W47676'               TO POSTSUM-FDNAMN                    
403200         MOVE 'W47651D8'             TO POSTSUM-DDNAMN2                   
403300         MOVE 'W33'                  TO POSTSUM-TRANSTYP                  
403400                                                                          
403500         CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                    
403600       END-IF                                                             
403700     END-IF                                                               
403800     .                                                                    
403900     EJECT                                                                
404000 N-SKAPA-R33-DIRLEV-EXP  SECTION.                                         
404100     MOVE 'N-SKAPA-DIREKTLEV-R33'    TO WS-SEKTION                        
404200                                                                          
404300     IF ORAD-FLDIRLEV = JA                                                
404400       MOVE IN-IDDISTR               TO TEST-IDDISTR                      
404500       IF DIST35-NONVCC-NONVCC-REFILL                                     
404600       OR DIST35-NONVCC-NONVCC-TRANSFER                                   
404700         CONTINUE                                                         
404800       ELSE                                                               
404900         IF ORAD-IDBIL > SPACE AND ORAD-IDSYSTEM = 'VDI '                 
405000           MOVE +1                   TO WRAD-KDSOFT                       
405100         ELSE                                                             
405200           IF ORAD-IDSYSTEM = 'SOFT'                                      
405300             MOVE +2                 TO WRAD-KDSOFT                       
405400           ELSE                                                           
405500             MOVE ORAD-IDARTNR       TO TEST-ARTIKEL                      
405600             IF ART04-SOFTWARE                                            
405700               MOVE +3               TO WRAD-KDSOFT                       
405800             ELSE                                                         
405900               IF ORAD-IDSYSTEM = 'W371' OR 'W37A'                        
406000                 MOVE +4             TO WRAD-KDSOFT                       
406100               ELSE                                                       
406200                 MOVE +0             TO WRAD-KDSOFT                       
406300               END-IF                                                     
406400             END-IF                                                       
406500           END-IF                                                         
406600         END-IF                                                           
406700         IF WRAD-KDSOFT = +0                                              
406800           MOVE 'R33'                TO UTA-IDPTYP                        
406900           MOVE BILL-IDPRODNR        TO UTA-IDPRODNR                      
407000           MOVE BILL-IDKOLLI         TO UTA-IDKOLLI                       
407100           MOVE BILL-IDDC            TO UTA-IDDC                          
407200           MOVE ORAD-BEVOLREF        TO UTA-BEVOLREF                      
407300                                                                          
407400           PERFORM S18-SKRIV-W47663                                       
407500         END-IF                                                           
407600       END-IF                                                             
407700     ELSE                                                                 
407800       MOVE IN-IDDISTR               TO TEST-IDDISTR                      
407900       MOVE IN-IDDC                  TO WS-IDDC                           
408000       IF (DIST35-NONVCC-NONVCC-REFILL AND NOT DCS-CDC)                   
408100       OR (DIST35-NONVCC-NONVCC-TRANSFER AND NOT DCS-CDC)                 
408200*                                                                         
408300*        'R33' SKAPAS TILL INLEVERANS FÖR 'STUDS'DISTRIKTEN               
408400*        I FÖRSTA FLÖDET, LEV.DC --> 'STUDS'DC'T                          
408500*        EJ POST PÅ FILEN NÄR TILLÄGGSKOSTNADSPOST (IDKOLLI=0)            
408600*                                                                         
408700         IF BILL-IDKOLLI NOT = ZERO                                       
408800           MOVE 'R33'                TO UTA-IDPTYP                        
408900           MOVE BILL-IDPRODNR        TO UTA-IDPRODNR                      
409000           MOVE BILL-IDKOLLI         TO UTA-IDKOLLI                       
409100           MOVE BILL-IDDC            TO UTA-IDDC                          
409200           MOVE ORAD-BEVOLREF        TO UTA-BEVOLREF                      
409300                                                                          
409400           PERFORM S18-SKRIV-W47663                                       
409500         END-IF                                                           
409600       ELSE                                                               
409700*---     VOR FRÅN DC 11 TILL ETT ANNAT FÖRETAG (CN, IN)                   
409800*---     NDC (EJ FTG 57) TILL EN IMPORTÖR                                 
409900         IF IN-KDFAKSTA-EXP = 1                                           
410000           IF BILL-IDKOLLI NOT = ZERO                                     
410100             MOVE 'R33'              TO UTA-IDPTYP                        
410200             MOVE BILL-IDPRODNR      TO UTA-IDPRODNR                      
410300             MOVE BILL-IDKOLLI       TO UTA-IDKOLLI                       
410400             MOVE BILL-IDDC          TO UTA-IDDC                          
410500             MOVE ORAD-BEVOLREF      TO UTA-BEVOLREF                      
410600                                                                          
410700             PERFORM S18-SKRIV-W47663                                     
410800           END-IF                                                         
410900         END-IF                                                           
411000*---                                                                      
411100       END-IF                                                             
411200     END-IF                                                               
411300     .                                                                    
411400     EJECT                                                                
411500 P-SKAPA-RAD-POSTBYTES  SECTION.                                          
411600     MOVE 'P-SKAPA-RAD-POSTBYTES'     TO WS-SEKTION                       
411700                                                                          
411800     IF IN-IDPURAD = ZERO AND IN-IDKOLLI = ZERO                           
411900       CONTINUE                                                           
412000     ELSE                                                                 
412100                                                                          
412200      MOVE ORAD-IDARTNR                TO TEST-ARTIKEL                    
412300      IF (KORD-KDFAKTYP = 'R' OR 'G' OR 'N')                              
412400       AND BYT02-RENOV                                                    
412500       MOVE 'FAK'                     TO BYTES-IDPTYP                     
412600       MOVE IN-IDDISTR                TO BYTES-IDDISTR                    
412700                                         TEST-IDDISTR                     
412800       MOVE IN-IDKUNDNR               TO BYTES-IDKUNDNR                   
412900       MOVE ORAD-IDARTNR              TO BYTES-IDARTNR                    
413000       MOVE IN-IDDC                   TO BYTES-IDDC                       
413100       MOVE ORAD-IDKUNDRF-RO          TO WS-IDKUNDRF                      
413200       IF WS-IDKUNDRF-1-5 NOT = ZERO                                      
413300         MOVE WS-IDKUNDRF-1-5         TO BYTES-IDORDNR                    
413400       ELSE                                                               
413500         MOVE KORD-IDKUNDRF           TO WS-IDKUNDRF                      
413600         MOVE WS-IDKUNDRF-1-5         TO BYTES-IDORDNR                    
413700       END-IF                                                             
413800                                                                          
413900       MOVE KKOLLI-KVLEVART           TO BYTES-KVLEVART                   
414000       MOVE ORAD-FLINVEST             TO BYTES-FLINVEST                   
414100                                                                          
414200       WRITE BYTES-POST         FROM BYTES-UTAREA                         
414300       MOVE SPACE                     TO BYTES-UTAREA                     
414400                                                                          
414500       MOVE 'W4758M'                  TO POSTSUM-FDNAMN                   
414600       MOVE 'W47651DC'                TO POSTSUM-DDNAMN2                  
414700       MOVE 'BYTES'                   TO POSTSUM-TRANSTYP                 
414800                                                                          
414900       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
415000      END-IF                                                              
415100     END-IF                                                               
415200     .                                                                    
415300     EJECT                                                                
415400 Q-SKAPA-RAD-GREKLAND   SECTION.                                          
415500     MOVE 'Q-SKAPA-RAD-GREKLAND '     TO WS-SEKTION                       
415600                                                                          
415700     IF IN-IDDISTR = 1558 OR                                              
415800        IN-IDDISTR = 1578                                                 
415900       MOVE IN-IDDC                   TO GRK-IDDC                         
416000       MOVE IN-IDDISTR                TO GRK-IDDISTR                      
416100       MOVE IN-IDKUNDNR               TO GRK-IDKUNDNR                     
416200       MOVE IN-IDFAKT                 TO GRK-IDFAKT                       
416300       MOVE IN-DAFINDOC               TO GRK-DAFINDOC                     
416400       MOVE IN-IDSHIPM                TO GRK-IDSHIPM                      
416500       MOVE IN-TISKEPPN               TO GRK-TISKEPPN                     
416600       MOVE IN-IDKOLLI                TO GRK-IDKOLLI                      
416700       IF IN-IDKOLLI NOT = ZERO AND IN-IDPURAD NOT = ZERO                 
416800         MOVE ORAD-IDARTNR            TO GRK-IDARTNR                      
416900                                         W-IDARTNR                        
417000         PERFORM QA-ARTNR-TKN                                             
417100         PERFORM IMS-GU-WDK601                                            
417200         PERFORM IMS-GNP-WDK611                                           
417300         MOVE ORAD-IDKUNDRF-RO        TO WS-IDKUNDRF                      
417400         IF WS-IDKUNDRF-1-5 NOT = ZERO                                    
417500           MOVE WS-IDKUNDRF-1-5       TO GRK-IDORDNR7                     
417600         ELSE                                                             
417700           MOVE KORD-IDKUNDRF         TO WS-IDKUNDRF                      
417800           MOVE WS-IDKUNDRF-1-5       TO GRK-IDORDNR7                     
417900         END-IF                                                           
418000         MOVE ORAD-KDORDKL            TO GRK-KDORDKL                      
418100         MOVE ORAD-KDFRAKT            TO GRK-KDFRAKT                      
418200         MOVE ORAD-KDPRODSL           TO GRK-KDPRODSL                     
418300                                                                          
418400         MOVE ORAD-IDVIN              TO GRK-IDVIN                        
418500         MOVE KORD-IDORDER            TO W-IDORDER                        
418600         PERFORM IMS-GU-WDQ201                                            
418700         MOVE OHUV-BEKUNDRF           TO GRK-BEKUNDRF                     
418800                                                                          
418900         MOVE CLAG-KDARTURS           TO GRK-KDARTURS                     
419000         MOVE CLAG-IDSTATNR (GMT-KDSTATNR)                                
419100                                      TO GRK-IDSTATNR                     
419200         IF GRK-IDSTATNR = ZERO                                           
419300            MOVE GEN-IDSTATNR         TO GRK-IDSTATNR                     
419400         END-IF                                                           
419500                                                                          
419600         MOVE KKOLLI-KVLEVART         TO GRK-KVLEVART                     
419700         MOVE ORAD-VKARTNTO           TO GRK-VKARTNTO                     
419800       ELSE                                                               
419900         MOVE IN-IDORDNR7             TO GRK-IDORDNR7                     
420000         MOVE ZERO                    TO GRK-IDARTNR                      
420100                                         GRK-VKARTNTO                     
420200                                         GRK-IDSTATNR                     
420300                                         GRK-KDARTURS                     
420400                                         GRK-KDORDKL                      
420500                                         GRK-KDPRODSL                     
420600         MOVE SPACE                   TO GRK-IDARTNR-TKN                  
420700         MOVE +1                      TO GRK-KVLEVART                     
420800       END-IF                                                             
420900                                                                          
421000*---   SÄNDANDE DC:S MOMSRGNR SKA MED PÅ GREK-FILEN                       
421100       IF UT5-EKHT-IDDC-SEND = DCS-IDDC                                   
421200         MOVE DCS-IDVAT               TO GRK-IDVAT                        
421300       ELSE                                                               
421400         IF UT5-EKHT-IDDC-SEND NOT = SPACE                                
421500           MOVE UT5-EKHT-IDDC-SEND    TO W-IDDC-B6                        
421600           PERFORM IMS-GU-WDB601                                          
421700           MOVE DCS-IDVAT             TO GRK-IDVAT                        
421800         ELSE                                                             
421900           MOVE SPACE                   TO GRK-IDVAT                      
422000         END-IF                                                           
422100       END-IF                                                             
422200                                                                          
422300       MOVE ORAD-PRARTNTO             TO GRK-PRARTNTO                     
422400                                                                          
422500       WRITE GRK-POST               FROM GRK-UTAREA                       
422600       MOVE SPACE                     TO GRK-UTAREA                       
422700                                                                          
422800       MOVE 'W4765G'                  TO POSTSUM-FDNAMN                   
422900       MOVE 'W4765GDD'                TO POSTSUM-DDNAMN2                  
423000       MOVE 'GRK'                     TO POSTSUM-TRANSTYP                 
423100                                                                          
423200       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
423300     END-IF                                                               
423400     .                                                                    
423500     EJECT                                                                
423600 QA-ARTNR-TKN  SECTION.                                                   
423700                                                                          
423800     MOVE ORAD-IDARTNR          TO WS-NUM9                                
423900     MOVE WS-NUM9               TO WS-WIN                                 
424000     INSPECT WS-WIN REPLACING LEADING ZERO BY SPACE                       
424100     CALL W009REDU USING WS-WIN WS-WUT                                    
424200     MOVE WS-WUT                TO GRK-IDARTNR-TKN                        
424300     .                                                                    
424400     EJECT                                                                
424500 R-SKAPA-RAD-LEVANM  SECTION.                                             
424600     MOVE 'R-SKAPA-RAD-LEVANM'   TO WS-SEKTION                            
424700                                                                          
424800     IF IN-IDKOLLI = ZERO AND IN-IDPURAD = ZERO                           
424900       CONTINUE                                                           
425000     ELSE                                                                 
425100       IF WR-IDFAKT NOT = IN-IDFAKT                                       
425200          MOVE +1                  TO WS-IDRADNR                          
425300          MOVE IN-IDFAKT           TO WR-IDFAKT                           
425400       END-IF                                                             
425500                                                                          
425600       MOVE 'FAK'                  TO LEVAN-IDPTYP                        
425700       MOVE IN-IDDISTR             TO LEVAN-IDDISTR                       
425800       MOVE IN-IDKUNDNR            TO LEVAN-IDKUNDNR                      
425900       MOVE IN-IDFAKT              TO LEVAN-IDRAPPNR                      
426000       MOVE ORAD-IDARTNR           TO LEVAN-IDARTNR                       
426100                                                                          
426200       MOVE WS-IDRADNR             TO LEVAN-IDRADNR                       
426300                                                                          
426400       MOVE JA                     TO LEVAN-FLAUTKRE                      
426500       MOVE NEJ                    TO LEVAN-FLDIRLEV                      
426600       MOVE IN-IDFAKT              TO LEVAN-IDFAKT                        
426700       MOVE '57'                   TO LEVAN-IDFTG                         
426800       MOVE IN-IDKOLLI             TO LEVAN-IDKOLLI                       
426900       MOVE IN-IDORDNR7            TO W-FILL7                             
427000       MOVE W-FILL7(3:5)           TO LEVAN-IDKUNDRF                      
427100                                                                          
427200       IF DIST35-NA-CDC-QUAL-RETURN OR                                    
427300          DIST35-CN-CDC-RETUR-Q     OR                                    
427400          DIST35-IN-CDC-RETUR-Q     OR                                    
427500          DIST35-KR-CDC-RETUR-Q     OR                                    
427600          DIST35-AE-CDC-RETUR-Q     OR                                    
427700          DIST35-TR-CDC-RETUR-Q     OR                                    
427800          DIST35-MY-CDC-RETUR-Q     OR                                    
427900          DIST35-ZA-CDC-RETUR-Q                                           
428000         MOVE '54'                 TO LEVAN-KDANMORS                      
428100       ELSE                                                               
428200         IF DIST18-SCRAP-NDC-QUAL                                         
428300           MOVE '55'               TO LEVAN-KDANMORS                      
428400         ELSE                                                             
428500           IF DIST35-NA-CDC-BB-RETURN OR                                  
428600              DIST35-CDC-RETURNS-NON-VCC                                  
428700             MOVE '94'             TO LEVAN-KDANMORS                      
428800           END-IF                                                         
428900         END-IF                                                           
429000       END-IF                                                             
429100                                                                          
429200       MOVE WC-CDC-SE              TO LEVAN-IDDC                          
429300       MOVE ZERO                   TO LEVAN-KDEMBLEV                      
429400                                      LEVAN-IDLOPNRM                      
429500       MOVE KORD-KDFAKTYP          TO LEVAN-KDFAKTYP                      
429600       MOVE ORAD-KDFRAKT           TO LEVAN-KDFRAKT                       
429700       MOVE KKOLLI-KVLEVART        TO LEVAN-KVLEVANM                      
429800       MOVE ORAD-PRARTNTO          TO LEVAN-PRARTBTO                      
429900       MOVE ZERO                   TO LEVAN-PRARTBTO-LOC                  
430000       IF DIST79-DEALER-PRICE OR                                          
430100          DIST79-ECOM-PRICE                                               
430200         MOVE ORAD-PRARTNTO-LOC    TO LEVAN-PRARTBTO-LOC                  
430300       END-IF                                                             
430400       MOVE ORAD-KDVALISO          TO LEVAN-KDVALISO                      
430500       MOVE IN-DAFINDOC(3:6)       TO LEVAN-TIFAKT                        
430600       MOVE DAGENS-DATUM           TO LEVAN-TILEVANM                      
430700       PERFORM S03-HAMTA-KUND                                             
430800       MOVE BET-IDMARKBO           TO LEVAN-IDMARKBO                      
430900                                                                          
431000       WRITE LEVAN-POST    FROM LEVAN-UTAREA                              
431100                                                                          
431200       MOVE 'W47677'               TO POSTSUM-FDNAMN                      
431300       MOVE 'LEVANM  '             TO POSTSUM-DDNAMN2                     
431400       MOVE 'LEV'                  TO POSTSUM-TRANSTYP                    
431500                                                                          
431600       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
431700                                                                          
431800       ADD +1                      TO WS-IDRADNR                          
431900     END-IF                                                               
432000     .                                                                    
432100     EJECT                                                                
432200 S-SKAPA-HUVUD-POSTER8  SECTION.                                          
432300     MOVE 'S-SKAPA-HUVUD-POSTER8'    TO WS-SEKTION                        
432400                                                                          
432500     IF FL-SKRIV-95X = JA                                                 
432600       MOVE 1                    TO W-8D                                  
432700       PERFORM S19-SKRIV-W4768D                                           
432800     END-IF                                                               
432900                                                                          
433000     MOVE '95X'                   TO EK95X-IDPTYP                         
433100     MOVE KORD-KDFAKTYP           TO EK95X-KDFAKTYP                       
433200     MOVE IN-IDFAKT               TO EK95X-IDFAKT                         
433300     MOVE IN-IDDC                 TO EK95X-IDDC                           
433400     MOVE IN-IDDISTR              TO EK95X-IDDISTR                        
433500**   MOVE IN-IDKUNDNR             TO EK95X-IDKUNDNR                       
433600     MOVE ZERO                    TO EK95X-IDKUNDNR                       
433700     PERFORM S50-LAS-WDE111                                               
433800     MOVE SGMT-KDORDKL-MAX        TO EK95X-KDORDKL                        
433900     MOVE KORD-KDFRAKT            TO EK95X-KDFRAKT                        
434000     MOVE IN-SUNTO-TOT            TO EK95X-SUFKTBEL                       
434100     MOVE ZERO                    TO EK95X-PRFRAKT                        
434200                                     EK95X-PRFOERS                        
434300                                     EK95X-PREMBHNT                       
434400                                     EK95X-PRLEGKST                       
434500                                     EK95X-PRAVDRAG                       
434600                                     EK95X-PREXPKST                       
434700     MOVE IN-SUVAT-FAKT           TO EK95X-PRMOMS                         
434800     MOVE IN-SUNTO-TOT            TO EK95X-SUFAKTRE                       
434900     MOVE IN-SUNTO-TOT            TO EK95X-SUFKTUTL                       
435000     MOVE ORAD-IDKONTO            TO EK95X-IDKONTO-AVDRAG                 
435100     MOVE ORAD-IDKST              TO EK95X-IDKST                          
435200                                                                          
435300     MOVE ZERO                    TO EK95X-IDKONTO                        
435400     MOVE IN-DAFINDOC(1:8)        TO EK95X-DAFAKT                         
435500                                                                          
435600     PERFORM S03-HAMTA-KUNDA                                              
435700     MOVE BET-BEBETRAD-1          TO EK95X-BEKOPARE                       
435800     MOVE BET-BEBETRAD-2          TO EK95X-BEKOPARE(36:19)                
435900     MOVE BET-ADBETRAD-1          TO EK95X-ADKOPARE                       
436000     MOVE BET-ADBETRAD-2          TO EK95X-ADKOPARE(36:19)                
436100     IF WS-PRKURS = ZERO OR W-KDVALISO NOT = IN-KDVALISO-FAKT             
436200       MOVE IN-KDVALISO-FAKT     TO W-KDVALISO                            
436300       PERFORM S02-HAMTA-VALUTAKURS                                       
436400     END-IF                                                               
436500     MOVE WS-PRKURS               TO EK95X-PRKURS                         
436600*    IF DIST79-LOCAL-CURRENCY AND NOT DIST79-DEALER-PRICE                 
436700*      IF IN-KDVALISO-FAKT NOT = IN-KDVALISO-BET                          
436800*        CANADA (CIRKA 0.7593  CAD/USD)                                   
436900*        MOVE IN-PRKURS-FIKTIV    TO EK95X-PRKURS                         
437000*      ELSE                                                               
437100*        MOVE 1.0                 TO EK95X-PRKURS                         
437200*      END-IF                                                             
437300*    ELSE                                                                 
437400       MOVE IN-PRKURS-BET         TO EK95X-PRKURS                         
437500*    END-IF                                                               
437600     MOVE EK95X-PRKURS            TO WS-PRKURS                            
437700     MOVE IN-KDVALISO-FAKT        TO EK95X-KDVALISO                       
437800                                                                          
437900     MOVE BILL-TISKEPPN           TO EK95X-DASKEPPN(3:6)                  
438000     MOVE 20                      TO EK95X-DASKEPPN(1:2)                  
438100     MOVE SPACE                   TO EK95X-BEVARREF                       
438200     MOVE KORD-FLOVRLEV           TO EK95X-FLOVRLEV                       
438300     MOVE IN-IDDISTR              TO DIS1-IDDISTR                         
438400     CALL W460DIS1 USING DIS1-W460DIS1                                    
438500     MOVE DIS1-IDLANDX2           TO EK95X-IDLANDX2                       
438600                                                                          
438700     IF IN-SUVAT-FAKT > ZERO                                              
438800                                                                          
438900       COMPUTE EK95X-PRMOMS ROUNDED =                                     
439000                 IN-SUVAT-FAKT  * WS-PRKURS                               
439100                                                                          
439200       COMPUTE EK95X-SUFKTBEL =                                           
439300                 IN-SUBTO-TOT * WS-PRKURS                                 
439400                                                                          
439500     ELSE                                                                 
439600       MOVE ZERO      TO EK95X-PRMOMS                                     
439700     END-IF                                                               
439800     IF DIST79-DEALER-PRICE OR                                            
439900        DIST79-ECOM-PRICE                                                 
440000       IF IN-KDVALISO-FAKT NOT = IN-KDVALISO-BET                          
440100*        CANADA (CIRKA 0.7593  CAD/USD)                                   
440200         COMPUTE IN-SUBTO-TOT ROUNDED =                                   
440300                 IN-SUBTO-TOT / IN-PRKURS-FIKTIV                          
440400       END-IF                                                             
440500     ELSE                                                                 
440600       COMPUTE IN-SUBTO-TOT ROUNDED =                                     
440700               IN-SUBTO-TOT / IN-PRKURS-BET                               
440800     END-IF                                                               
440900     MOVE IN-SUBTO-TOT           TO EK95X-SUFKTUTL                        
441000     IF KORD-FLOVRLEV = JA                                                
441100       IF KORD-IDORDER NOT = OHUV-IDORDER                                 
441200          AND KORD-IDORDER NOT = ZERO                                     
441300         MOVE KORD-IDORDER           TO W-IDORDER                         
441400         PERFORM IMS-GU-WDQ201                                            
441500       END-IF                                                             
441600       MOVE OHUV-BEVARREF         TO EK95X-BEVARREF                       
441700     END-IF                                                               
441800                                                                          
441900     MOVE EK95X-IDDISTR           TO TEST-IDDISTR                         
442000                                                                          
442100*    IF KORD-FLOVRLEV = JA                                                
442200*        CONTINUE                                                         
442300*      ELSE                                                               
442400*        MOVE 1         TO W-8D                                           
442500*        PERFORM S19-SKRIV-W4768D                                         
442600*    END-IF                                                               
442700     MOVE JA                      TO FL-SKRIV-95X                         
442800     .                                                                    
442900     EJECT                                                                
443000 S-SKAPA-RAD-LAB      SECTION.                                            
443100     MOVE 'S-SKAPA-RAD-LAB'          TO WS-SEKTION                        
443200                                                                          
443300     IF IN-IDKOLLI = ZERO AND IN-IDPURAD = ZERO                           
443400       EVALUATE IN-BEART                                                  
443500         WHEN 'INSURANCE '                                                
443600           ADD  IN-SUNTO-LINE    TO EK95X-PRFOERS                         
443700           SUBTRACT IN-SUNTO-LINE FROM EK95X-SUFAKTRE                     
443800         WHEN 'FREIGHT   '                                                
443900           ADD  IN-SUNTO-LINE    TO EK95X-PRFRAKT                         
444000           SUBTRACT IN-SUNTO-LINE FROM EK95X-SUFAKTRE                     
444100         WHEN 'PACKING & HANDLING'                                        
444200           ADD  IN-SUNTO-LINE    TO EK95X-PREMBHNT                        
444300           SUBTRACT IN-SUNTO-LINE FROM EK95X-SUFAKTRE                     
444400         WHEN 'SERVICE FEE'                                               
444500           ADD  IN-SUNTO-LINE    TO EK95X-PREMBHNT                        
444600           SUBTRACT IN-SUNTO-LINE FROM EK95X-SUFAKTRE                     
444700         WHEN 'LEGAL     '                                                
444800           ADD  IN-SUNTO-LINE    TO EK95X-PRLEGKST                        
444900           SUBTRACT IN-SUNTO-LINE FROM EK95X-SUFAKTRE                     
445000         WHEN 'REDUCTION '                                                
445100           ADD  IN-SUNTO-LINE    TO EK95X-PRAVDRAG                        
445200           SUBTRACT IN-SUNTO-LINE FROM EK95X-SUFAKTRE                     
445300        END-EVALUATE                                                      
445400     ELSE                                                                 
445500       MOVE '94X'                TO EK94X-IDPTYP                          
445600       MOVE KORD-KDFAKTYP        TO EK94X-KDFAKTYP                        
445700       MOVE IN-IDFAKT            TO EK94X-IDFAKT                          
445800       MOVE IN-IDKUNDNR          TO EK94X-IDDEALER                        
445900       MOVE IN-IDDC              TO EK94X-IDDC                            
446000       MOVE IN-IDDISTR           TO EK94X-IDDISTR                         
446100       MOVE IN-IDKUNDNR          TO EK94X-IDKUNDNR                        
446200       MOVE ORAD-IDARTNR         TO EK94X-IDARTNR                         
446300       MOVE KORD-FLLSBOK         TO EK94X-FLLSBOK                         
446400       IF KORD-FLORDSPE     = NEJ                                         
446500         MOVE ORAD-FLDIRLEV      TO EK94X-FLDIRLEV                        
446600       ELSE                                                               
446700         MOVE NEJ                TO EK94X-FLDIRLEV                        
446800       END-IF                                                             
446900       MOVE KKOLLI-KVLEVART      TO EK94X-KVLEVART                        
447000       MOVE ORAD-PRARTNTO        TO EK94X-PRARTNTO                        
447100       IF DIST79-DEALER-PRICE OR                                          
447200          DIST79-ECOM-PRICE                                               
447300         MOVE ORAD-PRARTNTO-LOC  TO EK94X-PRARTNTO                        
447400       END-IF                                                             
447500                                                                          
447600       COMPUTE EK94X-SUARTNTO    =  KKOLLI-KVLEVART *                     
447700                                    EK94X-PRARTNTO                        
447800                                                                          
447900       MOVE ORAD-IDKONTO         TO EK94X-IDKONTO                         
448000       MOVE ORAD-IDKST           TO EK94X-IDKST                           
448100       MOVE ORAD-KDORDTYP        TO EK94X-KDORDTYP                        
448200       MOVE SPACE                TO EK94X-IDKUNDRF                        
448300       MOVE IN-IDORDNR7          TO W-FILL7                               
448400       MOVE W-FILL7(3:5)         TO EK94X-IDKUNDRF                        
448500       MOVE ORAD-PRAVCOST        TO IN-PRAVCOST                           
448600**** AVCOST SHOULD BE SAVED IN WDE411 IN PROGRAM 4637                     
448700       IF IN-PRAVCOST = ZERO                                              
448800         PERFORM S40-HAMTA-PRAVCOST                                       
448900       END-IF                                                             
449000       MOVE IN-PRAVCOST          TO EK94X-PRAVCOST                        
449100       MOVE ORAD-BERADREF        TO EK94X-BERADREF                        
449200                                                                          
449300       MOVE EK94X-IDDISTR        TO TEST-IDDISTR                          
449400                                                                          
449500       MOVE 2                    TO W-8D                                  
449600       PERFORM S19-SKRIV-W4768D                                           
449700     END-IF                                                               
449800     .                                                                    
449900     EJECT                                                                
450000 T-SKAPA-RAD-SAUDARABIEN  SECTION.                                        
450100     MOVE 'T-SKAPA-RAD-SAUDIARAB '    TO WS-SEKTION                       
450200                                                                          
450300     MOVE IN-IDDISTR                  TO TEST-IDDISTR                     
450400     IF DIST74-DUBAI-TRADING AND                                          
450500       (KORD-KDFAKTYP = 'R' OR 'G') AND                                   
450600        ORAD-KDFRAKT = 43                                                 
450700       MOVE IN-IDDISTR                TO DUB-IDDISTR                      
450800       MOVE IN-IDFAKT                 TO DUB-IDFAKT                       
450900       MOVE IN-DAFINDOC               TO DUB-TIFAKT                       
451000       MOVE IN-IDKOLLI                TO DUB-IDKOLLI                      
451100       IF IN-IDKOLLI NOT = ZERO AND IN-IDPURAD NOT = ZERO                 
451200         MOVE ORAD-IDARTNR            TO DUB-IDARTNR                      
451300                                         W-IDARTNR                        
451400         PERFORM IMS-GU-WDK601                                            
451500         MOVE ART-REKSIFFR            TO DUB-REKSIFFR                     
451600         MOVE ORAD-IDKUNDRF-RO        TO WS-IDKUNDRF                      
451700         IF WS-IDKUNDRF-1-5 NOT = ZERO                                    
451800           MOVE WS-IDKUNDRF-1-5       TO DUB-IDORDNR                      
451900         ELSE                                                             
452000           MOVE KORD-IDKUNDRF         TO WS-IDKUNDRF                      
452100           MOVE WS-IDKUNDRF-1-5       TO DUB-IDORDNR                      
452200         END-IF                                                           
452300         MOVE ORAD-BEVOLREF           TO DUB-BEVOLREF                     
452400         MOVE ORAD-BEART              TO DUB-BEART                        
452500         MOVE KKOLLI-KVLEVART         TO DUB-KVLEVART                     
452600       ELSE                                                               
452700         MOVE IN-IDORDNR7             TO DUB-IDORDNR                      
452800         MOVE ZERO                    TO DUB-IDARTNR                      
452900         MOVE +1                      TO DUB-KVLEVART                     
453000       END-IF                                                             
453100       MOVE ORAD-PRARTNTO             TO DUB-PRARTNTO                     
453200       IF DIST79-DEALER-PRICE OR                                          
453300          DIST79-ECOM-PRICE                                               
453400         MOVE ORAD-PRARTNTO-LOC       TO DUB-PRARTNTO                     
453500       END-IF                                                             
453600       MOVE ORAD-KDVALISO             TO DUB-KDVALISO                     
453700                                                                          
453800       WRITE DUB-POST               FROM DUB-UTAREA                       
453900       MOVE SPACE                     TO DUB-UTAREA                       
454000                                                                          
454100       MOVE 'W4768Z'                  TO POSTSUM-FDNAMN                   
454200       MOVE 'W4768ZDF'                TO POSTSUM-DDNAMN2                  
454300       MOVE 'DUB'                     TO POSTSUM-TRANSTYP                 
454400                                                                          
454500       CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                      
454600     END-IF                                                               
454700     .                                                                    
454800     EJECT                                                                
454900 Z-FINIT SECTION.                                                         
455000     MOVE 'Z-FINIT'    TO WS-SEKTION                                      
455100                                                                          
455200     IF WS2-IDFAKT NOT = ZERO                                             
455300        PERFORM S22-SKRIV-RIL                                             
455400        PERFORM S20-NOLLA-RIL                                             
455500     END-IF                                                               
455600                                                                          
455700     IF RIM-POST                                                          
455800        PERFORM FA-SKRIV-RIM-POST                                         
455900     END-IF                                                               
456000                                                                          
456100     IF FL-SKRIV-VR1 = JA                                                 
456200        WRITE VR-POST-1 FROM VR1-UTAREA                                   
456300        MOVE SPACE                 TO VR-DATA-AREA                        
456400                                                                          
456500        MOVE 'W47662'              TO POSTSUM-FDNAMN                      
456600        MOVE 'VR      '            TO POSTSUM-DDNAMN2                     
456700        MOVE 'VR1'                 TO POSTSUM-TRANSTYP                    
456800        CALL POSTSUM USING POSTSUM-PARM POSTSUM-PARM2                     
456900                                                                          
457000     END-IF                                                               
457100                                                                          
457200                                                                          
457300     IF FL-SKRIV-95X = JA                                                 
457400       MOVE 1                    TO W-8D                                  
457500       PERFORM S19-SKRIV-W4768D                                           
457600     END-IF                                                               
457700                                                                          
457800     CLOSE W47650                                                         
457900           W47653                                                         
458000           W47654                                                         
458100           W47658                                                         
458200           W47660                                                         
458300           W47663                                                         
458400           W47662                                                         
458500           W47676                                                         
458600           W47677                                                         
458700           W4768M                                                         
458800           W4765G                                                         
458900           W4768D                                                         
459000           W4768Z                                                         
459100           W4768E                                                         
459200           W4768F                                                         
459300           W4768G                                                         
459400     SKIP2                                                                
459500     MOVE 'S' TO POSTSUM-OPKOD                                            
459600     CALL POSTSUM USING POSTSUM-PARM                                      
459700     .                                                                    
459800     EJECT                                                                
459900 XX-FIXE411  SECTION.                                                     
460000*    IF ORAD-PRARTNTO-LOC > ORAD-PRARTBTO-LOC                             
460100*      AND ORAD-PRARTBTO-LOC NOT = ZERO                                   
460200*      MOVE ORAD-PRARTNTO-LOC  TO X4-PRARTNTO                             
460300*      MOVE ORAD-PRARTBTO-LOC  TO X4-PRARTBTO                             
460400*      MOVE X4-PRARTBTO        TO ORAD-PRARTNTO-LOC                       
460500*      MOVE X4-PRARTNTO        TO ORAD-PRARTBTO-LOC                       
460600*    END-IF                                                               
460700**FIX  FÖR ATT TA HAND OM GROSS < NET PRISER ***                          
460800       IF DIST79-DEALER-PRICE                                             
460900         IF ORAD-PRARTNTO-LOC > ORAD-PRARTBTO-LOC                         
461000           MOVE ORAD-PRARTNTO-LOC TO ORAD-PRARTBTO-LOC                    
461100           MOVE ZERO              TO ORAD-RERAB                           
461200         END-IF                                                           
461300       END-IF                                                             
461400**END-FIX                                                                 
461500     .                                                                    
461600     EJECT                                                                
461700 S01-LAES-W47650  SECTION.                                                
461800     MOVE 'S01-LAES-47650'    TO WS-SEKTION                               
461900     READ W47650 INTO IN-AREA                                             
462000     AT END                                                               
462100        MOVE HIGH-VALUE TO IN-AREA                                        
462200        SET END-OF-W47650 TO TRUE                                         
462300                                                                          
462400     NOT AT END                                                           
462500        MOVE 'W47650'       TO POSTSUM-FDNAMN                             
462600        MOVE 'W47651D1'     TO POSTSUM-DDNAMN2                            
462700        MOVE SPACE          TO POSTSUM-TRANSTYP                           
462800        CALL POSTSUM USING POSTSUM-PARM                                   
462900     END-READ                                                             
463000     .                                                                    
463100     EJECT                                                                
463200 S02-HAMTA-VALUTAKURS  SECTION.                                           
463300     MOVE 'S02-HAMTA-VALUTAKURS' TO WS-SEKTION                            
463400                                                                          
463500     MOVE ORAD-KDVALISO      TO W-KDVALISO                                
463600     IF W-KDVALISO = 'EUR' OR 'GBP' OR 'SEK'                              
463700       OR 'NOK' OR 'DKK' OR 'CHF'                                         
463800       OR 'USD' OR 'CAD'                                                  
463900       CONTINUE                                                           
464000     ELSE                                                                 
464100       MOVE 'SEK'       TO W-KDVALISO                                     
464200     END-IF                                                               
464300                                                                          
464400     MOVE IN-PRKURS-FAKT     TO WS-PRKURS                                 
464500                                EXCH-PRKURS                               
464600     .                                                                    
464700     SKIP2                                                                
464800 S02-BET-HAMTA-VALUTAKURS  SECTION.                                       
464900     MOVE 'S02-BET-HAMTA-VALUTAKURS' TO WS-SEKTION                        
465000                                                                          
465100     MOVE BET-KDVALISO       TO W-KDVALISO                                
465200     IF W-KDVALISO = 'EUR' OR 'GBP' OR 'SEK'                              
465300       OR 'NOK' OR 'DKK' OR 'CHF'                                         
465400       OR 'USD' OR 'CAD'                                                  
465500       CONTINUE                                                           
465600     ELSE                                                                 
465700       MOVE 'SEK'       TO W-KDVALISO                                     
465800     END-IF                                                               
465900                                                                          
466000     MOVE IN-PRKURS-BET      TO EXCH-PRKURS                               
466100                                WS-PRKURS                                 
466200     .                                                                    
466300     SKIP2                                                                
466400 S03-HAMTA-KUND  SECTION.                                                 
466500     MOVE 'S03-HAMTA-KUND'    TO WS-SEKTION                               
466600                                                                          
466700     IF IN-IDDISTR  NOT = GMT-IDDISTR                                     
466800     OR IN-IDKUNDNR NOT = GMT-IDKUNDNR                                    
466900       MOVE IN-IDDISTR             TO W-IDDISTR-B2                        
467000       MOVE IN-IDKUNDNR            TO W-IDKUNDNR-B2                       
467100       PERFORM IMS-GU-WDB201                                              
467200       MOVE GMT-IDFTG              TO WS-WDB2-IDFTG                       
467300     END-IF                                                               
467400     IF GMT-IDPARTNR NOT = BET-IDPARTNR                                   
467500     OR GMT-IDFTG    NOT = BET-IDFTG                                      
467600       MOVE GMT-IDPARTNR           TO W-IDPARTNR                          
467700       MOVE GMT-IDFTG              TO W-IDFTG                             
467800       PERFORM IMS-GU-WDB101                                              
467900     END-IF                                                               
468000     .                                                                    
468100     EJECT                                                                
468200 S03-HAMTA-KUNDA  SECTION.                                                
468300     MOVE 'S03-HAMTA-KUNDA'   TO WS-SEKTION                               
468400                                                                          
468500     IF IN-IDDISTR NOT = GMT-IDDISTR                                      
468600       MOVE IN-IDDISTR             TO W-IDDISTR-MIN-B2                    
468700       MOVE IN-IDDISTR             TO W-IDDISTR-MAX-B2                    
468800       PERFORM IMS-GU-WDB201A                                             
468900     END-IF                                                               
469000     IF GMT-IDPARTNR NOT = BET-IDPARTNR                                   
469100     OR GMT-IDFTG    NOT = BET-IDFTG                                      
469200       MOVE GMT-IDPARTNR           TO W-IDPARTNR                          
469300       MOVE GMT-IDFTG              TO W-IDFTG                             
469400       PERFORM IMS-GU-WDB101                                              
469500     END-IF                                                               
469600     MOVE GMT-IDFTG                TO WS-WDB2-IDFTG                       
469700     .                                                                    
469800     EJECT                                                                
469900                                                                          
470000 S03-HAMTA-KUNDB  SECTION.                                                
470100     MOVE 'S03-HAMTA-KUNDB'   TO WS-SEKTION                               
470200                                                                          
470300     MOVE IN-IDDISTR             TO W-IDDISTR-MIN-B2                      
470400     MOVE IN-IDDISTR             TO W-IDDISTR-MAX-B2                      
470500     MOVE ZERO                   TO W-IDKUNDNR-MIN-B2                     
470600     MOVE 999999                 TO W-IDKUNDNR-MAX-B2                     
470700     PERFORM IMS-GU-WDB201A                                               
470800     IF SEGMENT-FINNS                                                     
470900       MOVE GMT-IDPARTNR           TO W-IDPARTNR                          
471000       MOVE GMT-IDFTG              TO W-IDFTG                             
471100       PERFORM IMS-GU-WDB101                                              
471200     END-IF                                                               
471300     MOVE GMT-IDFTG                TO WS-WDB2-IDFTG                       
471400     .                                                                    
471500     EJECT                                                                
471600 S03-HAMTA-KUNDC SECTION.                                                 
471700     MOVE 'S03-HAMTA-KUNDC'   TO WS-SEKTION                               
471800                                                                          
471900     IF IN-IDDISTR  NOT = GMT-IDDISTR                                     
472000     OR IN-IDKUNDNR NOT = GMT-IDKUNDNR                                    
472100       MOVE IN-IDDISTR             TO W-IDDISTR-B2                        
472200       MOVE IN-IDDC                TO W-IDKUNDNR-B2                       
472300       PERFORM IMS-GU-WDB201                                              
472400       MOVE GMT-IDFTG              TO WS-WDB2-IDFTG                       
472500     END-IF                                                               
472600     IF GMT-IDPARTNR NOT = BET-IDPARTNR                                   
472700     OR GMT-IDFTG    NOT = BET-IDFTG                                      
472800       MOVE GMT-IDPARTNR           TO W-IDPARTNR                          
472900       MOVE GMT-IDFTG              TO W-IDFTG                             
473000       PERFORM IMS-GU-WDB101                                              
473100     END-IF                                                               
473200     .                                                                    
473300     EJECT                                                                
473400                                                                          
473500 S04-OMVANDLA-SEK-TILL-UTL  SECTION.                                      
473600     MOVE 'S04-OMVANDLA-SEK'  TO WS-SEKTION                               
473700**EO   +1 KDCALL = UTL TILL SEK  VALUTA                                   
473800*      +2 KDCALL = SEK TILL UTL  VALUTA                                   
473900*      +2        ÄVEN FÖR EUR TILL GBP                                    
474000     MOVE +2                 TO EXCH-KDCALL                               
474100     MOVE +0                 TO EXCH-PRARTNTO-IN                          
474200     MOVE WS-BEL             TO EXCH-SUORDV-IN                            
474300                                                                          
474400     CALL W411EXCH USING EXCH-W411EXCH                                    
474500                                                                          
474600     MOVE EXCH-SUORDV-UT     TO WS-BEL                                    
474700     .                                                                    
474800     EJECT                                                                
474900 S01-OMVANDLA-TILL-SEK SECTION.                                           
475000     MOVE 'S01-OMVANDLA-TILL-SEK' TO WS-SEKTION                           
475100     MOVE BILL-PRKURS-FAKT     TO EXCH-PRKURS                             
475200**EO   +1 KDCALL =  LOKAL VALUTA TILL SEK                                 
475300     MOVE +1                 TO EXCH-KDCALL                               
475400     MOVE +0                 TO EXCH-SUORDV-IN                            
475500     MOVE UT3-PRARTNTO       TO EXCH-PRARTNTO-IN                          
475600                                                                          
475700     CALL W411EXCH USING EXCH-W411EXCH                                    
475800                                                                          
475900     MOVE EXCH-PRARTNTO-UT   TO UT3-PRARTNTO                              
476000     .                                                                    
476100     EJECT                                                                
476200 S05-BUILD-COMMON SECTION.                                                
476300     MOVE  'S05-BUILD-COMMON'  TO WS-SEKTION                              
476400                                                                          
476500******************************************************************        
476600*                                                                         
476700*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
476800*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
476900*                                                                         
477000******************************************************************        
477100                                                                          
477200     MOVE WS-IDDISTR       TO W-TP4TRAN-IDDISTR                           
477300                                                                          
477400     PERFORM DB2-SELECT-TP4TRAN                                           
477500                                                                          
477600     MOVE SPACE                TO WS-IDKST                                
477700     MOVE SPACE                TO UT5-EKHT-IDLEVNR                        
477800     MOVE WS-IDDISTR           TO TEST-IDDISTR                            
477900     EVALUATE KORD-KDFAKTYP                                               
478000                                                                          
478100     WHEN 'G'                                                             
478200       MOVE '201'              TO UT5-EKHT-KDEKHHT                        
478300       IF DIST35-NL-SITTARD-OBJEKT                                        
478400         MOVE '202'            TO UT5-EKHT-KDEKSHT                        
478500       ELSE                                                               
478600         MOVE '201'            TO UT5-EKHT-KDEKSHT                        
478700       END-IF                                                             
478800                                                                          
478900     WHEN 'K'                                                             
479000       IF (DIST35-REFILL AND NOT DIST35-REFILL-NA                         
479100                         AND NOT DIST35-CDC-NONVCC-REFILL)                
479200       OR  DIST35-REFILL-INOM-NONVCC-NDC                                  
479300       OR  DIST35-REFILL-INOM-JP                                          
479400       OR  DIST35-PACIFIC-TRANSFER                                        
479500       OR  RADER-FINNS                                                    
479600         MOVE '501'            TO UT5-EKHT-KDEKHHT                        
479700         MOVE '501'            TO UT5-EKHT-KDEKSHT                        
479800       ELSE                                                               
479900         IF DIST35-RETUR                                                  
480000         OR DIST35-TH-NDC63-RETUR                                         
480100         OR DIST35-CN-NDC-RETURNS                                         
480200         OR DIST35-CN-TRANSFER                                            
480300           MOVE '501'            TO UT5-EKHT-KDEKHHT                      
480400           MOVE '502'            TO UT5-EKHT-KDEKSHT                      
480500         ELSE                                                             
480600           IF DIST28-CDC-COMPOUND                                         
480700             MOVE '505'            TO UT5-EKHT-KDEKHHT                    
480800             MOVE '501'            TO UT5-EKHT-KDEKSHT                    
480900           ELSE                                                           
481000             IF DIST77-SUPPLIERS                                          
481100               MOVE '202'        TO UT5-EKHT-KDEKHHT                      
481200               MOVE '204'        TO UT5-EKHT-KDEKSHT                      
481300             ELSE                                                         
481400               MOVE '202'          TO UT5-EKHT-KDEKHHT                    
481500               MOVE '201'          TO UT5-EKHT-KDEKSHT                    
481600             END-IF                                                       
481700           END-IF                                                         
481800         END-IF                                                           
481900       END-IF                                                             
482000                                                                          
482100     WHEN 'N'                                                             
482200       IF DIST18-SKROT OR DIST18-SCRAP-NDC                                
482300         MOVE SPACE TO WS-IDKUNDNR-CHECK                                  
482400         MOVE IN-IDKUNDNR TO WS-IDKUNDNR                                  
482500         MOVE WS-IDKUNDNR TO WS-IDKUNDNR-CHECK                            
482600         INSPECT WS-IDKUNDNR-CHECK REPLACING LEADING ZERO BY SPACE        
482700         IF DIST18-SKROT-KVAL-CDC                                         
482800           IF WS-IDKUNDNR-CHECK = '     71'                               
482900             MOVE '102'          TO UT5-EKHT-KDEKHHT                      
483000             MOVE '122'          TO UT5-EKHT-KDEKSHT                      
483100             MOVE 'CHN07'        TO UT5-EKHT-IDLEVNR                      
483200           ELSE                                                           
483300             MOVE '404'          TO UT5-EKHT-KDEKHHT                      
483400             MOVE '401'          TO UT5-EKHT-KDEKSHT                      
483500           END-IF                                                         
483600         ELSE                                                             
483700           MOVE '404'          TO UT5-EKHT-KDEKHHT                        
483800           MOVE '401'          TO UT5-EKHT-KDEKSHT                        
483900         END-IF                                                           
484000       ELSE                                                               
484100         IF DIST19-SATS                                                   
484200           MOVE '406'          TO UT5-EKHT-KDEKHHT                        
484300           MOVE '401'          TO UT5-EKHT-KDEKSHT                        
484400         ELSE                                                             
484500           MOVE '203'          TO UT5-EKHT-KDEKHHT                        
484600           MOVE '201'          TO UT5-EKHT-KDEKSHT                        
484700         END-IF                                                           
484800       END-IF                                                             
484900                                                                          
485000     WHEN 'R'                                                             
485100       IF DIST35-NONVCC-REFILL                                            
485200       OR DIST35-NONVCC-NONVCC-TRANSFER                                   
485300       OR DIST35-NONVCC-VCC-TRANSFER                                      
485400**** FIRST POST IS CREATE HERE FOR EXPORT FLOW                            
485500         MOVE '204'            TO UT5-EKHT-KDEKHHT                        
485600         IF DIST35-NONVCC-CDC-REFILL                                      
485700         OR DIST35-NONVCC-VCC-REFILL                                      
485800         OR DIST35-NONVCC-VCC-TRANSFER                                    
485900         OR (DIST35-NONVCC-NONVCC-REFILL AND NOT DCS-CDC)                 
486000         OR (DIST35-NONVCC-NONVCC-TRANSFER AND NOT DCS-CDC)               
486100**** FIRST INVOICE FOR EXPORT BOUNCE FLOW OR SINGLE FLOW                  
486200           MOVE '301'          TO UT5-EKHT-KDEKSHT                        
486300           MOVE '1441'         TO UT5-EKHT-IDLEVNR                        
486400         END-IF                                                           
486500         IF (DIST35-NONVCC-NONVCC-REFILL AND DCS-CDC)                     
486600         OR (DIST35-NONVCC-NONVCC-TRANSFER AND DCS-CDC)                   
486700**** SECOND INVOICE FOR EXPORT BOUNCE FLOW                                
486800           MOVE '302'          TO UT5-EKHT-KDEKSHT                        
486900           MOVE IN-IDPARTNR    TO UT5-EKHT-IDLEVNR                        
487000**** WE NEED CDC STD PRIS                                                 
487100           MOVE ORAD-IDARTNR         TO W-IDARTNR                         
487200           PERFORM IMS-GU-WDK601                                          
487300           PERFORM IMS-GNP-WDK611                                         
487400           MOVE CLAG-PRARTSTD  TO UT5-EKHT-PRARTSTD                       
487500         END-IF                                                           
487600       ELSE                                                               
487700         MOVE '204'            TO UT5-EKHT-KDEKHHT                        
487800***** EXTENDED WARRANTY SHOULD BE OWN EVENT                               
487900         MOVE ORAD-IDARTNR         TO W-IDARTNR                           
488000         PERFORM IMS-GU-WDK601                                            
488100         PERFORM IMS-GNP-WDK611                                           
488200         MOVE ART-IDFKNGRP TO TEST-IDFKNGRP                               
488300         MOVE '201'                TO UT5-EKHT-KDEKSHT                    
488400         IF FKNGRP-VOC-PART                                               
488500***** FUNCTION GRP 3988 SHOULD HAVE DIFFERENT PROFIT CENTER               
488600           MOVE 'VOCEXT'           TO WS-IDKST                            
488700         END-IF                                                           
488800         IF FKNGRP-EXT-WARRANTY OR FKNGRP-VSA-IMP                         
488900***** EXTENDED WARRANTY SHOULD BE OWN EVENT                               
489000           IF FKNGRP-EXT-WARRANTY                                         
489100             MOVE '205'            TO UT5-EKHT-KDEKSHT                    
489200           END-IF                                                         
489300***** VOLVO SERVICE AGREMENT SHOULD BE OWN EVENT                          
489400           IF FKNGRP-VSA-IMP                                              
489500             MOVE '206'            TO UT5-EKHT-KDEKSHT                    
489600           END-IF                                                         
489700         END-IF                                                           
489800         IF DIST79-ECOM-PRICE                                             
489900***** ECOM SALES SHOULD BE OWN EVENT                                      
490000           MOVE '208'              TO UT5-EKHT-KDEKSHT                    
490100         END-IF                                                           
490200**** SECOND INVOICE GLOBAL EXPORT IMPORTER BOUNCE FLOW                    
490300         IF GMT-KDKUNDKAT = 04                                            
490400           IF IN-KDFAKSTA-EXP = 2                                         
490500             MOVE '303'      TO UT5-EKHT-KDEKSHT                          
490600             MOVE '1441'     TO UT5-EKHT-IDLEVNR                          
490700           END-IF                                                         
490800****   FIRST INVOICE GLOBAL EXPORT IMPORTER BOUNCE FLOW                   
490900           IF IN-KDFAKSTA-EXP = 1                                         
491000             MOVE '301'      TO UT5-EKHT-KDEKSHT                          
491100             MOVE '1441'     TO UT5-EKHT-IDLEVNR                          
491200           END-IF                                                         
491300         END-IF                                                           
491400       END-IF                                                             
491500                                                                          
491600     WHEN OTHER                                                           
491700       MOVE '2??'              TO UT5-EKHT-KDEKHHT                        
491800       MOVE '2??'              TO UT5-EKHT-KDEKSHT                        
491900                                                                          
492000     END-EVALUATE                                                         
492100                                                                          
492200     MOVE 'W4765100'           TO UT5-EKHT-IDPGM                          
492300     MOVE FUNCTION CURRENT-DATE (1:8) TO UT5-EKHT-DAREGDAT                
492400     MOVE IN-DAFINDOC                 TO UT5-EKHT-DAVERDAT                
492500     MOVE FUNCTION CURRENT-DATE (9:8) TO UT5-EKHT-TIKLOCK                 
492600     MOVE 1                    TO UT5-EKHT-IDSEKVNR                       
492700     MOVE SPACE                TO UT5-EKHT-IDDC-SEND                      
492800     MOVE SPACE                TO UT5-EKHT-FLDCET                         
492900     MOVE SPACE                TO UT5-EKHT-IDKUNDRF                       
493000     MOVE SPACE                TO UT5-EKHT-IDFAKT-EXP                     
493100                                                                          
493200     IF DIST35-RETUR                                                      
493300     OR DIST35-NA-CDC-RETURN                                              
493400     OR DIST35-CDC-RETURNS-NON-VCC                                        
493500     OR DIST35-NONVCC-CDC-REFILL                                          
493600     OR DIST35-NONVCC-VCC-REFILL                                          
493700     OR DIST35-NONVCC-VCC-TRANSFER                                        
493800       IF DIST35-JP-NDC-RETURNS                                           
493900       OR DIST35-NDCCN-JP-REFILL                                          
494000       OR DIST35-NDCUS-JP-REFILL                                          
494100       OR DIST35-NDCTH-JP-REFILL                                          
494200       OR DIST35-NONVCC-JP-TRANSFER                                       
494300         MOVE WC-NDC-JP-61       TO UT5-EKHT-IDDC-REC                     
494400       ELSE                                                               
494500         IF DIST35-NDCCN-AU-REFILL                                        
494600         OR DIST35-NDCUS-AU-REFILL                                        
494700         OR DIST35-NDCTH-AU-REFILL                                        
494800         OR DIST35-NONVCC-AU-TRANSFER                                     
494900           MOVE WC-NDC-AU          TO UT5-EKHT-IDDC-REC                   
495000         ELSE                                                             
495100           MOVE WC-CDC-SE          TO UT5-EKHT-IDDC-REC                   
495200         END-IF                                                           
495300       END-IF                                                             
495400     ELSE                                                                 
495500******************************************************************        
495600*                                                                         
495700*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
495800*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
495900*                                                                         
496000******************************************************************        
496100       MOVE WS-IDDISTR        TO W-TP4TRAN-IDDISTR                        
496200                                                                          
496300       PERFORM DB2-SELECT-TP4TRAN                                         
496400                                                                          
496500       IF RADER-FINNS                                                     
496600         MOVE TP4TRAN-IDDC-REC   TO UT5-EKHT-IDDC-REC                     
496700       ELSE                                                               
496800**** BOUNCE FLOW FIRST INVOICE WILL STOP AT CDC                           
496900         IF (DIST35-NONVCC-NONVCC-REFILL AND NOT DCS-CDC)                 
497000         OR (DIST35-NONVCC-NONVCC-TRANSFER AND NOT DCS-CDC)               
497100           MOVE WC-CDC-SE          TO UT5-EKHT-IDDC-REC                   
497200         ELSE                                                             
497300           IF DIST35-VCC-NONVCC-TRANSFER                                  
497400             MOVE SPACE TO WS-IDKUNDNR-CHECK                              
497500             MOVE IN-IDKUNDNR TO WS-IDKUNDNR                              
497600             MOVE WS-IDKUNDNR TO WS-IDKUNDNR-CHECK                        
497700             MOVE WS-IDKUNDNR-CHECK(6:2) TO UT5-EKHT-IDDC-REC             
497800           ELSE                                                           
497900             SEARCH ALL DIST57-REFILL-DC                                  
498000              AT END                                                      
498100                MOVE SPACE     TO UT5-EKHT-IDDC-REC                       
498200              WHEN DIST57-SOK-IDDISTR(DIST57-IX) = WS-IDDISTR             
498300                MOVE DIST57-REFILL-TO-DC(DIST57-IX)                       
498400                                     TO UT5-EKHT-IDDC-REC                 
498500             END-SEARCH                                                   
498600           END-IF                                                         
498700         END-IF                                                           
498800       END-IF                                                             
498900     END-IF                                                               
499000                                                                          
499100     MOVE IN-IDDISTR           TO UT5-EKHT-IDDISTR                        
499200     MOVE IN-IDKUNDNR          TO UT5-EKHT-IDKUNDNR                       
499300                                                                          
499400     MOVE 'VO'                 TO CIA-IDARTPRE-IN                         
499500     MOVE WS-IDFAKT            TO CIA-IDARTBET-IN                         
499600     CALL W009CIA USING           CIA-W009CIA                             
499700     MOVE CIA-IDARTBET-UT      TO UT5-EKHT-IDVERGL                        
499800                                                                          
499900     MOVE IN-KDVALISO-FAKT     TO UT5-EKHT-KDVALISO                       
500000     MOVE WS-PRKURS            TO UT5-EKHT-PRKURS                         
500100     MOVE KORD-FLOVRLEV        TO UT5-EKHT-FLOVRLEV                       
500200     MOVE 'W510EKHA'           TO UT5-EKHT-IDCPYTXT                       
500300     MOVE ZERO                 TO UT5-EKHT-KDPRODSL                       
500400                                  UT5-EKHT-KDPSLLOC                       
500500                                  UT5-EKHT-IDARTNR                        
500600                                  UT5-EKHT-PRARTNTO                       
500700                                  UT5-EKHT-PRARTSJK                       
500800                                  UT5-EKHT-PRARTSTD                       
500900                                  UT5-EKHT-PRHEMTAG                       
501000                                  UT5-EKHT-PRLANDCO                       
501100                                  UT5-EKHT-PRINK                          
501200                                  UT5-EKHT-PRDIRLON                       
501300                                  UT5-EKHT-PRDMTRL                        
501400                                  UT5-EKHT-PROVRPAL                       
501500                                  UT5-EKHT-KVANTAL                        
501600                                  UT5-EKHT-SUBEL                          
501700                                  UT5-EKHT-BEVAT                          
501800                                  UT5-EKHT-IDKONTO                        
501900                                  UT5-EKHT-KDANMORS                       
502000                                  UT5-EKHT-KDFRAKT                        
502100                                  UT5-EKHT-SUVAT                          
502200                                  UT5-EKHT-IDORDNR5                       
502300     MOVE SPACE                TO UT5-EKHT-IDANALYS                       
502400                                  UT5-EKHT-FLLSBOK                        
502500                                  UT5-EKHT-IDTRANS                        
502600                                  UT5-EKHT-IDUSER                         
502700     IF WS-IDKST NOT = SPACES                                             
502800       MOVE WS-IDKST           TO UT5-EKHT-IDKST                          
502900     ELSE                                                                 
503000       MOVE SPACES             TO UT5-EKHT-IDKST                          
503100     END-IF                                                               
503200     MOVE ZERO                 TO UT5-EKHT-DAAVIDAT                       
503300                                  UT5-EKHT-IDAVINR                        
503400                                  UT5-EKHT-KDAVVTYP                       
503500                                  UT5-EKHT-KDRT                           
503600                                  UT5-EKHT-KVANTMOT                       
503700                                  UT5-EKHT-KVAVIS                         
503800     MOVE SPACE                TO UT5-EKHT-KDSORT                         
503900     MOVE SPACE                TO UT5-EKHT-KDTRADP                        
504000     MOVE SPACE                TO UT5-EKHT-FLDCET                         
504100     MOVE SPACE                TO UT5-EKHT-IDKUNDRF                       
504200     MOVE SPACE                TO UT5-EKHT-IDFAKT-EXP                     
504300     .                                                                    
504400     EJECT                                                                
504500 S06-HAMTA-BOLLA-INFO   SECTION.                                          
504600     MOVE 'S06-HAMTA-BOLLA-INFO' TO WS-SEKTION                            
504700     IF DCS-SDC AND DCS-ITALY                                             
504800       MOVE W-DALASTN(3:6)     TO WS-DALASTN                              
504900       IF VORD-TILASTN-SK = WS-DALASTN  AND                               
505000         KOLLI-IDLBBET = W-IDLBBET AND                                    
505100         IN-IDKOLLI = W-IDKOLLI-4  AND                                    
505200         IN-IDDISTR = W-IDDISTR-4  AND                                    
505300         IN-IDKUNDNR = W-IDKUNDNR-4  AND                                  
505400         IN-IDORDNR7 = W-IDORDNR7-4  AND                                  
505500         IN-IDDC  =  W-IDDC-4491                                          
505600         CONTINUE                                                         
505700       ELSE                                                               
505800         MOVE IN-IDDC                 TO W-IDDC-4491                      
505900         PERFORM IMS-GU-WDGX4491                                          
506000         IF SEGMENT-FINNS                                                 
506100           MOVE VORD-TILASTN-SK       TO W-DALASTN(2:7)                   
506200           MOVE 20                    TO W-DALASTN(1:2)                   
506300           MOVE KOLLI-IDLBBET         TO W-IDLBBET                        
506400           MOVE IN-IDKOLLI            TO W-IDKOLLI-4                      
506500           MOVE IN-IDDISTR            TO W-IDDISTR-4                      
506600           MOVE IN-IDKUNDNR           TO W-IDKUNDNR-4                     
506700           MOVE IN-IDORDNR7           TO W-IDORDNR7-4                     
506800           PERFORM IMS-GNP-WDGX4494                                       
506900           IF SEGMENT-SAKNAS                                              
507000             MOVE ZERO                TO 4494-IDTRPBON                    
507100             MOVE SPACE               TO 4494-IDTRPBOT                    
507200           END-IF                                                         
507300         END-IF                                                           
507400       END-IF                                                             
507500     ELSE                                                                 
507600         MOVE ZERO                    TO 4494-IDTRPBON                    
507700         MOVE SPACE                   TO 4494-IDTRPBOT                    
507800     END-IF                                                               
507900     .                                                                    
508000     EJECT                                                                
508100 S07-W335PRIS  SECTION.                                                   
508200     MOVE 'S07-W335PRIS'       TO WS-SEKTION                              
508300                                                                          
508400     MOVE 1                    TO PRIS-KDCALL                             
508500     MOVE IDPGM                TO PRIS-IDPGM                              
508600     MOVE ORAD-IDARTNR         TO PRIS-IDARTNR                            
508700     MOVE IN-IDDISTR           TO PRIS-IDDISTR                            
508800     MOVE IN-IDKUNDNR          TO PRIS-IDKUNDNR                           
508900     MOVE KORD-IDDC            TO PRIS-IDDC                               
509000     MOVE ORAD-KDORDKL         TO PRIS-KDORDKL                            
509100     MOVE ORAD-KVBEART         TO PRIS-KVBEART                            
509200     MOVE ORAD-FLINVEST        TO PRIS-FLINVEST                           
509300     CALL W335PRIS USING PRIS-W335PRIS PRIS-WDK6-PCB PRIS-WDK7-PCB        
509400                         PRIS-WDB2-PCB PRIS-WDB1-PCB                      
509500                         PRIS-WDC1-PCB PRIS-WDC2-PCB                      
509600                         PRIS-COST-WDK6-PCB PRIS-COST-WDK7-PCB            
509700                         PRIS-COST-WDF1-PCB PRIS-COST-9305-PCB            
509800                         PRIS-COST-WDK72-PCB                              
509900                         PRIS-COST-WDB6-PCB                               
510000     IF PRIS-KDSVAR = '2'                                                 
510100        MOVE 'DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'                
510200                               TO FELTEXT                                 
510300        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
510400     END-IF                                                               
510500     .                                                                    
510600     EJECT                                                                
510700 S08-W335PRIS  SECTION.                                                   
510800     MOVE 'S08-W335PRIS'       TO WS-SEKTION                              
510900                                                                          
511000     MOVE 1                    TO PRIS-KDCALL                             
511100     MOVE IDPGM                TO PRIS-IDPGM                              
511200     MOVE ORAD-IDARTNR         TO PRIS-IDARTNR                            
511300     MOVE DCS-IDDISTR-REFILL   TO PRIS-IDDISTR                            
511400     MOVE 0                    TO PRIS-IDKUNDNR                           
511500     MOVE WC-CDC-SE            TO PRIS-IDDC                               
511600*--  DETTA DC GÄLLER BARA FÖR CN OCH IN (REFILLANDE DC = 11)              
511700     MOVE 4                    TO PRIS-KDORDKL                            
511800     MOVE 1                    TO PRIS-KVBEART                            
511900     MOVE SPACE                TO PRIS-FLINVEST                           
512000     CALL W335PRIS USING PRIS-W335PRIS PRIS-WDK6-PCB PRIS-WDK7-PCB        
512100                         PRIS-WDB2-PCB PRIS-WDB1-PCB                      
512200                         PRIS-WDC1-PCB PRIS-WDC2-PCB                      
512300                         PRIS-COST-WDK6-PCB PRIS-COST-WDK7-PCB            
512400                         PRIS-COST-WDF1-PCB PRIS-COST-9305-PCB            
512500                         PRIS-COST-WDK72-PCB                              
512600                         PRIS-COST-WDB6-PCB                               
512700     IF PRIS-KDSVAR = '2'                                                 
512800        MOVE 'DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'                
512900                               TO FELTEXT                                 
513000        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
513100     END-IF                                                               
513200     .                                                                    
513300     EJECT                                                                
513400 S09-BYT-KDVALISO-TILL-KDVALUTA  SECTION.                                 
513500     MOVE 'S09-BYT-KDVALISO'  TO WS-SEKTION                               
513600     MOVE ZERO                      TO RIK-KDVALUTA                       
513700     MOVE +1                        TO TAB-IX                             
513800     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
513900      IF DIST79-DEALER-PRICE                                              
514000       IF IN-KDVALISO-FAKT  = TAB-KDVALISO (TAB-IX)                       
514100         MOVE TAB-KDVALUTA (TAB-IX) TO RIK-KDVALUTA                       
514200         MOVE TAB-IX-MAX            TO TAB-IX                             
514300       END-IF                                                             
514400      ELSE                                                                
514500       IF BET-KDVALISO      = TAB-KDVALISO (TAB-IX)                       
514600         MOVE TAB-KDVALUTA (TAB-IX) TO RIK-KDVALUTA                       
514700         MOVE TAB-IX-MAX            TO TAB-IX                             
514800       END-IF                                                             
514900      END-IF                                                              
515000      ADD +1 TO TAB-IX                                                    
515100     END-PERFORM                                                          
515200     .                                                                    
515300     EJECT                                                                
515400 S10-SPARA-ID  SECTION.                                                   
515500     MOVE 'S10-SPARA-ID'  TO WS-SEKTION                                   
515600     MOVE IN-IDFAKT              TO WS-IDFAKT                             
515700     MOVE IN-IDDISTR             TO WS-IDDISTR                            
515800     MOVE IN-IDKUNDNR            TO WS-IDKUNDNR                           
515900     MOVE IN-IDORDNR7            TO WS-IDORDNR7                           
516000     MOVE IN-IDPRODNR            TO WS-IDPRODNR                           
516100     MOVE IN-IDKOLLI             TO WS-IDKOLLI                            
516200     .                                                                    
516300     EJECT                                                                
516400 S12-SKRIV-W47653 SECTION.                                                
516500     WRITE UT2-POST     FROM RJX-W4765201                                 
516600                                                                          
516700     MOVE UT2-IDPTYP        TO POSTSUM-TRANSTYP                           
516800     MOVE 'W47653'          TO POSTSUM-FDNAMN                             
516900     MOVE 'W47651D3'        TO POSTSUM-DDNAMN2                            
517000     CALL POSTSUM USING POSTSUM-PARM                                      
517100     .                                                                    
517200     EJECT                                                                
517300 S13-SKRIV-W47654 SECTION.                                                
517400     WRITE UT3-POST     FROM UT3-AREA                                     
517500                                                                          
517600     MOVE UT3-IDPTYP        TO POSTSUM-TRANSTYP                           
517700     MOVE 'W47654'          TO POSTSUM-FDNAMN                             
517800     MOVE 'W47651D4'        TO POSTSUM-DDNAMN2                            
517900     CALL POSTSUM USING POSTSUM-PARM                                      
518000     .                                                                    
518100     EJECT                                                                
518200 S14-SKRIV-W47658 SECTION.                                                
518300     WRITE UT4-POST     FROM UT4-AREA                                     
518400                                                                          
518500     MOVE UT4-IDPTYP        TO POSTSUM-TRANSTYP                           
518600     MOVE 'W47658'          TO POSTSUM-FDNAMN                             
518700     MOVE 'W47651D5'        TO POSTSUM-DDNAMN2                            
518800     CALL POSTSUM USING POSTSUM-PARM                                      
518900     .                                                                    
519000     EJECT                                                                
519100 S15-SKRIV-W47660 SECTION.                                                
519200     WRITE UT5-POST     FROM UT5-AREA                                     
519300                                                                          
519400     MOVE UT5-AREA          TO UT5-FILLER                                 
519500     MOVE UT5-IDPTYP        TO POSTSUM-TRANSTYP                           
519600     MOVE 'W47660'          TO POSTSUM-FDNAMN                             
519700     MOVE 'W47651D6'        TO POSTSUM-DDNAMN2                            
519800     CALL POSTSUM USING POSTSUM-PARM                                      
519900     .                                                                    
520000     EJECT                                                                
520100 S16-SKRIV-W4768E SECTION.                                                
520200                                                                          
520300     MOVE UT5-EKHT-IDPGM TO UT6-EKHT-IDPGM                                
520400     ACCEPT UT6-EKHT-TIREGDAT FROM DATE                                   
520500     ACCEPT UT6-EKHT-TIKLOCK  FROM TIME                                   
520600     MOVE UT5-EKHT-IDSEKVNR TO UT6-EKHT-IDSEKVNR                          
520700     MOVE UT5-EKHT-CT-IDSYSTEM TO UT6-EKHT-CT-IDSYSTEM                    
520800     MOVE UT5-EKHT-CT-IDPTYP   TO UT6-EKHT-CT-IDPTYP                      
520900     MOVE UT5-EKHT-CT-IDVTYP   TO UT6-EKHT-CT-IDVTYP                      
521000     MOVE UT5-EKHT-BEVAT       TO UT6-EKHT-BEVAT                          
521100     MOVE UT5-EKHT-DAVERDAT    TO UT6-EKHT-DAVERDAT                       
521200     MOVE UT5-EKHT-FLLSBOK     TO UT6-EKHT-FLLSBOK                        
521300     MOVE UT5-EKHT-IDANALYS    TO UT6-EKHT-IDANALYS                       
521400     MOVE UT5-EKHT-IDKONTO     TO UT6-EKHT-IDKONTO                        
521500     MOVE UT5-EKHT-IDKST       TO UT6-EKHT-IDKST                          
521600     MOVE UT5-EKHT-IDARTNR     TO UT6-EKHT-IDARTNR                        
521700     MOVE UT5-EKHT-IDDC-SEND   TO UT6-EKHT-IDDC-SEND                      
521800     MOVE UT5-EKHT-IDDC-REC    TO UT6-EKHT-IDDC-REC                       
521900     MOVE UT5-EKHT-IDDISTR     TO UT6-EKHT-IDDISTR                        
522000     MOVE UT5-EKHT-IDKUNDNR    TO UT6-EKHT-IDKUNDNR                       
522100     MOVE UT5-EKHT-IDTRANS     TO UT6-EKHT-IDTRANS                        
522200     MOVE UT5-EKHT-IDVERGL     TO UT6-EKHT-IDVERGL                        
522300     MOVE UT5-EKHT-KDANMORS    TO UT6-EKHT-KDANMORS                       
522400     MOVE UT5-EKHT-KDEKHHT     TO UT6-EKHT-KDEKHHT                        
522500     MOVE UT5-EKHT-KDEKSHT     TO UT6-EKHT-KDEKSHT                        
522600     MOVE UT5-EKHT-KDEKNIVA    TO UT6-EKHT-KDEKNIVA                       
522700     MOVE UT5-EKHT-KDFRAKT     TO UT6-EKHT-KDFRAKT                        
522800     MOVE UT5-EKHT-KDPRODSL    TO UT6-EKHT-KDPRODSL                       
522900     MOVE UT5-EKHT-KDPSLLOC    TO UT6-EKHT-KDPSLLOC                       
523000     MOVE UT5-EKHT-KDVALISO    TO UT6-EKHT-KDVALISO                       
523100     MOVE UT5-EKHT-KVANTAL     TO UT6-EKHT-KVANTAL                        
523200     MOVE UT5-EKHT-PRARTNTO    TO UT6-EKHT-PRARTNTO                       
523300     MOVE UT5-EKHT-PRARTSJK    TO UT6-EKHT-PRARTSJK                       
523400     MOVE UT5-EKHT-PRARTSTD    TO UT6-EKHT-PRARTSTD                       
523500     MOVE UT5-EKHT-PRDIRLON    TO UT6-EKHT-PRDIRLON                       
523600     MOVE UT5-EKHT-PRDMTRL     TO UT6-EKHT-PRDMTRL                        
523700     MOVE UT5-EKHT-PRINK       TO UT6-EKHT-PRINK                          
523800     MOVE UT5-EKHT-PRKURS      TO UT6-EKHT-PRKURS                         
523900     MOVE UT5-EKHT-PRLANDCO    TO UT6-EKHT-PRLANDCO                       
524000     MOVE UT5-EKHT-PROVRPAL    TO UT6-EKHT-PROVRPAL                       
524100     MOVE UT5-EKHT-SUBEL       TO UT6-EKHT-SUBEL                          
524200     MOVE UT5-EKHT-SUVAT       TO UT6-EKHT-SUVAT                          
524300     MOVE UT5-EKHT-DAAVIDAT    TO UT6-EKHT-DAAVIDAT                       
524400     MOVE UT5-EKHT-IDAVINR     TO UT6-EKHT-IDAVINR                        
524500     MOVE UT5-EKHT-IDLEVNR     TO UT6-EKHT-IDLEVNR                        
524600     MOVE UT5-EKHT-KDAVVTYP    TO UT6-EKHT-KDAVVTYP                       
524700     MOVE UT5-EKHT-KDRT        TO UT6-EKHT-KDRT                           
524800     MOVE UT5-EKHT-KVANTMOT    TO UT6-EKHT-KVANTMOT                       
524900     MOVE UT5-EKHT-KVAVIS      TO UT6-EKHT-KVAVIS                         
525000     MOVE UT5-EKHT-KDSORT      TO UT6-EKHT-KDSORT                         
525100     MOVE UT5-EKHT-KDTRADP     TO UT6-EKHT-KDTRADP                        
525200     MOVE UT5-EKHT-FLOVRLEV    TO UT6-EKHT-FLOVRLEV                       
525300     MOVE UT5-EKHT-IDORDNR5    TO UT6-EKHT-IDORDNR5                       
525400     MOVE UT5-EKHT-IDUSER      TO UT6-EKHT-IDUSER                         
525500     MOVE UT5-EKHT-PRHEMTAG    TO UT6-EKHT-PRHEMTAG                       
525600     MOVE UT5-EKHT-FLDCET      TO UT6-EKHT-FLDCET                         
525700     MOVE UT5-EKHT-IDKUNDRF    TO UT6-EKHT-IDKUNDRF                       
525800     MOVE UT5-EKHT-IDFAKT-EXP  TO UT6-EKHT-IDFAKT-EXP                     
525900                                                                          
526000     WRITE UT6-POST        FROM UT6-AREA                                  
526100                                                                          
526200     MOVE UT6-AREA           TO UT6-FILLER                                
526300     MOVE UT6-IDPTYP         TO POSTSUM-TRANSTYP                          
526400     MOVE 'W4768E'           TO POSTSUM-FDNAMN                            
526500     MOVE 'W47651DG'         TO POSTSUM-DDNAMN2                           
526600     CALL POSTSUM USING POSTSUM-PARM                                      
526700     IF (UT6-EKHT-KDTRADP = 'BR12' AND                                    
526800        ((UT6-EKHT-KDEKHHT = '203' AND UT6-EKHT-KDEKSHT = '201')          
526900          OR                                                              
527000         (UT6-EKHT-KDEKHHT = '404' AND UT6-EKHT-KDEKSHT = '401')))        
527100        IF NOT-FIRST-REC-TRANS                                            
527200          PERFORM S26-FIX-LOCAL-TIME                                      
527300          MOVE ZERO TO NOTF-IDSEKVNR                                      
527400          PERFORM S30-SEND-OPEN                                           
527500          MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                              
527600          PERFORM S31-SEND-PUT-PROP                                       
527700          MOVE JA  TO FIRST-REC-TRANS-SW                                  
527800        END-IF                                                            
527900        MOVE UT6-EKHT-KDEKHHT  TO NOTF-KDEKHHT                            
528000        MOVE UT6-EKHT-KDEKSHT  TO NOTF-KDEKSHT                            
528100        MOVE UT6-EKHT-DAVERDAT TO NOTF-DAVERDAT                           
528200        MOVE AKTUELL-TID(1:6)  TO NOTF-TIREGTID                           
528300        MOVE UT6-EKHT-IDVERGL  TO NOTF-IDVERGL                            
528400        MOVE WC-NDC-BR         TO NOTF-IDDC                               
528500        MOVE IN-IDFAKT         TO NOTF-IDFAKT                             
528600        MOVE UT6-EKHT-IDORDNR5 TO NOTF-IDORDER                            
528700        MOVE UT6-EKHT-IDKUNDNR TO NOTF-IDKUNDNR                           
528800        MOVE IN-IDKOLLI        TO NOTF-IDKOLLI                            
528900        MOVE UT6-EKHT-IDARTNR  TO W-IDARTNR-EDIT-X                        
529000        MOVE FUNCTION TRIM(W-IDARTNR-EDIT-X LEADING)                      
529100                                TO NOTF-IDARTNR20                         
529200        IF UT6-EKHT-KVANTAL > ZERO                                        
529300         COMPUTE NOTF-KVANTAL = UT6-EKHT-KVANTAL * -1                     
529400        ELSE                                                              
529500         MOVE UT6-EKHT-KVANTAL   TO NOTF-KVANTAL                          
529600        END-IF                                                            
529700        ADD +1 TO NOTF-IDSEKVNR                                           
529800        PERFORM S32-SEND-PUT                                              
529900     END-IF                                                               
530000     .                                                                    
530100     EJECT                                                                
530200 S17-SKRIV-W4768F SECTION.                                                
530300                                                                          
530400     MOVE UT5-EKHT-IDPGM TO UT7-EKHT-IDPGM                                
530500     ACCEPT UT7-EKHT-TIREGDAT FROM DATE                                   
530600     ACCEPT UT7-EKHT-TIKLOCK  FROM TIME                                   
530700     MOVE UT5-EKHT-IDSEKVNR TO UT7-EKHT-IDSEKVNR                          
530800     MOVE UT5-EKHT-CT-IDSYSTEM TO UT7-EKHT-CT-IDSYSTEM                    
530900     MOVE UT5-EKHT-CT-IDPTYP   TO UT7-EKHT-CT-IDPTYP                      
531000     MOVE UT5-EKHT-CT-IDVTYP   TO UT7-EKHT-CT-IDVTYP                      
531100     MOVE UT5-EKHT-BEVAT       TO UT7-EKHT-BEVAT                          
531200     MOVE UT5-EKHT-DAVERDAT    TO UT7-EKHT-DAVERDAT                       
531300     MOVE UT5-EKHT-FLLSBOK     TO UT7-EKHT-FLLSBOK                        
531400     MOVE UT5-EKHT-IDANALYS    TO UT7-EKHT-IDANALYS                       
531500     MOVE UT5-EKHT-IDKONTO     TO UT7-EKHT-IDKONTO                        
531600     MOVE UT5-EKHT-IDKST       TO UT7-EKHT-IDKST                          
531700     MOVE UT5-EKHT-IDARTNR     TO UT7-EKHT-IDARTNR                        
531800     MOVE UT5-EKHT-IDDC-SEND   TO UT7-EKHT-IDDC-SEND                      
531900     MOVE UT5-EKHT-IDDC-REC    TO UT7-EKHT-IDDC-REC                       
532000     MOVE UT5-EKHT-IDDISTR     TO UT7-EKHT-IDDISTR                        
532100     MOVE UT5-EKHT-IDKUNDNR    TO UT7-EKHT-IDKUNDNR                       
532200     MOVE UT5-EKHT-IDTRANS     TO UT7-EKHT-IDTRANS                        
532300     MOVE UT5-EKHT-IDVERGL     TO UT7-EKHT-IDVERGL                        
532400     MOVE UT5-EKHT-KDANMORS    TO UT7-EKHT-KDANMORS                       
532500     MOVE UT5-EKHT-KDEKHHT     TO UT7-EKHT-KDEKHHT                        
532600     MOVE UT5-EKHT-KDEKSHT     TO UT7-EKHT-KDEKSHT                        
532700     MOVE UT5-EKHT-KDEKNIVA    TO UT7-EKHT-KDEKNIVA                       
532800     MOVE UT5-EKHT-KDFRAKT     TO UT7-EKHT-KDFRAKT                        
532900     MOVE UT5-EKHT-KDPRODSL    TO UT7-EKHT-KDPRODSL                       
533000     MOVE UT5-EKHT-KDPSLLOC    TO UT7-EKHT-KDPSLLOC                       
533100     MOVE UT5-EKHT-KDVALISO    TO UT7-EKHT-KDVALISO                       
533200     MOVE UT5-EKHT-KVANTAL     TO UT7-EKHT-KVANTAL                        
533300     MOVE UT5-EKHT-PRARTNTO    TO UT7-EKHT-PRARTNTO                       
533400     MOVE UT5-EKHT-PRARTSJK    TO UT7-EKHT-PRARTSJK                       
533500     MOVE UT5-EKHT-PRARTSTD    TO UT7-EKHT-PRARTSTD                       
533600     MOVE UT5-EKHT-PRDIRLON    TO UT7-EKHT-PRDIRLON                       
533700     MOVE UT5-EKHT-PRDMTRL     TO UT7-EKHT-PRDMTRL                        
533800     MOVE UT5-EKHT-PRINK       TO UT7-EKHT-PRINK                          
533900     MOVE UT5-EKHT-PRKURS      TO UT7-EKHT-PRKURS                         
534000     MOVE UT5-EKHT-PRLANDCO    TO UT7-EKHT-PRLANDCO                       
534100     MOVE UT5-EKHT-PROVRPAL    TO UT7-EKHT-PROVRPAL                       
534200     MOVE UT5-EKHT-SUBEL       TO UT7-EKHT-SUBEL                          
534300     MOVE UT5-EKHT-SUVAT       TO UT7-EKHT-SUVAT                          
534400     MOVE UT5-EKHT-DAAVIDAT    TO UT7-EKHT-DAAVIDAT                       
534500     MOVE UT5-EKHT-IDAVINR     TO UT7-EKHT-IDAVINR                        
534600     MOVE UT5-EKHT-IDLEVNR     TO UT7-EKHT-IDLEVNR                        
534700     MOVE UT5-EKHT-KDAVVTYP    TO UT7-EKHT-KDAVVTYP                       
534800     MOVE UT5-EKHT-KDRT        TO UT7-EKHT-KDRT                           
534900     MOVE UT5-EKHT-KVANTMOT    TO UT7-EKHT-KVANTMOT                       
535000     MOVE UT5-EKHT-KVAVIS      TO UT7-EKHT-KVAVIS                         
535100     MOVE UT5-EKHT-KDSORT      TO UT7-EKHT-KDSORT                         
535200     MOVE UT5-EKHT-KDTRADP     TO UT7-EKHT-KDTRADP                        
535300     MOVE UT5-EKHT-FLOVRLEV    TO UT7-EKHT-FLOVRLEV                       
535400     MOVE UT5-EKHT-IDORDNR5    TO UT7-EKHT-IDORDNR5                       
535500     MOVE UT5-EKHT-IDUSER      TO UT7-EKHT-IDUSER                         
535600     MOVE UT5-EKHT-PRHEMTAG    TO UT7-EKHT-PRHEMTAG                       
535700     MOVE UT5-EKHT-FLDCET      TO UT7-EKHT-FLDCET                         
535800     MOVE UT5-EKHT-IDKUNDRF    TO UT7-EKHT-IDKUNDRF                       
535900     MOVE UT5-EKHT-IDFAKT-EXP  TO UT7-EKHT-IDFAKT-EXP                     
536000                                                                          
536100     WRITE UT7-POST        FROM UT7-AREA                                  
536200                                                                          
536300     MOVE UT7-AREA           TO UT7-FILLER                                
536400     MOVE UT7-IDPTYP         TO POSTSUM-TRANSTYP                          
536500     MOVE 'W4768F'           TO POSTSUM-FDNAMN                            
536600     MOVE 'W47651DH'         TO POSTSUM-DDNAMN2                           
536700     CALL POSTSUM USING POSTSUM-PARM                                      
536800     .                                                                    
536900     EJECT                                                                
537000 S18-SKRIV-W47663 SECTION.                                                
537100     WRITE UTA-POST         FROM UTA-AREA                                 
537200                                                                          
537300     MOVE UTA-IDPTYP        TO POSTSUM-TRANSTYP                           
537400     MOVE 'W47663'          TO POSTSUM-FDNAMN                             
537500     MOVE 'W47651DA'        TO POSTSUM-DDNAMN2                            
537600     CALL POSTSUM USING POSTSUM-PARM                                      
537700     .                                                                    
537800     EJECT                                                                
537900 S19-SKRIV-W4768D SECTION.                                                
538000     MOVE 'S19-SKRIV-W4768D'  TO WS-SEKTION                               
538100                                                                          
538200     EVALUATE W-8D                                                        
538300       WHEN 1                                                             
538400         WRITE LAB-POST     FROM EK95X-W51095X                            
538500       WHEN 2                                                             
538600         WRITE LAB-POST     FROM EK94X-W51094X                            
538700     END-EVALUATE                                                         
538800                                                                          
538900                                                                          
539000     MOVE W-8D              TO POSTSUM-TRANSTYP                           
539100     MOVE 'W4768D'          TO POSTSUM-FDNAMN                             
539200     MOVE 'W47651DE'        TO POSTSUM-DDNAMN2                            
539300     CALL POSTSUM USING POSTSUM-PARM                                      
539400     .                                                                    
539500     EJECT                                                                
539600 S20-NOLLA-RIL  SECTION.                                                  
539700     MOVE 'RIL'                 TO RIL-IDPTYP                             
539800     MOVE ZERO                  TO RIL-PRAVDRAG                           
539900                                   RIL-PREMBHNT                           
540000                                   RIL-PRFOERS                            
540100                                   RIL-PRFRAKT                            
540200                                   RIL-PRLEGKST                           
540300                                   RIL-PRMOMS                             
540400                                   RIL-SUFKTTILL                          
540500                                   RIL-KDFRAKT                            
540600     MOVE SPACE                 TO RIL-FILLERX12                          
540700     .                                                                    
540800     EJECT                                                                
540900 S21-SKRIV-W4768G SECTION.                                                
541000                                                                          
541100     MOVE UT5-EKHT-IDPGM       TO UT9-EKHT-IDPGM                          
541200     ACCEPT UT9-EKHT-TIREGDAT  FROM DATE                                  
541300     ACCEPT UT9-EKHT-TIKLOCK   FROM TIME                                  
541400     MOVE UT5-EKHT-IDSEKVNR    TO UT9-EKHT-IDSEKVNR                       
541500     MOVE UT5-EKHT-CT-IDSYSTEM TO UT9-EKHT-CT-IDSYSTEM                    
541600     MOVE UT5-EKHT-CT-IDPTYP   TO UT9-EKHT-CT-IDPTYP                      
541700     MOVE UT5-EKHT-CT-IDVTYP   TO UT9-EKHT-CT-IDVTYP                      
541800     MOVE UT5-EKHT-BEVAT       TO UT9-EKHT-BEVAT                          
541900     MOVE UT5-EKHT-DAVERDAT    TO UT9-EKHT-DAVERDAT                       
542000     MOVE UT5-EKHT-FLLSBOK     TO UT9-EKHT-FLLSBOK                        
542100     MOVE UT5-EKHT-IDANALYS    TO UT9-EKHT-IDANALYS                       
542200     MOVE UT5-EKHT-IDKONTO     TO UT9-EKHT-IDKONTO                        
542300     MOVE UT5-EKHT-IDKST       TO UT9-EKHT-IDKST                          
542400     MOVE UT5-EKHT-IDARTNR     TO UT9-EKHT-IDARTNR                        
542500     MOVE UT5-EKHT-IDDC-SEND   TO UT9-EKHT-IDDC-SEND                      
542600     MOVE UT5-EKHT-IDDC-REC    TO UT9-EKHT-IDDC-REC                       
542700     MOVE UT5-EKHT-IDDISTR     TO UT9-EKHT-IDDISTR                        
542800     MOVE UT5-EKHT-IDKUNDNR    TO UT9-EKHT-IDKUNDNR                       
542900     MOVE UT5-EKHT-IDTRANS     TO UT9-EKHT-IDTRANS                        
543000     MOVE UT5-EKHT-IDVERGL     TO UT9-EKHT-IDVERGL                        
543100     MOVE UT5-EKHT-KDANMORS    TO UT9-EKHT-KDANMORS                       
543200     MOVE UT5-EKHT-KDEKHHT     TO UT9-EKHT-KDEKHHT                        
543300     MOVE UT5-EKHT-KDEKSHT     TO UT9-EKHT-KDEKSHT                        
543400     MOVE UT5-EKHT-KDEKNIVA    TO UT9-EKHT-KDEKNIVA                       
543500     MOVE UT5-EKHT-KDFRAKT     TO UT9-EKHT-KDFRAKT                        
543600     MOVE UT5-EKHT-KDPRODSL    TO UT9-EKHT-KDPRODSL                       
543700     MOVE UT5-EKHT-KDPSLLOC    TO UT9-EKHT-KDPSLLOC                       
543800     MOVE UT5-EKHT-KDVALISO    TO UT9-EKHT-KDVALISO                       
543900     MOVE UT5-EKHT-KVANTAL     TO UT9-EKHT-KVANTAL                        
544000     MOVE UT5-EKHT-PRARTNTO    TO UT9-EKHT-PRARTNTO                       
544100     MOVE UT5-EKHT-PRARTSJK    TO UT9-EKHT-PRARTSJK                       
544200     MOVE UT5-EKHT-PRARTSTD    TO UT9-EKHT-PRARTSTD                       
544300     MOVE UT5-EKHT-PRDIRLON    TO UT9-EKHT-PRDIRLON                       
544400     MOVE UT5-EKHT-PRDMTRL     TO UT9-EKHT-PRDMTRL                        
544500     MOVE UT5-EKHT-PRINK       TO UT9-EKHT-PRINK                          
544600     MOVE UT5-EKHT-PRKURS      TO UT9-EKHT-PRKURS                         
544700     MOVE UT5-EKHT-PRLANDCO    TO UT9-EKHT-PRLANDCO                       
544800     MOVE UT5-EKHT-PROVRPAL    TO UT9-EKHT-PROVRPAL                       
544900     MOVE UT5-EKHT-SUBEL       TO UT9-EKHT-SUBEL                          
545000     MOVE UT5-EKHT-SUVAT       TO UT9-EKHT-SUVAT                          
545100     MOVE UT5-EKHT-DAAVIDAT    TO UT9-EKHT-DAAVIDAT                       
545200     MOVE UT5-EKHT-IDAVINR     TO UT9-EKHT-IDAVINR                        
545300     MOVE UT5-EKHT-IDLEVNR     TO UT9-EKHT-IDLEVNR                        
545400     MOVE UT5-EKHT-KDAVVTYP    TO UT9-EKHT-KDAVVTYP                       
545500     MOVE UT5-EKHT-KDRT        TO UT9-EKHT-KDRT                           
545600     MOVE UT5-EKHT-KVANTMOT    TO UT9-EKHT-KVANTMOT                       
545700     MOVE UT5-EKHT-KVAVIS      TO UT9-EKHT-KVAVIS                         
545800     MOVE UT5-EKHT-KDSORT      TO UT9-EKHT-KDSORT                         
545900     MOVE UT5-EKHT-KDTRADP     TO UT9-EKHT-KDTRADP                        
546000     MOVE UT5-EKHT-FLOVRLEV    TO UT9-EKHT-FLOVRLEV                       
546100     MOVE UT5-EKHT-IDORDNR5    TO UT9-EKHT-IDORDNR5                       
546200     MOVE UT5-EKHT-IDUSER      TO UT9-EKHT-IDUSER                         
546300     MOVE UT5-EKHT-PRHEMTAG    TO UT9-EKHT-PRHEMTAG                       
546400     MOVE UT5-EKHT-FLDCET      TO UT9-EKHT-FLDCET                         
546500     MOVE UT5-EKHT-IDKUNDRF    TO UT9-EKHT-IDKUNDRF                       
546600     MOVE UT5-EKHT-IDFAKT-EXP  TO UT9-EKHT-IDFAKT-EXP                     
546700                                                                          
546800     WRITE UT9-POST          FROM UT9-AREA                                
546900                                                                          
547000     MOVE UT9-AREA             TO UT9-FILLER                              
547100     MOVE UT9-IDPTYP           TO POSTSUM-TRANSTYP                        
547200     MOVE 'W4768G'             TO POSTSUM-FDNAMN                          
547300     MOVE 'W47651DI'           TO POSTSUM-DDNAMN2                         
547400     CALL POSTSUM USING POSTSUM-PARM                                      
547500     .                                                                    
547600     EJECT                                                                
547700 S22-SKRIV-RIL  SECTION.                                                  
547800     MOVE 'S22-SKRIV-RIL'   TO WS-SEKTION                                 
547900*                                                                         
548000      MOVE 'RIL'                TO RJX-IDPTYP                             
548100      MOVE RIL-W461RILN-CTX     TO RJX-FILLER                             
548200      MOVE WS2-IDFAKT           TO RJX-IDFAKT                             
548300      MOVE ZERO                 TO RJX-IDDISTR                            
548400      MOVE ZERO                 TO RJX-IDKUNDNR                           
548500      MOVE ZERO                 TO RJX-IDORDER                            
548600      MOVE ZERO                 TO RJX-IDKUNDNR-S                         
548700      MOVE ZERO                 TO RJX-IDPRODNR                           
548800      MOVE ZERO                 TO RJX-IDKOLLI                            
548900      MOVE ZERO                 TO RJX-IDPURAD                            
549000      MOVE ZERO                 TO RJX-IDTRPBON                           
549100*                                                                         
549200     PERFORM S12-SKRIV-W47653                                             
549300     .                                                                    
549400     EJECT                                                                
549500 S25-MARKNADSBOLAGS-VALUTA  SECTION.                                      
549600                                                                          
549700     IF WS-IDMARKBO NOT = SPACE                                           
549800                                                                          
549900       MOVE ZERO                      TO CURR-SUORDV-IN                   
550000                                         CURR-PRARTVNA-IN                 
550100                                         CURR-PRARTSTD-IN                 
550200                                         CURR-PRKURS-02                   
550300*                                                                         
550400*      ÅRS-KURSEN                                                         
550500*      COMPUTE CURR-PRKURS = 5118-PRKURS / 100                            
550600       MOVE 1                         TO CURR-PRKURS                      
550700       MOVE ZERO                      TO CURR-PRARTSJK-IN                 
550800       MOVE RIO-PRARTSTD              TO CURR-PRARTSTD-IN                 
550900       MOVE W-KDVALISO                TO CURR-KDVALISO-01                 
551000       MOVE +2                        TO CURR-KDCALL                      
551100       CALL W335CURR            USING CURR-W335CURR                       
551200       MOVE CURR-PRARTSTD-UT          TO RIO-PRARTSTD                     
551300     END-IF                                                               
551400     .                                                                    
551500     SKIP2                                                                
551600 S26-FIX-LOCAL-TIME SECTION.                                              
551700                                                                          
551800******** ADAPT DATE AND TIME FOR TIMEZONES                                
551900     PERFORM IMS-GU-WDB601                                                
552000                                                                          
552100     MOVE '011'                TO MSGI-KDCALL                             
552200     MOVE DCS-IDTIDZON         TO MSGI-IDTIDZON                           
552300     MOVE DCS-IDDC             TO MSGI-IDDC                               
552400     MOVE DAGENS-DATUM         TO MSGI-TILOKDAT                           
552500     MOVE W-TIKLOCK            TO MSGI-TILOKTID                           
552600     CALL WL01TIDZ USING          MSGI-WL01TIDZ                           
552700     MOVE MSGI-TILOKTID(1:4) TO AKTUELL-TID(1:4)                          
552800     .                                                                    
552900     EJECT                                                                
553000 S30-SEND-OPEN SECTION.                                                   
553100     MOVE 'OPEN'                        TO SEND-KDFUNC                    
553200     MOVE WS-ADDRESS-MQASYNC            TO SEND-ADDISPABS                 
553300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
553400                         SEND-OPEN-AREA                                   
553500     IF SEND-KDRC > 0                                                     
553600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
553700       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
553800       DELIMITED BY SIZE INTO FELTEXT                                     
553900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
554000     END-IF                                                               
554100     .                                                                    
554200     EJECT                                                                
554300 S32-SEND-PUT SECTION.                                                    
554400                                                                          
554500     MOVE 'PUT'                            TO SEND-KDFUNC                 
554600     MOVE LENGTH OF NOTF-AREA              TO SEND-KVDLEN                 
554700     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
554800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
554900                         SEND-KVDLEN                                      
555000                         NOTF-AREA                                        
555100     IF SEND-KDRC > 1                                                     
555200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
555300       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
555400       DELIMITED BY SIZE INTO FELTEXT                                     
555500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
555600     END-IF                                                               
555700     .                                                                    
555800     EJECT                                                                
555900 S31-SEND-PUT-PROP SECTION.                                               
556000                                                                          
556100     SET PROP-IX                 TO +1                                    
556200*    MANDATORY PROPERTY THAT SPECIFIES THE ACTUAL DESTINATION             
556300     MOVE 'ADDRESS'              TO PROP-IDPROPTYPE  (PROP-IX)            
556400     MOVE 'ADDISPABS'            TO PROP-IDPROPNAME  (PROP-IX)            
556500     MOVE WS-ADDRESS-WHSTOCKA    TO PROP-BEPROPVALUE (PROP-IX)            
556600                                                                          
556700     SET PROP-IX              UP BY +1                                    
556800*    OPTIONAL MQ MESSAGE PROPERTIES. CAN BE CASE-SENSITIVE                
556900     MOVE 'MQMPROP'              TO PROP-IDPROPTYPE  (PROP-IX)            
557000     MOVE 'CountryCode'          TO PROP-IDPROPNAME  (PROP-IX)            
557100     MOVE 'BR'                   TO PROP-BEPROPVALUE (PROP-IX)            
557200                                                                          
557300*    SET THE NUMBER OF PROPERTIES (KVANTAL) SO CORRECT LENGTH             
557400*    IS CALCULATED.                                                       
557500     SET PROP-KVANTAL            TO PROP-IX                               
557600                                                                          
557700     MOVE 'PUT'                            TO SEND-KDFUNC                 
557800     MOVE LENGTH OF PROP-WZ04PROP          TO SEND-KVDLEN                 
557900     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
558000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
558100                         SEND-KVDLEN                                      
558200                         PROP-WZ04PROP                                    
558300     IF SEND-KDRC > 1                                                     
558400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
558500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
558600       DELIMITED BY SIZE INTO FELTEXT                                     
558700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
558800     END-IF                                                               
558900     .                                                                    
559000     EJECT                                                                
559100 S33-SEND-CLOSE SECTION.                                                  
559200     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
559300     MOVE WZ04-SEND-IDCOM                  TO SEND-IDCOM                  
559400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
559500                                                                          
559600     IF SEND-KDRC > 0                                                     
559700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
559800       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
559900       DELIMITED BY SIZE INTO FELTEXT                                     
560000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
560100     END-IF                                                               
560200     .                                                                    
560300     EJECT                                                                
560400 S40-HAMTA-PRAVCOST  SECTION.                                             
560500     MOVE IN-IDDISTR            TO TEST-IDDISTR                           
560600     MOVE KORD-IDDC             TO W-IDDC                                 
560700                                   WS-IDDC                                
560800     IF KORD-IDDC NOT = DCS-IDDC                                          
560900       MOVE KORD-IDDC           TO W-IDDC-B6                              
561000       PERFORM IMS-GU-WDB601                                              
561100     END-IF                                                               
561200     MOVE ORAD-IDARTNR          TO W-IDARTNR                              
561300                                                                          
561400* FETCH PRAVCOST FOR US                                                   
561500     IF DCS-NDC-NA                                                        
561600     OR ((DCS-CDC OR DCS-DDC)AND (DIST07-USA-RETAILER))                   
561700     OR ((DCS-CDC OR DCS-DDC)AND (DIST07-CAN-RETAILER))                   
561800        IF (DCS-CDC OR DCS-DDC) AND (DIST07-USA-RETAILER)                 
561900           MOVE  WC-NDC-US-RU           TO W-IDDC                         
562000        ELSE                                                              
562100          IF (DCS-CDC OR DCS-DDC) AND (DIST07-CAN-RETAILER)               
562200             MOVE WC-NDC-CA           TO W-IDDC                           
562300          END-IF                                                          
562400        END-IF                                                            
562500                                                                          
562600        PERFORM IMS-GU-WDK711                                             
562700        IF SEGMENT-FINNS                                                  
562800          MOVE SLAG-PRAVCOST    TO IN-PRAVCOST                            
562900        END-IF                                                            
563000        MOVE W-IDARTNR          TO TEST-ARTIKEL                           
563100                                   W-IDARTNR-CORE                         
563200        IF BYT19-BYTES OR BYT19-RADIO                                     
563300          IF BYT19-BYTES                                                  
563400           ADD +6000            TO W-IDARTNR-CORE                         
563500          ELSE                                                            
563600           IF BYT19-RADIO                                                 
563700             ADD +1000          TO W-IDARTNR-CORE                         
563800           END-IF                                                         
563900          END-IF                                                          
564000          PERFORM IMS-GU-WDK711-CORE                                      
564100          IF SEGMENT-SAKNAS                                               
564200            MOVE ZERO           TO IN-PRAVCOST-CORE                       
564300          ELSE                                                            
564400            MOVE CORE-SLAG-PRAVCOST TO IN-PRAVCOST-CORE                   
564500          END-IF                                                          
564600        ELSE                                                              
564700           MOVE ZERO            TO IN-PRAVCOST-CORE                       
564800        END-IF                                                            
564900     END-IF                                                               
565000                                                                          
565100* FETCH PRAVCOST FOR OTHER MARKETS                                        
565200     IF XDC-NON-VCC-OWNED OR LDC-CN                                       
565300        PERFORM IMS-GU-WDK711                                             
565400        IF SEGMENT-FINNS                                                  
565500          MOVE SLAG-PRAVCOST    TO IN-PRAVCOST                            
565600        END-IF                                                            
565700        MOVE W-IDARTNR          TO TEST-ARTIKEL                           
565800                                   W-IDARTNR-CORE                         
565900        IF BYT19-BYTES OR BYT19-RADIO                                     
566000          IF BYT19-BYTES                                                  
566100           ADD +6000            TO W-IDARTNR-CORE                         
566200          ELSE                                                            
566300           IF BYT19-RADIO                                                 
566400             ADD +1000          TO W-IDARTNR-CORE                         
566500           END-IF                                                         
566600          END-IF                                                          
566700          PERFORM IMS-GU-WDK711-CORE                                      
566800          IF SEGMENT-SAKNAS                                               
566900            MOVE ZERO           TO IN-PRAVCOST-CORE                       
567000          ELSE                                                            
567100            MOVE CORE-SLAG-PRAVCOST TO IN-PRAVCOST-CORE                   
567200          END-IF                                                          
567300        ELSE                                                              
567400          MOVE ZERO             TO IN-PRAVCOST-CORE                       
567500        END-IF                                                            
567600     END-IF                                                               
567700                                                                          
567800**** ÅTERSTÄLL DC EFTER AVERAGE COST BERÄKNING                            
567900     MOVE IN-IDDC           TO W-IDDC-B6                                  
568000     PERFORM IMS-GU-WDB601                                                
568100     .                                                                    
568200     EJECT                                                                
568300 S50-LAS-WDE111  SECTION.                                                 
568400     MOVE 'S50-LAS-WDE111'        TO WS-SEKTION                           
568500                                                                          
568600     IF W-IDSHIPM = IN-IDSHIPM  AND                                       
568700        W-IDDISTR-E1 = IN-IDDISTR AND                                     
568800        W-IDKUNDNR-E1 = IN-IDKUNDNR                                       
568900        CONTINUE                                                          
569000     ELSE                                                                 
569100        MOVE IN-IDSHIPM         TO W-IDSHIPM                              
569200        MOVE IN-IDDISTR         TO W-IDDISTR-E1                           
569300        MOVE IN-IDKUNDNR        TO W-IDKUNDNR-E1                          
569400        PERFORM IMS-GU-WDE111                                             
569500        IF SEGMENT-SAKNAS                                                 
569600          MOVE ZERO             TO SGMT-KDORDKL-MAX                       
569700        END-IF                                                            
569800     END-IF                                                               
569900     .                                                                    
570000     EJECT                                                                
570100 S50-HAMTA-USA-FRAKT   SECTION.                                           
570200     MOVE IN-IDSHIPM          TO W-IDSHIPM                                
570300     MOVE IN-IDDISTR          TO W-IDDISTR-E1                             
570400     MOVE IN-IDKUNDNR         TO W-IDKUNDNR-E1                            
570500     PERFORM IMS-GU-WDE111                                                
570600     IF SEGMENT-FINNS                                                     
570700       PERFORM IMS-GNP-WDE122                                             
570800       IF SEGMENT-FINNS                                                   
570900         IF TILL-PRFRAKT NOT = ZERO                                       
571000           MOVE TILL-PRFRAKT  TO SPAR-PRFRAKT-LOC                         
571100         END-IF                                                           
571200       END-IF                                                             
571300     END-IF                                                               
571400     .                                                                    
571500     EJECT                                                                
571600* --- IMS SEKTIONER ---                                                   
571700                                                                          
571800 IMS-GU-WDE411-BSEQ SECTION.                                              
571900     MOVE 'IMS-GU-WDE411-BSEQ'    TO WS-SEKTION                           
572000                                                                          
572100     STRING 'WDE411  (WDE4BSEQ =' W-WDE4BSEQ-X ')'                        
572200          DELIMITED BY SIZE INTO SSA1                                     
572300     MOVE '    ' TO GODK-STATUSKODER                                      
572400     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE411 SSA1                    
572500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
572600     PERFORM IMS-STATUSKONTROLL                                           
572700     .                                                                    
572800     SKIP3                                                                
572900 IMS-GNP-WDE401 SECTION.                                                  
573000     MOVE 'IMS-GNP-WDE401'     TO WS-SEKTION                              
573100                                                                          
573200     MOVE 'WDE401'     TO SSA1                                            
573300     MOVE '    ' TO GODK-STATUSKODER                                      
573400     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-WDE401 SSA1                   
573500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
573600     PERFORM IMS-STATUSKONTROLL                                           
573700     .                                                                    
573800     EJECT                                                                
573900 IMS-GNP-WDE421  SECTION.                                                 
574000     MOVE 'IMS-GNP-WDE421'     TO WS-SEKTION                              
574100                                                                          
574200     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
574300          DELIMITED BY SIZE INTO SSA1                                     
574400     MOVE '    ' TO GODK-STATUSKODER                                      
574500     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-WDE421 SSA1                   
574600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
574700     PERFORM IMS-STATUSKONTROLL                                           
574800     .                                                                    
574900     SKIP3                                                                
575000 IMS-GU-WDE401-ESEQ SECTION.                                              
575100     MOVE 'IMS-GU-WDE401-ESEQ'    TO WS-SEKTION                           
575200                                                                          
575300     STRING 'WDE401  (WDE4ESEQ =' W-WDE4ESEQ-X ')'                        
575400          DELIMITED BY SIZE INTO SSA1                                     
575500     MOVE '    ' TO GODK-STATUSKODER                                      
575600     CALL CBLTDLI USING GU WDE4X-PCB DLI-IO-WDE401 SSA1                   
575700     MOVE WDE4X-STATUS-CODE TO STATUS-WS                                  
575800     PERFORM IMS-STATUSKONTROLL                                           
575900     .                                                                    
576000     SKIP3                                                                
576100 IMS-GU-WDE401F-ESEQ SECTION.                                             
576200     MOVE 'IMS-GU-WDE401F-ESEQ'    TO WS-SEKTION                          
576300                                                                          
576400     STRING 'WDE401  *F(WDE4ESEQ =' W-WDE4ESEQ-X ')'                      
576500          DELIMITED BY SIZE INTO SSA1                                     
576600     MOVE '    ' TO GODK-STATUSKODER                                      
576700     CALL CBLTDLI USING GU WDE4X-PCB DLI-IO-WDE401 SSA1                   
576800     MOVE WDE4X-STATUS-CODE TO STATUS-WS                                  
576900     PERFORM IMS-STATUSKONTROLL                                           
577000     .                                                                    
577100     SKIP3                                                                
577200 IMS-GN-WDE401-ESEQ SECTION.                                              
577300     MOVE 'IMS-GN-WDE401-ESEQ'    TO WS-SEKTION                           
577400                                                                          
577500     STRING 'WDE401  (WDE4ESEQ =' W-WDE4ESEQ-X ')'                        
577600          DELIMITED BY SIZE INTO SSA1                                     
577700     MOVE '  GE' TO GODK-STATUSKODER                                      
577800     CALL CBLTDLI USING GN WDE4X-PCB DLI-IO-WDE401 SSA1                   
577900     MOVE WDE4X-STATUS-CODE TO STATUS-WS                                  
578000     PERFORM IMS-STATUSKONTROLL                                           
578100     .                                                                    
578200     SKIP3                                                                
578300 IMS-GNP-WDE411-ESEQ SECTION.                                             
578400     MOVE 'IMS-GNP-WDE411-ESEQ'    TO WS-SEKTION                          
578500                                                                          
578600     MOVE 'WDE411'     TO SSA1                                            
578700     MOVE '    ' TO GODK-STATUSKODER                                      
578800     CALL CBLTDLI USING GNP WDE4X-PCB DLI-IO-WDE411 SSA1                  
578900     MOVE WDE4X-STATUS-CODE TO STATUS-WS                                  
579000     PERFORM IMS-STATUSKONTROLL                                           
579100     .                                                                    
579200     EJECT                                                                
579300 IMS-GU-WDE601 SECTION.                                                   
579400     MOVE 'IMS-GU-WDE601'     TO WS-SEKTION                               
579500                                                                          
579600     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-E6-X ')'                     
579700          DELIMITED BY SIZE INTO SSA1                                     
579800     MOVE '    ' TO GODK-STATUSKODER                                      
579900     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
580000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
580100     PERFORM IMS-STATUSKONTROLL                                           
580200     .                                                                    
580300     EJECT                                                                
580400 IMS-GNP-WDE611   SECTION.                                                
580500     MOVE 'IMS-GNP-WDE611'    TO WS-SEKTION                               
580600                                                                          
580700     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-E6-X ')'                      
580800          DELIMITED BY SIZE INTO SSA1                                     
580900     MOVE '    ' TO GODK-STATUSKODER                                      
581000     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-WDE611 SSA1                   
581100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
581200     PERFORM IMS-STATUSKONTROLL                                           
581300     .                                                                    
581400     EJECT                                                                
581500 IMS-GU-WDK601 SECTION.                                                   
581600     MOVE 'IMS-GU-WDK601'     TO WS-SEKTION                               
581700                                                                          
581800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
581900          DELIMITED BY SIZE INTO SSA1                                     
582000     MOVE '    ' TO GODK-STATUSKODER                                      
582100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
582200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
582300     PERFORM IMS-STATUSKONTROLL                                           
582400     .                                                                    
582500     EJECT                                                                
582600 IMS-GNP-WDK611 SECTION.                                                  
582700     MOVE 'IMS-GNP-WDK611'    TO WS-SEKTION                               
582800                                                                          
582900     MOVE 'WDK611'       TO SSA1                                          
583000     MOVE '    ' TO GODK-STATUSKODER                                      
583100     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
583200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
583300     PERFORM IMS-STATUSKONTROLL                                           
583400     .                                                                    
583500     EJECT                                                                
583600 IMS-GU-WDK711 SECTION.                                                   
583700     MOVE 'IMS-GU-WDK711'    TO WS-SEKTION                                
583800                                                                          
583900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
584000          DELIMITED BY SIZE INTO SSA1                                     
584100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
584200          DELIMITED BY SIZE INTO SSA2                                     
584300     MOVE '  GE' TO GODK-STATUSKODER                                      
584400     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
584500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
584600     PERFORM IMS-STATUSKONTROLL                                           
584700     .                                                                    
584800     EJECT                                                                
584900 IMS-GU-WDK711-CORE SECTION.                                              
585000     MOVE 'IMS-GU-WDK711-CORE'    TO WS-SEKTION                           
585100                                                                          
585200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-CORE-X ')'                    
585300          DELIMITED BY SIZE INTO SSA1                                     
585400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
585500          DELIMITED BY SIZE INTO SSA2                                     
585600     MOVE '  GE' TO GODK-STATUSKODER                                      
585700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711-CORE SSA1 SSA2          
585800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
585900     PERFORM IMS-STATUSKONTROLL                                           
586000     .                                                                    
586100     EJECT                                                                
586200 IMS-GU-WDQ201 SECTION.                                                   
586300     MOVE 'IMS-GU-WDQ201'   TO WS-SEKTION                                 
586400                                                                          
586500     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
586600          DELIMITED BY SIZE INTO SSA1                                     
586700     MOVE '    ' TO GODK-STATUSKODER                                      
586800     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
586900     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
587000     PERFORM IMS-STATUSKONTROLL                                           
587100     .                                                                    
587200     EJECT                                                                
587300 IMS-GU-WDB101  SECTION.                                                  
587400     MOVE 'IMS-GU-WDB101'       TO WS-SEKTION                             
587500                                                                          
587600     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
587700          DELIMITED BY SIZE INTO SSA1                                     
587800     MOVE '    ' TO GODK-STATUSKODER                                      
587900     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
588000     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
588100     PERFORM IMS-STATUSKONTROLL                                           
588200     .                                                                    
588300     SKIP3                                                                
588400 IMS-GU-WDB201  SECTION.                                                  
588500     MOVE 'IMS-GU-WDB201'       TO WS-SEKTION                             
588600                                                                          
588700     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
588800          DELIMITED BY SIZE INTO SSA1                                     
588900     MOVE '    ' TO GODK-STATUSKODER                                      
589000     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
589100     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
589200     PERFORM IMS-STATUSKONTROLL                                           
589300     .                                                                    
589400     SKIP3                                                                
589500 IMS-GU-WDB201A   SECTION.                                                
589600     MOVE 'IMS-GU-WDB201A'       TO WS-SEKTION                            
589700                                                                          
589800     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
589900                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
590000          DELIMITED BY SIZE INTO SSA1                                     
590100     MOVE '    ' TO GODK-STATUSKODER                                      
590200     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
590300     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
590400     PERFORM IMS-STATUSKONTROLL                                           
590500     .                                                                    
590600     SKIP3                                                                
590700 IMS-GU-WDGX4491  SECTION.                                                
590800     MOVE 'IMS-GU-WDGX4491'     TO WS-SEKTION                             
590900                                                                          
591000     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4491-X ')'                    
591100          DELIMITED BY SIZE INTO SSA1                                     
591200     MOVE '  GE' TO GODK-STATUSKODER                                      
591300     CALL CBLTDLI USING GU 4494-PCB DLI-IO-4494 SSA1                      
591400     MOVE 4494-STATUS-CODE TO STATUS-WS                                   
591500     PERFORM IMS-STATUSKONTROLL                                           
591600     .                                                                    
591700     SKIP3                                                                
591800 IMS-GNP-WDGX4494 SECTION.                                                
591900     MOVE 'IMS-GNP-WDGX4494'   TO WS-SEKTION                              
592000                                                                          
592100     STRING 'WDGX4494*F(DALASTN  =' W-DALASTN-X                           
592200                      '&IDGMTREF =' W-IDGMTREF-X                          
592300                      '&IDLBBET  =' W-IDLBBET-X                           
592400                      '&IDKOLLI  =' W-IDKOLLI-4-X ')'                     
592500          DELIMITED BY SIZE INTO SSA1                                     
592600     MOVE '  GE' TO GODK-STATUSKODER                                      
592700     CALL CBLTDLI USING GNP 4494-PCB DLI-IO-4494 SSA1                     
592800     MOVE 4494-STATUS-CODE TO STATUS-WS                                   
592900     PERFORM IMS-STATUSKONTROLL                                           
593000     .                                                                    
593100     SKIP3                                                                
593200 IMS-GU-WDE111 SECTION.                                                   
593300     MOVE 'IMS-GU-WDE111'   TO WS-SEKTION                                 
593400                                                                          
593500     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
593600          DELIMITED BY SIZE INTO SSA1                                     
593700     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
593800          DELIMITED BY SIZE INTO SSA2                                     
593900     MOVE '  GE' TO GODK-STATUSKODER                                      
594000     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE111 SSA1 SSA2               
594100     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
594200     PERFORM IMS-STATUSKONTROLL                                           
594300     .                                                                    
594400     EJECT                                                                
594500 IMS-GU-WDE131   SECTION.                                                 
594600     MOVE 'IMS-GU-WDE131'   TO WS-SEKTION                                 
594700                                                                          
594800     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
594900          DELIMITED BY SIZE INTO SSA1                                     
595000     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
595100          DELIMITED BY SIZE INTO SSA2                                     
595200     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
595300          DELIMITED BY SIZE INTO SSA3                                     
595400     STRING 'WDE131  (IDPURAD  =' W-IDPURAD-X ')'                         
595500          DELIMITED BY SIZE INTO SSA4                                     
595600     MOVE '  ' TO GODK-STATUSKODER                                        
595700     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE131 SSA1 SSA2               
595800                                                  SSA3 SSA4               
595900     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
596000     PERFORM IMS-STATUSKONTROLL                                           
596100     .                                                                    
596200 IMS-GNP-WDE141   SECTION.                                                
596300     MOVE 'IMS-GU-WDE141'   TO WS-SEKTION                                 
596400                                                                          
596500     MOVE 'WDE141 ' TO SSA1                                               
596600     MOVE '  GE' TO GODK-STATUSKODER                                      
596700     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE141 SSA1                   
596800     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
596900     PERFORM IMS-STATUSKONTROLL                                           
597000     .                                                                    
597100 IMS-GNP-WDE122 SECTION.                                                  
597200     MOVE 'IMS-GNP-WDE122'    TO WS-SEKTION                               
597300                                                                          
597400     MOVE 'WDE122'      TO SSA1                                           
597500     MOVE '    ' TO GODK-STATUSKODER                                      
597600     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE122 SSA1                   
597700     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
597800     PERFORM IMS-STATUSKONTROLL                                           
597900     .                                                                    
598000     EJECT                                                                
598100 IMS-GU-WDB601    SECTION.                                                
598200                                                                          
598300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
598400          DELIMITED BY SIZE INTO SSA1                                     
598500     MOVE '  GE' TO GODK-STATUSKODER                                      
598600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
598700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
598800     PERFORM IMS-STATUSKONTROLL                                           
598900     IF SEGMENT-SAKNAS                                                    
599000         MOVE SPACE TO DCS-KDDC                                           
599100                       DCS-IDVAT                                          
599200     END-IF                                                               
599300     .                                                                    
599400     SKIP3                                                                
599500 DB2-SELECT-TP4TRAN SECTION.                                              
599600     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
599700                                                                          
599800     MOVE 000100 TO GODK-SQLCODEKODER                                     
599900                                                                          
600000     EXEC SQL                                                             
600100           SELECT  DISTINCT                                               
600200                   IDDC_REC                                               
600300                                                                          
600400           INTO   :TP4TRAN-IDDC-REC                                       
600500                                                                          
600600           FROM    TP4TRAN                                                
600700                                                                          
600800           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
600900     END-EXEC                                                             
601000                                                                          
601100     MOVE SQLCODE TO SQLCODE-WS                                           
601200     PERFORM DB2-STATUSKONTROLL                                           
601300     .                                                                    
601400     EJECT                                                                
601500 IMS-STATUSKONTROLL SECTION.                                              
601600                                                                          
601700     SET STATUS-IX TO 1                                                   
601800     SEARCH GODK-STATUS                                                   
601900       AT END                                                             
602000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
602100           DELIMITED BY SIZE INTO FELTEXT                                 
602200         CALL FELLOG                                                      
602300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
602400         CONTINUE                                                         
602500     END-SEARCH                                                           
602600     .                                                                    
602700     EJECT                                                                
602800 DB2-STATUSKONTROLL  SECTION.                                             
602900                                                                          
603000     SET SQLCODE-IX TO 1                                                  
603100     SEARCH GODK-SQLCODE                                                  
603200       AT END                                                             
603300          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
603400          DELIMITED BY SIZE INTO FELTEXT                                  
603500          CALL ABEND USING RKOD-ABEND-DB2                                 
603600       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
603700     END-SEARCH                                                           
603800     .                                                                    
