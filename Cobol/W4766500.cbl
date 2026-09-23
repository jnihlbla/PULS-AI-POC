000100*********************************************                             
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4766500.                                                
000400 AUTHOR.         MOGREN STINA.                                            
000500 DATE-WRITTEN.   03/01/24.                                                
000600 DATE-COMPILED.                                                           
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        INVOICE INFO COD/IMP                                             
001000*                                                                         
001100*        PROGRAMMET STARTAS EFTER BMP W4766800                            
001200*        I SOP-RUTIN W476S5                                               
001300*        RADPOSTER FRÅN BILLIT ÄR 'PARAMETER' IN                          
001400*        LÄS POST/ER PER RAD PÅ W47665                                    
001500*                                                                         
001600*        PROGRAMMET LÄSER         WDB1, WDK6                              
001700*                                 WDB2  KUNDREG                           
001800*        PROGRAMMET UPPDATERAR    WDK6 (ARTIKELREG CDC)                   
001900*                                 WDK7 (ARTIKELREG *DC)                   
002000*                                 WDL9 (SALDOREG-SOL-LOGGAR)              
002100*                                 WDM7 (TULLREG)                          
002200*                                 WDM8 (TULLREG)                          
002300*                                 WDL5 (FAKTHIST)                         
002400*                                 WDR8 (TRANSAR BATCH-EKONOMI)            
002500*                                 HÄNDELSE REGISTER (CHKPOINT)            
002600*                                 WL4579-(WDGX)                           
002700*                                 WDA9 UPPFÖLJNING BYTES                  
002800*                                 WDR4-3161-WDGX3162                      
002900*                                       RENOVATOR CONFIRMATION            
003000*                                                                         
003100*    ABENDKODER:                                                          
003200*        U0016 -  . . . .                                                 
003300*        U1000 -  . . . .                                                 
003400*                                                                         
003500*    CHANGE LOG:                                                          
003600*                                                                         
003700*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
003800*      ----------------------------------------------------------         
003900*      14/11/28 - REDDY RAHUL     - REMOVE SCRAP QTY FIELDS               
004000*                                   E'TRACKER 10193018                    
004100*      22/10/17 - CAMELIA O.      - ADD DC.24 TO THE CUSTOMES DB.         
004200*                                   STORY 2942753                         
004300*                                                                         
004400                                                                          
004500     SKIP3                                                                
004600 ENVIRONMENT DIVISION.                                                    
004700     SKIP2                                                                
004800 INPUT-OUTPUT SECTION.                                                    
004900                                                                          
005000 FILE-CONTROL.                                                            
005100     SKIP2                                                                
005200*          --- FIL MED RAD-POSTER FRÅN W4766800                           
005300     SELECT W47665                     ASSIGN TO W47665D1.                
005400     SKIP2                                                                
005500 DATA DIVISION.                                                           
005600     SKIP2                                                                
005700 FILE SECTION.                                                            
005800     SKIP3                                                                
005900 FD  W47665                                                               
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200                                                                          
006300*01  POST -COPY W4766501  -PRE  UT-  -L.                                  
006400     EJECT                                                                
006500                                                                          
006600 WORKING-STORAGE SECTION.                                                 
006700                                                                          
006800 77  IDPGM                       PIC X(8)    VALUE 'W4766500'.            
006900 77  JA                          PIC X       VALUE 'J'.                   
007000 77  NEJ                         PIC X       VALUE 'N'.                   
007100                                                                          
007200 01  -COPY WWDCKONS                                                       
007300 01  -COPY WWDCLAND                                                       
007400 01  -COPY WWDC99                                                         
007500                                                                          
007600 77  W47665-EOF-SW               PIC X       VALUE 'N'.                   
007700     88  END-OF-W47665                       VALUE 'J'.                   
007800                                                                          
007900 77  FIRST-POST-SW               PIC X.                                   
008000     88  FIRST-POST                          VALUE 'J'.                   
008100     88  EJ-FIRST-POST                       VALUE 'N'.                   
008200                                                                          
008300 77  TILLAGG-SW                  PIC X       VALUE 'N'.                   
008400     88  TILLAGG-JA                          VALUE 'J'.                   
008500     88  TILLAGG-NEJ                         VALUE 'N'.                   
008600                                                                          
008700 77  W-ANT-POST                  PIC S9(7)   VALUE ZERO COMP-3.           
008800 77  W-ANT-POST-FORBI            PIC S9(7)   VALUE ZERO COMP-3.           
008900 01  W-RAKNARE                   PIC S9(7)   VALUE ZERO COMP-3.           
009000 01  WS-KVLS                     PIC S9(7)   VALUE ZERO COMP-3.           
009100*77  IX                          PIC S9(7)   VALUE ZERO COMP-3.           
009200 77  IX                          PIC 9(2)    VALUE ZERO.                  
009300 77  IX2                         PIC 9(2)    VALUE ZERO.                  
009400 77  INDX                        PIC 9(3)    VALUE ZERO.                  
009500 77  SAVE-IDSHIPM                PIC 9(7)    VALUE ZERO.                  
009600 77  SAVE-IDFAKT                 PIC 9(7)    VALUE ZERO.                  
009700                                                                          
009800 01  ERRTEXT.                                                             
009900     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
010000     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
010100                                                                          
010200*    --- ARBETSFÄLT FÖR BERÄKNING AV DAT./TID                             
010300 77  WS-AAAAMMDD                 PIC 9(8)    VALUE ZERO.                  
010400 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
010500 01  WS-DAAAPP                   PIC 9(6)    VALUE 200000.                
010600                                                                          
010700 01  WS-TIKLOCK-R8               PIC S9(9)   VALUE ZERO COMP-3.           
010800 01  WS-IDKUNDRF.                                                         
010900     03 WS-IDKUNDRF-1-5          PIC  9(5).                               
011000     03 FILLER                   PIC  X(5).                               
011100 01 WS-IDARTNR                   PIC  9(9).                               
011200 01 WS-IDRADNR                   PIC  9(5).                               
011300 01 WS-SPAR-IDKUNDRF-1-5         PIC  9(5)   VALUE ZERO.                  
011400                                                                          
011500 01  SPAR-DAFAKT                 PIC 9(8)    VALUE ZERO.                  
011600 01  FILLER                      REDEFINES SPAR-DAFAKT.                   
011700   03  SPAR-SEKEL                PIC 9(2).                                
011800   03  SPAR-AAMMDD               PIC 9(6).                                
011900 01  SPAR-TIFAKTID               PIC S9(7)   VALUE ZERO COMP-3.           
012000 01  SPAR-IDKOLLI                PIC S9(5)   VALUE ZERO COMP-3.           
012100 01  SPAR-IDARTNR                PIC S9(9)   VALUE ZERO COMP-3.           
012200*                                                                         
012300 01  KONSTANTER.                                                          
012400     03  GEN-IDSTATNR            PIC S9(9) COMP-3 VALUE 87089997.         
012500                                                                          
012600 01  W-VKORDBTO-ORDER-LB        PIC S9(8)V9(1) COMP-3 VALUE ZERO.         
012700 01  W-VKORDBTO-ORDER           PIC S9(8)V9(1) COMP-3 VALUE ZERO.         
012800 01  SPAR-PRFRAKT-LOC            PIC 9(7)V9(2)  VALUE ZERO.               
012900                                                                          
013000 01  WS-BEL                     PIC S9(11)V9(2) COMP-3 VALUE ZERO.        
013100                                                                          
013200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
013300 01  FILLER REDEFINES DAGENS-DATUM.                                       
013400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
013500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
013600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
013700 01  TIDPUNKT                    PIC 9(8)    VALUE ZERO.                  
013800     EJECT                                                                
013900 01  CHKP-VAR.                                                            
014000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
014100     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
014200     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
014300     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
014400     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
014500     03 CHKP-MAX                 PIC S9(3)   VALUE +150 COMP-3.           
014600                                                                          
014700 01  WS-IDDC-SEND                PIC X(2)    VALUE SPACE.                 
014800 01  WS-IDDC-REC                 PIC X(2)    VALUE SPACE.                 
014900 01  WS-IDDC-SPAR                PIC X(2)    VALUE SPACE.                 
015000 01  WS-IDCOM                    PIC S9(9)   VALUE ZERO COMP-3.           
015100 01  WS-PRAVCOST-OLD             PIC S9(7)V9(2) COMP-3.                   
015200*                                                                         
015300     EJECT                                                                
015400 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
015500*01  FILLER        -COPY WWDIST18   -RED  TEST-IDDISTR.                   
015600                                                                          
015700*01  FILLER        -COPY WWDIST42   -RED  TEST-IDDISTR.                   
015800                                                                          
015900*01  FILLER        -COPY WWDIST07   -RED  TEST-IDDISTR.                   
016000                                                                          
016100*01  FILLER        -COPY WWDIST35   -RED  TEST-IDDISTR.                   
016200                                                                          
016300*01  FILLER        -COPY WWDIST79   -RED  TEST-IDDISTR.                   
016400                                                                          
016500*01  FILLER        -COPY WWDIS134   -RED  TEST-IDDISTR.                   
016600     SKIP2                                                                
016700 01  TEST-IDARTNR              PIC 9(9)  COMP-3.                          
016800*01  FILLER -COPY WWBYT19    -RED TEST-IDARTNR                            
016900*01  FILLER -COPY WWBYT03    -RED TEST-IDARTNR                            
017000     EJECT                                                                
017100                                                                          
017200 01  DYNAMISKA-SUBPROGRAM.                                                
017300*                                                                         
017400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
017500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
017800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017900     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
018000     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
018100     03  W005WDL7                PIC X(8)    VALUE 'W005WDL7'.            
018200     03  W510AVG                 PIC X(8)    VALUE 'W510AVG '.            
018300*        BERÄKNA MEDELVÄRDE PRAVCOST                                      
018400     SKIP2                                                                
018500                                                                          
018600 01  WS-DISP                     PIC 9(12)   VALUE ZERO.                  
018700 01  W-KEY-KDTULLVE              PIC S9(1)   VALUE ZERO COMP-3.           
018800                                                                          
018900 01  WRO-IDKUNDRF                PIC X(10) VALUE '00000     '.            
019000                                                                          
019100*    --- PARAMETRAR TILL ABEND                                            
019200                                                                          
019300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
019400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
019500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
019600     SKIP2                                                                
019700 01  FELTEXT.                                                             
019800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
019900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
020000 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
020100                                                                          
020200 01  WS-PRKURS               PIC S9(6)V9(5)  VALUE ZERO COMP-3.           
020300                                                                          
020400 01  WS2-NYCKLAR.                                                         
020500     03  WS2-IDFAKT              PIC S9(7)  VALUE ZERO  COMP-3.           
020600     03  WS2-IDDISTR             PIC S9(5)  VALUE ZERO  COMP-3.           
020700     03  WS2-IDKUNDNR            PIC S9(7)  VALUE ZERO  COMP-3.           
020800     03  WS2-IDORDNR7            PIC S9(7)  VALUE ZERO  COMP-3.           
020900     03  WS2-IDPRODNR            PIC S9(7)  VALUE ZERO  COMP-3.           
021000     03  WS2-IDKOLLI             PIC S9(5)  VALUE ZERO  COMP-3.           
021100                                                                          
021200*    --- PARAMETRAR TILL W930VAL                                          
021300*01  -COPY W930VAL                                                        
021400                                                                          
021500*    --- PARAMETRAR TILL W411EXCH                                         
021600*01  -COPY W411EXCH                                                       
021700                                                                          
021800 01 FILLER                       PIC X(8)    VALUE  'W005WDK7'.           
021900*   -COPY W005WDK7                                                        
022000     EJECT                                                                
022100 01 FILLER                       PIC X(8)    VALUE  'W005WDL7'.           
022200*   -COPY W005WDL7                                                        
022300     EJECT                                                                
022400 01 FILLER                       PIC X(8)    VALUE 'W510AVG '.            
022500*   -COPY W510AVG                                                         
022600     EJECT                                                                
022700 01 FILLER                       PIC X(8)    VALUE 'WWIDFTG '.            
022800*   -COPY WWIDFTG                                                         
022900     EJECT                                                                
023000 01  FILLER                      PIC X(16)    VALUE                       
023100                                              'W510A13         '.         
023200*01 -COPY W510A13               -PRE EKOTRA13-                            
023300     EJECT                                                                
023400                                                                          
023500*    --- PARAMETRAR TILL POSTSUM                                          
023600*                                                                         
023700*01  -COPY W0005   -PRE  POSTSUM-                                         
023800     EJECT                                                                
023900*01  -COPY WDATAREA                                                       
024000*        PARAMETRAR TILL WWOMVAND                                         
024100*    -COPY WWOMVAND                                                       
024200     EJECT                                                                
024300*********                                                                 
024400 01  FILLER                      PIC X(16) VALUE 'BILL-W4766501'.         
024500                                                                          
024600 01  IN-AREA.                                                             
024700   03  -COPY W4766501                                                     
024800                                                                          
024900     EJECT                                                                
025000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025100*                                                                         
025200     EJECT                                                                
025300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025400     SKIP3                                                                
025500 01  NYCKLAR-TILL-DLI.                                                    
025600     03  W-WDGXKEY-MIN-X.                                                 
025700         05  W-IDFAKT-MIN        PIC S9(7)    VALUE ZERO COMP-3.          
025800         05  W-IDDISTR-MIN       PIC S9(5)    VALUE ZERO COMP-3.          
025900         05  W-IDKUNDNR-MIN      PIC S9(7)    VALUE ZERO COMP-3.          
026000         05  W-IDORDER-MIN       PIC S9(7)    VALUE ZERO COMP-3.          
026100         05  W-IDPRODNR-MIN      PIC S9(7)    VALUE ZERO COMP-3.          
026200         05  W-IDPURAD-MIN       PIC S9(5)    VALUE ZERO COMP-3.          
026300                                                                          
026400     03  W-WDGXKEY-MAX-X.                                                 
026500         05  W-IDFAKT-MAX        PIC S9(7)    VALUE ZERO COMP-3.          
026600         05  W-IDDISTR-MAX       PIC S9(5)    VALUE ZERO COMP-3.          
026700         05  W-IDKUNDNR-MAX      PIC S9(7)    VALUE ZERO COMP-3.          
026800         05  W-IDORDER-MAX       PIC S9(7)    VALUE ZERO COMP-3.          
026900         05  W-IDPRODNR-MAX      PIC S9(7)    VALUE ZERO COMP-3.          
027000         05  W-IDPURAD-MAX       PIC S9(5)    VALUE ZERO COMP-3.          
027100                                                                          
027200     03  W-IDSHIPM-X.                                                     
027300         05  W-IDSHIPM           PIC  9(7)   VALUE ZERO.                  
027400     03  W-WDE111KY-X.                                                    
027500         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
027600         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
027700     03  W-WDE121KY-X.                                                    
027800         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
027900         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
028000     03  W-IDPURAD-X.                                                     
028100         05  W-IDPURAD           PIC S9(5)   VALUE ZERO COMP-3.           
028200                                                                          
028300     03  W-IDORDER-X.                                                     
028400         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
028500     03  W-IDARTNR-X.                                                     
028600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
028700     03  W-IDARTNR-CORE-X.                                                
028800         05  W-IDARTNR-CORE      PIC S9(9)   VALUE ZERO COMP-3.           
028900     03   W-KDSEGKEY-X.                                                   
029000         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
029100     03   W-IDDC-X.                                                       
029200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
029300     03   W-IDLAND-X.                                                     
029400         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
029500                                                                          
029600     03  W-WDB101KY-X.                                                    
029700         05  W-IDPARTNR          PIC X(9)    VALUE SPACE.                 
029800         05  W-IDFTG             PIC 9(2)    VALUE ZERO.                  
029900                                                                          
030000     03  W-IDGMT-X.                                                       
030100         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO COMP-3.           
030200         05  W-IDKUNDNR-B2       PIC S9(7)   VALUE ZERO COMP-3.           
030300                                                                          
030400    03  W-WDM701KY-X.                                                     
030500        05  W-IDFAKT-M7          PIC S9(7)   VALUE ZERO COMP-3.           
030600        05  W-IDORDNR-M7         PIC S9(7)   VALUE ZERO COMP-3.           
030700        05  W-IDKOLLI-M7         PIC S9(5)   VALUE ZERO COMP-3.           
030800        05  W-IDPRODNR-M7        PIC S9(7)   VALUE ZERO COMP-3.           
030900    03  W-WDM801KY-X.                                                     
031000        05  W-IDFAKT-M8          PIC S9(7)   VALUE ZERO COMP-3.           
031100        05  W-IDPRODNR-M8        PIC S9(7)   VALUE ZERO COMP-3.           
031200        05  W-IDKOLLI-M8         PIC S9(5)   VALUE ZERO COMP-3.           
031300        05  W-IDARTNR-M8         PIC S9(9)   VALUE ZERO COMP-3.           
031400        05  W-IDRADNR-M8         PIC S9(5)   VALUE ZERO COMP-3.           
031500     03  W-WDGXKEY-4579-X.                                                
031600       05  W-IDHTYP-4579         PIC X(4)    VALUE '4579'.                
031700       05  W-IDPGM-4579          PIC X(8)    VALUE 'W4766500'.            
031800       05  FILLER                PIC X(18)   VALUE LOW-VALUE.             
031900     03  W-IDDC-B6-X.                                                     
032000       05  W-IDDC-B6             PIC X(2)    VALUE SPACE.                 
032100                                                                          
032200     03  W-IDARTNR-WDA9-X.                                                
032300         05  W-IDARTNR-WDA9      PIC S9(9)  COMP-3.                       
032400     03  W-DAAAPP-WDA9-X.                                                 
032500         05  W-DAAAPP-WDA9       PIC  9(6).                               
032600                                                                          
032700     03  W-3161-WDGXKEY-X.                                                
032800         05  W-3161-IDHTYP       PIC  X(4)  VALUE '3161'.                 
032900         05  W-3161-IDDISTR      PIC S9(5)  COMP-3.                       
033000         05  FILLER              PIC  X(23) VALUE LOW-VALUE.              
033100     03  W-3162-WDGXKEY-X.                                                
033200         05  W-3162-DAORDREG     PIC  9(8).                               
033300         05  W-3162-IDORDER      PIC S9(7)  COMP-3.                       
033400         05  W-3162-IDARTNR      PIC S9(9)  COMP-3.                       
033500                                                                          
033600     03  W-IDFAKT-X.                                                      
033700         05  W-IDFAKT            PIC S9(7)   VALUE ZERO COMP-3.           
033800                                                                          
033900     03  W-WDL511KY-X.                                                    
034000         05  W-IDPRODNR-L511     PIC S9(7)   VALUE ZERO COMP-3.           
034100         05  W-IDKOLLI-L511      PIC S9(5)   VALUE ZERO COMP-3.           
034200                                                                          
034300     03  W-WDL521KY-X.                                                    
034400         05  W-IDARTNR-L521      PIC S9(9)   VALUE ZERO COMP-3.           
034500         05  W-IDPURAD-L521      PIC S9(5)   VALUE ZERO COMP-3.           
034600*                                                                         
034700     SKIP2                                                                
034800*    --- STATUS-KOD FRÅN IMS                                              
034900 01  STATUS-WS                   PIC XX.                                  
035000     88  SEGMENT-FINNS                       VALUE '  '.                  
035100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
035200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
035300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
035400     88  IMS-NOT-OK                          VALUE 'XD'.                  
035500     SKIP2                                                                
035600 01  GODK-STATUSKODER.                                                    
035700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
035800     SKIP3                                                                
035900 01  SSA1                        PIC X(96).                               
036000 01  SSA2                        PIC X(96).                               
036100 01  SSA3                        PIC X(96).                               
036200     EJECT                                                                
036300*    --- IMS FUNKTIONSKODER                                               
036400*01  -COPY W0003                                                          
036500     EJECT                                                                
036600*    ---  DLI INPUT-OUTPUT AREA                                           
036700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK601'.         
036800 01  DLI-IO-WDK601.                                                       
036900*    03  -COPY WDK601                                                     
037000     SKIP2                                                                
037100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK611'.         
037200 01  DLI-IO-WDK611.                                                       
037300*    03  -COPY WDK611                                                     
037400     SKIP2                                                                
037500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
037600 01  DLI-IO-WDK711.                                                       
037700*    03  -COPY WDK711                                                     
037800     SKIP2                                                                
037900 01  FILLER                      PIC X(16) VALUE 'DLI-WDK711-CO'.         
038000 01  DLI-IO-WDK711-CORE.                                                  
038100*    03  -COPY WDK711     -PRE CORE-                                      
038200     SKIP2                                                                
038300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK712'.         
038400 01  DLI-IO-WDK712.                                                       
038500*    03  -COPY WDK712                                                     
038600     SKIP2                                                                
038700                                                                          
038800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB101'.         
038900 01  DLI-IO-WDB101.                                                       
039000*    03  -COPY WDB101                                                     
039100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB201'.         
039200 01  DLI-IO-WDB201.                                                       
039300*     03 -COPY WDB201.                                                    
039400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDM701'.         
039500 01  DLI-IO-WDM701.                                                       
039600*    03  -COPY WDM701                                                     
039700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDM801'.         
039800 01  DLI-IO-WDM801.                                                       
039900*    03  -COPY WDM801                                                     
040000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDL901'.         
040100 01  DLI-IO-WDL901.                                                       
040200*    03  -COPY WDL901                                                     
040300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDR801'.         
040400 01  DLI-IO-WDR801.                                                       
040500*    03  -COPY WDR801 -PRE R8-                                            
040600*01  -COPY  WDGX01                                                        
040700     EJECT                                                                
040800 01  FILLER                      PIC X(16) VALUE '4580-IO-AREA'.          
040900 01  4580-IO-AREA.                                                        
041000*03  FILLER  -COPY WDGX4580                                               
041100     EJECT                                                                
041200 01  FILLER                      PIC X(20) VALUE 'DLI-IO-WDE101'.         
041300 01  DLI-IO-WDE101.                                                       
041400     03  -COPY WDE101                                                     
041500     EJECT                                                                
041600 01  FILLER                      PIC X(20) VALUE 'DLI-IO-WDE111'.         
041700 01  DLI-IO-WDE111.                                                       
041800     03  -COPY WDE111                                                     
041900     EJECT                                                                
042000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE121'.         
042100 01  DLI-IO-WDE121.                                                       
042200*    03  -COPY WDE121                                                     
042300     EJECT                                                                
042400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDE122'.         
042500 01  DLI-IO-WDE122.                                                       
042600*    03  -COPY WDE122                                                     
042700     EJECT                                                                
042800 01  FILLER                      PIC X(16) VALUE 'WDB601 AREA'.           
042900 01  DLI-IO-AREA-B601.                                                    
043000*    03  -COPY WDB601                                                     
043100     EJECT                                                                
043200 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDA901'.         
043300 01  DLI-IO-WDA901.                                                       
043400*    03  -COPY WDA901                                                     
043500     EJECT                                                                
043600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDA912'.         
043700 01  DLI-IO-WDA912.                                                       
043800*    03  -COPY WDA912                                                     
043900     EJECT                                                                
044000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-3161'.           
044100 01  DLI-IO-WDGX3161.                                                     
044200*    03  -COPY WDGX3161                                                   
044300     EJECT                                                                
044400 01  FILLER                      PIC X(16) VALUE 'DLI-IO-3162'.           
044500 01  DLI-IO-WDGX3162.                                                     
044600*    03  -COPY WDGX3162                                                   
044700     EJECT                                                                
044800 01  FILLER                      PIC X(20) VALUE 'DLI-IO-WDL501'.         
044900 01  DLI-IO-WDL501.                                                       
045000     03  -COPY WDL501                                                     
045100     EJECT                                                                
045200 01  FILLER                      PIC X(20) VALUE 'DLI-IO-WDL511'.         
045300 01  DLI-IO-WDL511.                                                       
045400     03  -COPY WDL511                                                     
045500     EJECT                                                                
045600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDL521'.         
045700 01  DLI-IO-WDL521.                                                       
045800*    03  -COPY WDL521                                                     
045900     EJECT                                                                
046000 01  IO-PCB                      PIC X.                                   
046100 LINKAGE SECTION.                                                         
046200                                                                          
046300*01  -COPY W0009   -PRE MSG-                                              
046400                                                                          
046500                                                                          
046600*01  -COPY W0008  -PRE WDB1-                                              
046700     05  FILLER                  PIC X.                                   
046800*01  -COPY W0008  -PRE 9305-                                              
046900     05  FILLER                  PIC X.                                   
047000*01  -COPY W0008  -PRE AVG-WDB6-                                          
047100     05  FILLER                  PIC X.                                   
047200*01  -COPY W0008  -PRE WDK6-                                              
047300     05  FILLER                  PIC X.                                   
047400*01  -COPY W0008  -PRE WDK7-                                              
047500     05  FILLER                  PIC X.                                   
047600*01  -COPY W0008  -PRE WDL9-                                              
047700     05  FILLER                  PIC X.                                   
047800*01  -COPY W0008  -PRE WDM7-                                              
047900     05  FILLER                  PIC X.                                   
048000*01  -COPY W0008  -PRE WDM8-                                              
048100     05  FILLER                  PIC X.                                   
048200*01  -COPY W0008  -PRE WDR8-                                              
048300     05  FILLER                  PIC X.                                   
048400*01  -COPY W0008  -PRE 4579-                                              
048500     05  FILLER                  PIC X.                                   
048600*01  -COPY W0008  -PRE WDE1-                                              
048700     05  FILLER                  PIC X.                                   
048800*01  -COPY W0008  -PRE WDB6-                                              
048900     05  FILLER                  PIC X.                                   
049000*01  -COPY W0008  -PRE WDA9-                                              
049100     05  FILLER                  PIC X.                                   
049200*01  -COPY W0008  -PRE 3161-                                              
049300     05  FILLER                  PIC X.                                   
049400*01  -COPY W0008  -PRE OIGA-                                              
049500     05  FILLER                  PIC X.                                   
049600*01  -COPY W0008  -PRE WDB2-                                              
049700     05  FILLER                  PIC X.                                   
049800*01  -COPY W0008  -PRE WDL5-                                              
049900     05  FILLER                  PIC X.                                   
050000     EJECT                                                                
050100 PROCEDURE DIVISION  USING MSG-PCB                                        
050200                           WDB1-PCB                                       
050300                           9305-PCB                                       
050400                           AVG-WDB6-PCB                                   
050500                           WDK6-PCB                                       
050600                           WDK7-PCB                                       
050700                           WDL9-PCB                                       
050800                           WDM7-PCB                                       
050900                           WDM8-PCB                                       
051000                           WDR8-PCB                                       
051100                           4579-PCB                                       
051200                           WDE1-PCB                                       
051300                           WDB6-PCB                                       
051400                           WDA9-PCB                                       
051500                           3161-PCB                                       
051600                           OIGA-PCB                                       
051700                           WDB2-PCB                                       
051800                           WDL5-PCB.                                      
051900 MAIN SECTION.                                                            
052000     ENTRY 'DLITCBL' USING MSG-PCB                                        
052100                           WDB1-PCB                                       
052200                           9305-PCB                                       
052300                           AVG-WDB6-PCB                                   
052400                           WDK6-PCB                                       
052500                           WDK7-PCB                                       
052600                           WDL9-PCB                                       
052700                           WDM7-PCB                                       
052800                           WDM8-PCB                                       
052900                           WDR8-PCB                                       
053000                           4579-PCB                                       
053100                           WDE1-PCB                                       
053200                           WDB6-PCB                                       
053300                           WDA9-PCB                                       
053400                           3161-PCB                                       
053500                           OIGA-PCB                                       
053600                           WDB2-PCB                                       
053700                           WDL5-PCB.                                      
053800                                                                          
053900                                                                          
054000     PERFORM A-INIT                                                       
054100                                                                          
054200     PERFORM IMS-RESTART                                                  
054300     PERFORM IMS-LAS-ATERSTART                                            
054400                                                                          
054500     IF SEGMENT-SAKNAS                                                    
054600        MOVE SPACE        TO 4580-WDGX4580-CTX                            
054700        MOVE '1'          TO 4580-KDSEGKEY                                
054800        MOVE ZERO         TO 4580-KVPOST                                  
054900        MOVE DAGENS-DATUM TO 4580-TIUPPDAT                                
055000        MOVE TIDPUNKT     TO 4580-TIUPPTID                                
055100                                                                          
055200        PERFORM IMS-ISRT-ATERSTART                                        
055300        PERFORM IMS-LAS-ATERSTART                                         
055400     END-IF                                                               
055500                                                                          
055600     IF 4580-KVPOST > +0                                                  
055700        PERFORM B-LAES-FRAM-TILL-CHKPOINT                                 
055800     ELSE                                                                 
055900        PERFORM S01-LAES-W47665                                           
056000        MOVE +1 TO W-ANT-POST                                             
056100     END-IF                                                               
056200                                                                          
056300     PERFORM UNTIL (END-OF-W47665)                                        
056400                                                                          
056500       PERFORM B-VILKA-ARTIKLAR                                           
056600                                                                          
056700       IF TILLAGG-NEJ                                                     
056800         PERFORM L-UPPDATERA-ARTIKEL-TULL                                 
056900       END-IF                                                             
057000                                                                          
057100       PERFORM M-UPPDATERA-FAKT-HIST                                      
057200                                                                          
057300       PERFORM S01-LAES-W47665                                            
057400       ADD +1           TO W-ANT-POST                                     
057500       IF  NOT END-OF-W47665                                              
057600       AND W-ANT-POST > CHKP-MAX                                          
057700            PERFORM C-TAG-CHECKPOINT                                      
057800            MOVE +1 TO W-ANT-POST                                         
057900       END-IF                                                             
058000                                                                          
058100     END-PERFORM                                                          
058200                                                                          
058300                                                                          
058400     PERFORM Z-FINIT                                                      
058500                                                                          
058600     MOVE ZERO TO RETURN-CODE                                             
058700     GOBACK                                                               
058800     .                                                                    
058900     EJECT                                                                
059000 A-INIT SECTION.                                                          
059100     MOVE 'A-INIT'              TO WS-SEKTION                             
059200                                                                          
059300                                                                          
059400     OPEN INPUT W47665                                                    
059500                                                                          
059600                                                                          
059700     ACCEPT DAGENS-DATUM         FROM DATE                                
059800     ACCEPT TIDPUNKT             FROM TIME                                
059900     ACCEPT WS-TIKLOCK-R8        FROM TIME                                
060000     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
060100                                                                          
060200     MOVE SPACE                  TO BET-IDPARTNR                          
060300                                                                          
060400     MOVE ZERO                   TO R8-FIL-IDSEKVNR                       
060500                                                                          
060600     MOVE JA                     TO FIRST-POST-SW                         
060700     .                                                                    
060800     EJECT                                                                
060900 B-LAES-FRAM-TILL-CHKPOINT  SECTION.                                      
061000     MOVE 'B-LAES-FRAM-TILL-CHK' TO WS-SEKTION                            
061100                                                                          
061200     MOVE +0                   TO W-ANT-POST-FORBI                        
061300     PERFORM S01-LAES-W47665                                              
061400                                                                          
061500     PERFORM UNTIL END-OF-W47665                                          
061600                OR W-ANT-POST-FORBI = 4580-KVPOST                         
061700        PERFORM S01-LAES-W47665                                           
061800        ADD +1                 TO W-ANT-POST-FORBI                        
061900     END-PERFORM                                                          
062000                                                                          
062100     IF END-OF-W47665                                                     
062200        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
062300                      TO FELTEXT                                          
062400        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
062500     ELSE                                                                 
062600        MOVE +1  TO W-ANT-POST                                            
062700     END-IF                                                               
062800     .                                                                    
062900     EJECT                                                                
063000 B-VILKA-ARTIKLAR   SECTION.                                              
063100     MOVE 'B-VILKA-ARTIKLAR'     TO WS-SEKTION                            
063200                                                                          
063300     MOVE BILL-IDDISTR           TO TEST-IDDISTR                          
063400     IF BILL-IDDC NOT = DCS-IDDC                                          
063500        MOVE BILL-IDDC           TO W-IDDC-B6                             
063600        PERFORM IMS-GU-WDB601                                             
063700     END-IF                                                               
063800                                                                          
063900                                                                          
064000     MOVE NEJ                    TO  TILLAGG-SW                           
064100                                                                          
064200     IF  BILL-BEART  = 'FREIGHT' OR 'INSURANCE' OR                        
064300                       'LEGAL' OR                                         
064400                       'PACKING & HANDLING' OR                            
064500                       'REDUCTION' OR                                     
064600                       'SERVICE FEE'                                      
064700       MOVE JA                   TO TILLAGG-SW                            
064800     END-IF                                                               
064900                                                                          
065000     .                                                                    
065100     EJECT                                                                
065200 C-TAG-CHECKPOINT SECTION.                                                
065300     MOVE 'C-TAG-CHECKPOINT'      TO WS-SEKTION                           
065400                                                                          
065500*    UPPDATERA ÅTERSTARTREGISTRET                                         
065600     PERFORM IMS-LAS-ATERSTART                                            
065700     ADD  CHKP-MAX                TO 4580-KVPOST                          
065800     ACCEPT 4580-TIUPPDAT FROM DATE                                       
065900     ACCEPT 4580-TIUPPTID FROM TIME                                       
066000                                                                          
066100     PERFORM IMS-REPL-ATERSTART                                           
066200                                                                          
066300*    TAG CHECKPOINT                                                       
066400     PERFORM IMS-CHECKPOINT                                               
066500     .                                                                    
066600     EJECT                                                                
066700 L-UPPDATERA-ARTIKEL-TULL  SECTION.                                       
066800*    ARTIKELREG.(K6/K7) + TULLREG.                                        
066900     MOVE 'L-UPPDATERA-ARTIKEL-TULL'  TO WS-SEKTION                       
067000                                                                          
067100     IF BILL-FLLSBOK = JA                                                 
067200*                                                                         
067300*      INGEN UPPDATERING FÖR STUDSFLÖDET ANDRA FAKTURAN                   
067400*     DIST35-NONVCC-NONVCC-REFILL AND DCS-SWEDEN**                        
067500       IF BILL-FLSTUDSUP = NEJ                                            
067600         CONTINUE                                                         
067700       ELSE                                                               
067800         PERFORM LA-UPPDATERA-ARTIKELREG                                  
067900       END-IF                                                             
068000     ELSE                                                                 
068100       PERFORM LAK-NDC-A13                                                
068200     END-IF                                                               
068300                                                                          
068400     MOVE BILL-IDDISTR            TO TEST-IDDISTR                         
068500     IF BILL-IDDC NOT = DCS-IDDC                                          
068600        MOVE BILL-IDDC            TO W-IDDC-B6                            
068700        PERFORM IMS-GU-WDB601                                             
068800     END-IF                                                               
068900*                                                                         
069000* För BREXIT --> DIST42-EJ-EU-MIC ISTF DIST42-EJ-EU                       
069100*                                                                         
069200     IF ((DIST42-EJ-EU-MIC) AND (DCS-CDC OR                               
069300                                 DCS-DDC AND DCS-SWEDEN)                  
069400                                                                          
069500**      AND  BILL-KDTULLVE     = +0                                       
069600        AND (BILL-KDFAKTYP     = 'R' OR 'G' OR 'K')                       
069700        AND  BILL-KDFRAKT  NOT = 88                                       
069800        AND  BILL-FLOVRLEV NOT = JA                                       
069900        AND  BILL-FLORDSPE NOT = JA)                                      
070000                                                                          
070100                                                                          
070200* FIX LDC OSLO PILOT, ÄVEN YTTRE PARENTES OVAN                            
070300                                                                          
070400     OR                                                                   
070500       (BILL-IDDC    = '1C'                 AND                           
070600        BILL-IDDISTR = 0878                 AND                           
070700        BILL-IDKUNDNR = 0000884             AND                           
070800                                                                          
070900        (BILL-KDFAKTYP = 'R' OR 'G' OR 'K') AND                           
071000        BILL-KDFRAKT  NOT = 88              AND                           
071100        BILL-FLOVRLEV NOT = JA              AND                           
071200        BILL-FLORDSPE NOT = JA)                                           
071300* ENDFIX                                                                  
071400                                                                          
071500*                                                                         
071600*--    BARA DE RELEVANTA FAKT. I ETT STUDS-FLÖDE SKALL HAMNA              
071700*--    PÅ TULL DATABASEN. KONTROLL SKER I W4766800.                       
071800*                                                                         
071900       IF BILL-FLSTUDSUP = JA                                             
072000                                                                          
072100                                                                          
072200         PERFORM LB-SKRIV-FAKT-HUVUD-TULL                                 
072300         PERFORM LC-SKRIV-FAKT-RAD-TULL                                   
072400       END-IF                                                             
072500     END-IF                                                               
072600                                                                          
072700*--  FÖR LEV. FRÅN DUBAI, DC.87                                           
072800                                                                          
072900     MOVE BILL-IDDC TO WS-IDDC                                            
073000     IF NDC-AE                                                            
073100                                                                          
073200       IF DIST42-EJ-EU-MIC                                                
073300        AND (BILL-KDFAKTYP     = 'R' OR 'G' OR 'K')                       
073400        AND  BILL-FLOVRLEV NOT = JA                                       
073500        AND  BILL-KDFRAKT  NOT = 88                                       
073600        AND  BILL-FLORDSPE NOT = JA                                       
073700                                                                          
073800         PERFORM LB-SKRIV-FAKT-HUVUD-TULL                                 
073900         PERFORM LC-SKRIV-FAKT-RAD-TULL                                   
074000       END-IF                                                             
074100     END-IF                                                               
074200*                                                                         
074300*--  FÖR LEV. FRÅN DC 21                                                  
074400                                                                          
074500     IF SDC-NL                                                            
074600                                                                          
074700       IF DIST42-EJ-EU-DC21                                               
074800        AND (BILL-KDFAKTYP     = 'R' OR 'G' OR 'K')                       
074900        AND  BILL-KDFRAKT  NOT = 88                                       
075000        AND  BILL-FLOVRLEV NOT = JA                                       
075100        AND  BILL-FLORDSPE NOT = JA                                       
075200                                                                          
075300         PERFORM LB-SKRIV-FAKT-HUVUD-TULL                                 
075400         PERFORM LC-SKRIV-FAKT-RAD-TULL                                   
075500       END-IF                                                             
075600     END-IF                                                               
075700*                                                                         
075800*--  FÖR LEV. FRÅN DC 24, SPANIEN                                         
075900                                                                          
076000     IF SDC-ES                                                            
076100       IF DIST42-EJ-EU-MIC                                                
076200        AND (BILL-KDFAKTYP     = 'R' OR 'G' OR 'K')                       
076300        AND  BILL-KDFRAKT  NOT = 88                                       
076400        AND  BILL-FLOVRLEV NOT = JA                                       
076500        AND  BILL-FLORDSPE NOT = JA                                       
076600                                                                          
076700         PERFORM LB-SKRIV-FAKT-HUVUD-TULL                                 
076800         PERFORM LC-SKRIV-FAKT-RAD-TULL                                   
076900       END-IF                                                             
077000     END-IF                                                               
077100*                                                                         
077200*--  FÖR LEV. FRÅN DC I KINA                                              
077300                                                                          
077400     IF NDC-CN                                                            
077500       IF DIST42-MIC-CN                                                   
077600        AND (BILL-KDFAKTYP     = 'R' OR 'G' OR 'K')                       
077700        AND  BILL-KDFRAKT  NOT = 88                                       
077800        AND  BILL-FLOVRLEV NOT = JA                                       
077900        AND  BILL-FLORDSPE NOT = JA                                       
078000                                                                          
078100         PERFORM LB-SKRIV-FAKT-HUVUD-TULL                                 
078200         PERFORM LC-SKRIV-FAKT-RAD-TULL                                   
078300       END-IF                                                             
078400     END-IF                                                               
078500*                                                                         
078600*--  FÖR LEV. FRÅN DC I USA                                               
078700                                                                          
078800     IF NDC-US                                                            
078900       IF DIST42-MIC-USA                                                  
079000        AND (BILL-KDFAKTYP     = 'R' OR 'G' OR 'K')                       
079100        AND  BILL-KDFRAKT  NOT = 88                                       
079200        AND  BILL-FLOVRLEV NOT = JA                                       
079300        AND  BILL-FLORDSPE NOT = JA                                       
079400                                                                          
079500         PERFORM LB-SKRIV-FAKT-HUVUD-TULL                                 
079600         PERFORM LC-SKRIV-FAKT-RAD-TULL                                   
079700       END-IF                                                             
079800     END-IF                                                               
079900     .                                                                    
080000     EJECT                                                                
080100 LA-UPPDATERA-ARTIKELREG  SECTION.                                        
080200     MOVE 'LA-UPPDATERA-ARTIKELREG'  TO WS-SEKTION                        
080300*                                                                         
080400*    IF TILLAGG-JA                                                        
080500*     CONTINUE                                                            
080600*    ELSE                                                                 
080700      MOVE BILL-IDDC                 TO W-IDDC                            
080800                                        WS-IDDC-SEND                      
080900                                                                          
081000      IF BILL-IDDC NOT = DCS-IDDC                                         
081100         MOVE BILL-IDDC           TO W-IDDC-B6                            
081200         PERFORM IMS-GU-WDB601                                            
081300      END-IF                                                              
081400      MOVE BILL-IDARTNR              TO W-IDARTNR                         
081500                                        W-IDARTNR-CORE                    
081600      PERFORM IMS-GU-WDK601                                               
081700      PERFORM IMS-GNP-WDK611                                              
081800      IF ((DCS-CDC  OR DCS-DDC) AND (DIST07-USA-RETAILER))                
081900      OR ((DCS-CDC  OR DCS-DDC) AND (DIST07-CAN-RETAILER))                
082000         PERFORM LAE-BERAKNA-PRAVCOST                                     
082100         PERFORM LAF-SKAPA-A13-TRANSAKTION                                
082200      END-IF                                                              
082300      IF ((DCS-CDC  OR DCS-DDC) AND (DIST07-NON-VCC-OWNED))               
082400         PERFORM LAE-BERAKNA-PRAVCOST                                     
082500         PERFORM LAF-SKAPA-TRANSAKTION-SC                                 
082600      END-IF                                                              
082700      MOVE BILL-IDDC                 TO W-IDDC                            
082800                                        WS-IDDC-SEND                      
082900                                                                          
083000      IF BILL-IDDC NOT = DCS-IDDC                                         
083100         MOVE BILL-IDDC           TO W-IDDC-B6                            
083200         PERFORM IMS-GU-WDB601                                            
083300      END-IF                                                              
083400      PERFORM IMS-GU-WDK601                                               
083500                                                                          
083600      IF DCS-CDC                                                          
083700*                                                                         
083800*--     INGEN UPPDATERING AV KVERFS FÖR STUDSFLÖDET FRÅN SE               
083900        IF (DIST35-NONVCC-NONVCC-REFILL   AND DCS-SWEDEN)                 
084000                   OR                                                     
084100           (DIST35-NONVCC-NONVCC-TRANSFER AND DCS-SWEDEN)                 
084200          CONTINUE                                                        
084300*                                                                         
084400        ELSE                                                              
084500          IF BILL-KVLEVART > +0                                           
084600            PERFORM IMS-GHNP-WDK611                                       
084700            COMPUTE CLAG-KVEFRS = CLAG-KVEFRS - BILL-KVLEVART             
084800            PERFORM IMS-REPL-WDK611                                       
084900*****   SKALL LOGGA DATABAS WDL9 FÖR ALLA SALDOFÖRÄNDRINGAR. *****        
085000*****    OBS ! BOKAR SALDOT MED TOTALT ANTAL AV ART.NR/DC  *****          
085100            PERFORM LAA-SKAPA-SALDOLOGG                                   
085200          END-IF                                                          
085300        END-IF                                                            
085400                                                                          
085500      ELSE                                                                
085600*                                                                         
085700        IF DIST07-NON-VCC-OWNED                                           
085800          IF BILL-IDSHIPM NOT = SAVE-IDSHIPM                              
085900            MOVE BILL-IDSHIPM    TO W-IDSHIPM                             
086000            PERFORM IMS-GHU-WDE101                                        
086100                                                                          
086200            MOVE BILL-IDSHIPM  TO SAVE-IDSHIPM                            
086300          END-IF                                                          
086400*                                                                         
086500*--       FÖR VOR-STUDSFLÖDE FRÅN DC 11 SKALL INTE KVERFS UPPDAT.         
086600*--       DÅ ÄR IDDC-EXP = STUDS-DC:T.                                    
086700*--       VID VANLIGT FLÖDE ÄR IDDC-EXP = SPACE                           
086800*                                                                         
086900          IF SHIP-IDDC-EXP = SPACE                                        
087000            IF BILL-KVLEVART > +0                                         
087100              PERFORM IMS-GHU-WDK711                                      
087200              IF SEGMENT-SAKNAS                                           
087300                PERFORM S09A-LAGG-UPP-SLAGSEG                             
087400              ELSE                                                        
087500               COMPUTE SLAG-KVEFRS = SLAG-KVEFRS - BILL-KVLEVART          
087600                MOVE BILL-IDDISTR TO TEST-IDDISTR                         
087700                MOVE BILL-IDDC     TO WS-IDDC-SEND                        
087800                                                                          
087900                IF BILL-IDDC NOT = DCS-IDDC                               
088000                   MOVE BILL-IDDC      TO W-IDDC-B6                       
088100                   PERFORM IMS-GU-WDB601                                  
088200                END-IF                                                    
088300                                                                          
088400                PERFORM IMS-REPL-WDK711                                   
088500*****     LOGGAR DATABAS WDL9 FÖR ALLA SALDOFÖRÄNDRINGAR    *****         
088600*****     OBS ! BOKAR SALDOT MED TOTALT ANTAL AV ART.NR/DC *****          
088700                PERFORM LAB-SKAPA-SALDOLOGG                               
088800              END-IF                                                      
088900            END-IF                                                        
089000          END-IF                                                          
089100*                                                                         
089200        ELSE                                                              
089300          IF BILL-KVLEVART > +0                                           
089400            PERFORM IMS-GHU-WDK711                                        
089500            IF SEGMENT-SAKNAS                                             
089600              PERFORM S09A-LAGG-UPP-SLAGSEG                               
089700            ELSE                                                          
089800              COMPUTE SLAG-KVEFRS = SLAG-KVEFRS - BILL-KVLEVART           
089900              MOVE BILL-IDDISTR    TO TEST-IDDISTR                        
090000              MOVE BILL-IDDC       TO WS-IDDC-SEND                        
090100                                                                          
090200              IF BILL-IDDC NOT = DCS-IDDC                                 
090300                 MOVE BILL-IDDC        TO W-IDDC-B6                       
090400                 PERFORM IMS-GU-WDB601                                    
090500              END-IF                                                      
090600                                                                          
090700              PERFORM IMS-REPL-WDK711                                     
090800*****    SKALL LOGGA DATABAS WDL9 FÖR ALLA SALDOFÖRÄNDRINGAR *****        
090900*****    OBS ! BOKAR SALDOT MED TOTALT ANTAL AV ART.NR/DC  *****          
091000              PERFORM LAB-SKAPA-SALDOLOGG                                 
091100            END-IF                                                        
091200          END-IF                                                          
091300        END-IF                                                            
091400                                                                          
091500      END-IF                                                              
091600      PERFORM LD-ORDER-BYTERENOVOR                                        
091700*    END-IF                                                               
091800     .                                                                    
091900     EJECT                                                                
092000 LAA-SKAPA-SALDOLOGG SECTION.                                             
092100     MOVE 'LAA-SKAPA-SALDOLOGG'    TO WS-SEKTION                          
092200                                                                          
092300     MOVE W-IDARTNR                TO LOGG-IDARTNR                        
092400                                                                          
092500     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
092600     COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                             
092700                                   - WS-AAAAMMDD                          
092800     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
092900     COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                            
093000                                   - WS-TTMMSSTH                          
093100     MOVE 9                        TO LOGG-IDSEKVNR                       
093200     MOVE W-IDDC                   TO LOGG-IDDC                           
093300     MOVE 'OUTB'                   TO LOGG-IDHUVTYP                       
093400     MOVE 'INV'                    TO LOGG-IDSUBTYP                       
093500     MOVE 'W4766500'               TO LOGG-IDPGM                          
093600     MOVE SPACE                    TO LOGG-IDTRANS                        
093700     MOVE 'W4766500'               TO LOGG-IDUSER                         
093800     MOVE SPACE                    TO LOGG-REF                            
093900     MOVE BILL-IDFAKT              TO LOGG-IDFAKT                         
094000     MOVE BILL-IDDISTR             TO LOGG-IDDISTR                        
094100     MOVE BILL-IDKUNDNR            TO LOGG-IDKUNDNR                       
094200     MOVE BILL-IDKUNDRF(1:8)       TO LOGG-IDKUNDRF(3:8)                  
094300     MOVE '00'                     TO LOGG-IDKUNDRF(1:2)                  
094400                                                                          
094500     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS                 
094600     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS-PAV             
094700     MOVE '-'                      TO LOGG-IDTECKEN-KVEFRS                
094800     MOVE ' '                      TO LOGG-IDTECKEN-KVLS                  
094900     MOVE BILL-KVLEVART            TO LOGG-KVART-SALDO                    
095000                                                                          
095100     COMPUTE LOGG-KVAKS = CLAG-KVAKS-CDC                                  
095200                        + CLAG-KVAKS-T                                    
095300                                                                          
095400     MOVE CLAG-KVAKS-PAV           TO LOGG-KVAKS-PAV                      
095500     MOVE CLAG-KVEFRS              TO LOGG-KVEFRS                         
095600     MOVE CLAG-KVLS                TO LOGG-KVLS                           
095700     MOVE 000000                   TO LOGG-DAREGDAT-LADD                  
095800                                                                          
095900     PERFORM S28-IMS-ISRT-WDL901                                          
096000     .                                                                    
096100     EJECT                                                                
096200 LAB-SKAPA-SALDOLOGG SECTION.                                             
096300     MOVE 'LAB-SKAPA-SALDOLOGG'    TO WS-SEKTION                          
096400                                                                          
096500     MOVE W-IDARTNR                TO LOGG-IDARTNR                        
096600                                                                          
096700     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
096800     COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                             
096900                                   - WS-AAAAMMDD                          
097000     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
097100     COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                            
097200                                   - WS-TTMMSSTH                          
097300     MOVE 9                        TO LOGG-IDSEKVNR                       
097400     MOVE W-IDDC                   TO LOGG-IDDC                           
097500     MOVE 'OUTB'                   TO LOGG-IDHUVTYP                       
097600     MOVE 'INV'                    TO LOGG-IDSUBTYP                       
097700     MOVE 'W4766500'               TO LOGG-IDPGM                          
097800     MOVE SPACE                    TO LOGG-IDTRANS                        
097900     MOVE 'W4766500'               TO LOGG-IDUSER                         
098000     MOVE SPACE                    TO LOGG-REF                            
098100     MOVE BILL-IDFAKT              TO LOGG-IDFAKT                         
098200     MOVE BILL-IDDISTR             TO LOGG-IDDISTR                        
098300     MOVE BILL-IDKUNDNR            TO LOGG-IDKUNDNR                       
098400     MOVE BILL-IDKUNDRF(1:8)       TO LOGG-IDKUNDRF(3:8)                  
098500     MOVE '00'                     TO LOGG-IDKUNDRF(1:2)                  
098600                                                                          
098700     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS                 
098800     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS-PAV             
098900     MOVE '-'                      TO LOGG-IDTECKEN-KVEFRS                
099000     MOVE ' '                      TO LOGG-IDTECKEN-KVLS                  
099100     MOVE BILL-KVLEVART            TO LOGG-KVART-SALDO                    
099200     MOVE SLAG-KVAKS-SDC           TO LOGG-KVAKS                          
099300     MOVE SLAG-KVAKS-PAV           TO LOGG-KVAKS-PAV                      
099400     MOVE SLAG-KVEFRS              TO LOGG-KVEFRS                         
099500     MOVE SLAG-KVLS                TO LOGG-KVLS                           
099600     MOVE 000000                   TO LOGG-DAREGDAT-LADD                  
099700                                                                          
099800     PERFORM S28-IMS-ISRT-WDL901                                          
099900     .                                                                    
100000     EJECT                                                                
100100 LAE-BERAKNA-PRAVCOST SECTION.                                            
100200     MOVE 'LAE-BERAKNA-PRAVCOST'  TO WS-SEKTION                           
100300                                                                          
100400     IF (DCS-CDC OR DCS-DDC) AND (DIST07-USA-RETAILER)                    
100500       MOVE WC-NDC-US-RU          TO W-IDDC                               
100600                                     WS-IDDC-REC                          
100700     ELSE                                                                 
100800       IF (DCS-CDC OR DCS-DDC)  AND DIST07-CAN-RETAILER                   
100900         MOVE WC-NDC-CA           TO W-IDDC                               
101000                                     WS-IDDC-REC                          
101100       END-IF                                                             
101200     END-IF                                                               
101300     IF (DCS-CDC OR DCS-DDC) AND (DIST07-NON-VCC-OWNED)                   
101400       IF DIST07-KINA                                                     
101500         MOVE WC-NDC-CN-71        TO W-IDDC                               
101600                                     WS-IDDC-REC                          
101700       END-IF                                                             
101800       IF DIST07-INDIEN                                                   
101900         MOVE WC-NDC-IN           TO W-IDDC                               
102000                                     WS-IDDC-REC                          
102100       END-IF                                                             
102200       IF DIST07-KOREA                                                    
102300         MOVE WC-NDC-KR           TO W-IDDC                               
102400                                     WS-IDDC-REC                          
102500       END-IF                                                             
102600       IF DIST07-TURKEY                                                   
102700         MOVE WC-NDC-TR           TO W-IDDC                               
102800                                     WS-IDDC-REC                          
102900       END-IF                                                             
103000       IF DIST07-MALAYSIA                                                 
103100         MOVE WC-NDC-MY           TO W-IDDC                               
103200                                     WS-IDDC-REC                          
103300       END-IF                                                             
103400       IF DIST07-THAILAND                                                 
103500         MOVE WC-NDC-TH           TO W-IDDC                               
103600                                     WS-IDDC-REC                          
103700       END-IF                                                             
103800       IF DIST07-TAIWAN                                                   
103900         MOVE WC-NDC-TW           TO W-IDDC                               
104000                                     WS-IDDC-REC                          
104100       END-IF                                                             
104200       IF DIST07-MEXICO                                                   
104300         MOVE WC-NDC-MX           TO W-IDDC                               
104400                                     WS-IDDC-REC                          
104500       END-IF                                                             
104501       IF DIST07-BRAZIL                                                   
104502         MOVE WC-NDC-BR           TO W-IDDC                               
104503                                     WS-IDDC-REC                          
104504       END-IF                                                             
104510       IF DIST07-S-AFRICA                                                 
104520         MOVE WC-NDC-ZA           TO W-IDDC                               
104530                                     WS-IDDC-REC                          
104540       END-IF                                                             
104600     END-IF                                                               
104700     PERFORM S09-HAMTA-ARTS                                               
104800     PERFORM IMS-GHU-WDK711                                               
104900     IF SEGMENT-FINNS                                                     
105000        MOVE SLAG-PRAVCOST        TO WS-PRAVCOST-OLD                      
105100        PERFORM LAEA-CALL-W510AVG                                         
105200        PERFORM IMS-REPL-WDK711                                           
105300     END-IF                                                               
105400     .                                                                    
105500     EJECT                                                                
105600 LAEA-CALL-W510AVG SECTION.                                               
105700     MOVE 'LAEA-CALL-W510AVG'  TO WS-SEKTION                              
105800     MOVE BILL-DAFINDOC(3:2)   TO AVG-TIAA                                
105900     MOVE BILL-DAFINDOC(5:2)   TO AVG-TIMM                                
106000     MOVE W-IDDC               TO WS-IDDC                                 
106100     EVALUATE TRUE                                                        
106200       WHEN NDC-IN                                                        
106300         MOVE 062              TO AVG-KDCALL                              
106400       WHEN XDC-NON-VCC-OWNED                                             
106500         MOVE 061              TO AVG-KDCALL                              
106600       WHEN OTHER                                                         
106700         MOVE DAGENS-DATUM-AAR                                            
106800                               TO AVG-TIAA                                
106900         MOVE DAGENS-DATUM-MAANAD                                         
107000                               TO AVG-TIMM                                
107100         MOVE 060              TO AVG-KDCALL                              
107200     END-EVALUATE                                                         
107300     MOVE SLAG-PRAVCOST        TO AVG-PRAVCOST-OLD                        
107400     COMPUTE WS-KVLS = SLAG-KVLS + SLAG-KVEFRS                            
107500     IF WS-KVLS < ZERO                                                    
107600       MOVE ZERO               TO AVG-KVLS-OLD                            
107700     ELSE                                                                 
107800       MOVE WS-KVLS            TO AVG-KVLS-OLD                            
107900     END-IF                                                               
108000     MOVE BILL-PRARTNTO        TO AVG-PRARTNTO                            
108100     MOVE BILL-IDDISTR         TO TEST-IDDISTR                            
108200     IF DIST79-DEALER-PRICE OR                                            
108400        DIST79-ECOM-PRICE                                                 
108500       MOVE BILL-PRARTNTO-LOC  TO AVG-PRARTNTO                            
108600     END-IF                                                               
108700     MOVE ZERO                 TO AVG-PRKURS                              
108800     MOVE W-IDDC               TO AVG-IDDC                                
108900     MOVE 'SEK'                TO AVG-KDVALISO                            
109000     IF BILL-KDVALISO     = SPACE OR = 'SEK'                              
109100       CONTINUE                                                           
109200     ELSE                                                                 
109300       MOVE BILL-KDVALISO      TO AVG-KDVALISO                            
109400     END-IF                                                               
109500     MOVE BILL-KVLEVART        TO AVG-KVLEVART                            
109600     MOVE CLAG-KDPSLLOC        TO AVG-KDPSLLOC                            
109700     MOVE ART-IDFKNGRP         TO AVG-IDFKNGRP                            
109800     MOVE ART-KDPRODSL         TO AVG-KDPRODSL                            
109900                                                                          
110000     CALL W510AVG USING AVG-W510AVG 9305-PCB                              
110100                        AVG-WDB6-PCB                                      
110200     IF AVG-KDSVAR = ' '                                                  
110300        MOVE AVG-PRAVCOST-NEW  TO SLAG-PRAVCOST                           
110400     ELSE                                                                 
110500        DISPLAY '*** W4766500: FEL FRÅN ANROP FRÅN W510AVG  '             
110600        DISPLAY 'KDSVAR = ' AVG-KDSVAR                                    
110700        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
110800     END-IF                                                               
110900                                                                          
111000     .                                                                    
111100     EJECT                                                                
111200 LAF-SKAPA-A13-TRANSAKTION  SECTION.                                      
111300*        WDR8 TRANS TILL BATCH EKONOMI                                    
111400     MOVE 'LAF-SKAPA-A13-TRANSAKTION'  TO WS-SEKTION                      
111500                                                                          
111600     MOVE 'O20'               TO EKOTRA13-KDEKOHT                         
111700     IF WS-IDDC-REC NOT = DCS-IDDC                                        
111800        MOVE WS-IDDC-REC         TO W-IDDC-B6                             
111900        PERFORM IMS-GU-WDB601                                             
112000     END-IF                                                               
112100     IF DCS-IDDC = WC-NDC-US-RU                                           
112200       MOVE 53                TO EKOTRA13-IDFTG                           
112300     ELSE                                                                 
112400       IF DCS-NDC-NA AND DCS-CANADA                                       
112500         MOVE 54              TO EKOTRA13-IDFTG                           
112600       ELSE                                                               
112700         DISPLAY '*** W4766500: FEL MOTTAGANDE DC FÖR A13-TRANS'          
112800         DISPLAY 'IDDC   = ' W-IDDC                                       
112900         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
113000       END-IF                                                             
113100     END-IF                                                               
113200                                                                          
113300     MOVE 'A13'               TO EKOTRA13-IDPTYP                          
113400     IF WS-IDDC-SEND NOT = DCS-IDDC                                       
113500        MOVE WS-IDDC-SEND        TO W-IDDC-B6                             
113600        PERFORM IMS-GU-WDB601                                             
113700     END-IF                                                               
113800     IF DCS-DDC                                                           
113900        MOVE WC-CDC-SE        TO EKOTRA13-IDDC-SEND                       
114000     ELSE                                                                 
114100        IF DCS-KDDC NOT = SPACE                                           
114200           MOVE WS-IDDC-SEND  TO EKOTRA13-IDDC-SEND                       
114300        END-IF                                                            
114400     END-IF                                                               
114500     MOVE WS-IDDC-REC         TO EKOTRA13-IDDC-REC                        
114600     MOVE BILL-IDDISTR        TO EKOTRA13-IDDISTR                         
114700     MOVE BILL-IDKUNDNR       TO EKOTRA13-IDKUNDNR                        
114800     MOVE BILL-IDFAKT         TO EKOTRA13-IDFAKT                          
114900                                                                          
115000     MOVE BILL-DAFINDOC       TO EKOTRA13-DAFAKT                          
115100                                                                          
115200     MOVE ZERO                TO EKOTRA13-IDORDNR7                        
115300     MOVE BILL-IDORDNR7       TO EKOTRA13-IDORDNR7                        
115400     MOVE BILL-IDARTNR        TO EKOTRA13-IDARTNR                         
115500     MOVE ART-KDPRODSL        TO EKOTRA13-KDPRODSL                        
115600     MOVE CLAG-KDPSLLOC       TO EKOTRA13-KDPSLLOC                        
115700     MOVE BILL-KVLEVART       TO EKOTRA13-KVLEVART                        
115800     MOVE BILL-PRARTNTO       TO EKOTRA13-PRARTNTO                        
115900     MOVE SLAG-PRAVCOST       TO EKOTRA13-PRAVCOST                        
116000     MOVE WS-PRAVCOST-OLD     TO EKOTRA13-PRAVCOST-OLD                    
116100     MOVE SLAG-KVLS           TO EKOTRA13-KVLS-OLD                        
116200     MOVE AVG-PRKURS          TO EKOTRA13-PRKURS                          
116300     MOVE AVG-REMARKUP        TO EKOTRA13-REMARKUP                        
116400                                                                          
116500     MOVE 'W4766500'          TO R8-FIL-IDPGM                             
116600     MOVE DAGENS-DATUM        TO R8-FIL-TIREGDAT                          
116700     MOVE WS-TIKLOCK-R8       TO R8-FIL-TIKLOCK                           
116800     ADD +1                   TO R8-FIL-IDSEKVNR                          
116900     MOVE 'W510'              TO R8-FIL-CT-IDSYSTEM                       
117000     MOVE 'A13'               TO R8-FIL-CT-IDPTYP                         
117100     MOVE ' '                 TO R8-FIL-CT-IDVTYP                         
117200     MOVE EKOTRA13-W510A13    TO R8-FIL-WDR801-DATA                       
117300                                                                          
117400     PERFORM IMS-ISRT-WDR801                                              
117500                                                                          
117600     PERFORM UNTIL SEGMENT-FINNS                                          
117700        ADD +1                TO R8-FIL-IDSEKVNR                          
117800        PERFORM IMS-ISRT-WDR801                                           
117900        IF R8-FIL-IDSEKVNR = 999                                          
118000          MOVE ZERO           TO R8-FIL-IDSEKVNR                          
118100          ADD +1              TO WS-TIKLOCK-R8                            
118200          MOVE WS-TIKLOCK-R8  TO R8-FIL-TIKLOCK                           
118300        END-IF                                                            
118400     END-PERFORM                                                          
118500     .                                                                    
118600     EJECT                                                                
118700 LAF-SKAPA-TRANSAKTION-SC  SECTION.                                       
118800*        WDR8 TRANS TILL BATCH EKONOMI                                    
118900     MOVE 'LAF-SKAPA-TRANSAKTION-SC'  TO WS-SEKTION                       
119000                                                                          
119100     MOVE 'O20'               TO EKOTRA13-KDEKOHT                         
119200     IF WS-IDDC-REC NOT = DCS-IDDC                                        
119300        MOVE WS-IDDC-REC         TO W-IDDC-B6                             
119400        PERFORM IMS-GU-WDB601                                             
119500     END-IF                                                               
119600     MOVE DCS-IDDC            TO WS-IDDC                                  
119700     EVALUATE TRUE                                                        
119800       WHEN NDC-CN                                                        
119900         MOVE 60              TO EKOTRA13-IDFTG                           
120000       WHEN NDC-IN                                                        
120100         MOVE 61              TO EKOTRA13-IDFTG                           
120200       WHEN NDC-TH                                                        
120300         MOVE 63              TO EKOTRA13-IDFTG                           
120400       WHEN NDC-TW                                                        
120500         MOVE 64              TO EKOTRA13-IDFTG                           
120600       WHEN NDC-KR                                                        
120700         MOVE 65              TO EKOTRA13-IDFTG                           
120800       WHEN NDC-TR                                                        
120900         MOVE 86              TO EKOTRA13-IDFTG                           
121000       WHEN NDC-MY                                                        
121100         MOVE 66              TO EKOTRA13-IDFTG                           
121200       WHEN NDC-MX                                                        
121300         MOVE 83              TO EKOTRA13-IDFTG                           
121400       WHEN NDC-BR                                                        
121500         MOVE 82              TO EKOTRA13-IDFTG                           
121510       WHEN NDC-ZA                                                        
121520         MOVE 85              TO EKOTRA13-IDFTG                           
121600       WHEN OTHER                                                         
121700         DISPLAY '*** W4766500: FEL MOTTAGANDE DC FÖR A13-TRANS'          
121800         DISPLAY 'IDDC   = ' W-IDDC                                       
121900         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
122000     END-EVALUATE                                                         
122100                                                                          
122200     MOVE 'A13'               TO EKOTRA13-IDPTYP                          
122300     IF WS-IDDC-SEND NOT = DCS-IDDC                                       
122400        MOVE WS-IDDC-SEND        TO W-IDDC-B6                             
122500        PERFORM IMS-GU-WDB601                                             
122600     END-IF                                                               
122700     IF DCS-DDC                                                           
122800        MOVE WC-CDC-SE        TO EKOTRA13-IDDC-SEND                       
122900     ELSE                                                                 
123000        IF DCS-KDDC NOT = SPACE                                           
123100           MOVE WS-IDDC-SEND  TO EKOTRA13-IDDC-SEND                       
123200        END-IF                                                            
123300     END-IF                                                               
123400     MOVE WS-IDDC-REC         TO EKOTRA13-IDDC-REC                        
123500     MOVE BILL-IDDISTR        TO EKOTRA13-IDDISTR                         
123600     MOVE BILL-IDKUNDNR       TO EKOTRA13-IDKUNDNR                        
123700     MOVE BILL-IDFAKT         TO EKOTRA13-IDFAKT                          
123800                                                                          
123900     MOVE BILL-DAFINDOC       TO EKOTRA13-DAFAKT                          
124000                                                                          
124100     MOVE ZERO                TO EKOTRA13-IDORDNR7                        
124200     MOVE BILL-IDORDNR7       TO EKOTRA13-IDORDNR7                        
124300     MOVE BILL-IDARTNR        TO EKOTRA13-IDARTNR                         
124400     MOVE ART-KDPRODSL        TO EKOTRA13-KDPRODSL                        
124500     MOVE CLAG-KDPSLLOC       TO EKOTRA13-KDPSLLOC                        
124600     MOVE BILL-KVLEVART       TO EKOTRA13-KVLEVART                        
124700     MOVE BILL-PRARTNTO       TO EKOTRA13-PRARTNTO                        
124800     MOVE SLAG-PRAVCOST       TO EKOTRA13-PRAVCOST                        
124900     MOVE WS-PRAVCOST-OLD     TO EKOTRA13-PRAVCOST-OLD                    
125000     MOVE SLAG-KVLS           TO EKOTRA13-KVLS-OLD                        
125100     MOVE AVG-PRKURS          TO EKOTRA13-PRKURS                          
125200     MOVE AVG-REMARKUP        TO EKOTRA13-REMARKUP                        
125300                                                                          
125400     MOVE 'W4766500'          TO R8-FIL-IDPGM                             
125500     MOVE DAGENS-DATUM        TO R8-FIL-TIREGDAT                          
125600     MOVE WS-TIKLOCK-R8       TO R8-FIL-TIKLOCK                           
125700     ADD +1                   TO R8-FIL-IDSEKVNR                          
125800     MOVE 'W510'              TO R8-FIL-CT-IDSYSTEM                       
125900     MOVE 'A13'               TO R8-FIL-CT-IDPTYP                         
126000     MOVE ' '                 TO R8-FIL-CT-IDVTYP                         
126100     MOVE EKOTRA13-W510A13    TO R8-FIL-WDR801-DATA                       
126200                                                                          
126300     PERFORM IMS-ISRT-WDR801                                              
126400                                                                          
126500     PERFORM UNTIL SEGMENT-FINNS                                          
126600        ADD +1                TO R8-FIL-IDSEKVNR                          
126700        PERFORM IMS-ISRT-WDR801                                           
126800        IF R8-FIL-IDSEKVNR = 999                                          
126900          MOVE ZERO           TO R8-FIL-IDSEKVNR                          
127000          ADD +1              TO WS-TIKLOCK-R8                            
127100          MOVE WS-TIKLOCK-R8  TO R8-FIL-TIKLOCK                           
127200        END-IF                                                            
127300     END-PERFORM                                                          
127400     .                                                                    
127500     EJECT                                                                
127600 LAK-NDC-A13  SECTION.                                                    
127700     MOVE 'LAK-NDC-A13'  TO WS-SEKTION                                    
127800*                                                                         
127900     IF TILLAGG-JA                                                        
128000      CONTINUE                                                            
128100     ELSE                                                                 
128200      MOVE BILL-IDDC                 TO W-IDDC                            
128300                                        WS-IDDC-SEND                      
128400                                                                          
128500      IF BILL-IDDC NOT = DCS-IDDC                                         
128600         MOVE BILL-IDDC           TO W-IDDC-B6                            
128700         PERFORM IMS-GU-WDB601                                            
128800      END-IF                                                              
128900      MOVE BILL-IDARTNR              TO W-IDARTNR                         
129000                                        W-IDARTNR-CORE                    
129100      PERFORM IMS-GU-WDK601                                               
129200      PERFORM IMS-GNP-WDK611                                              
129300      IF ((DCS-CDC  OR DCS-DDC) AND (DIST07-USA-RETAILER))                
129400      OR ((DCS-CDC  OR DCS-DDC) AND (DIST07-CAN-RETAILER))                
129500         PERFORM LAE-BERAKNA-PRAVCOST                                     
129600         PERFORM LAF-SKAPA-A13-TRANSAKTION                                
129700      END-IF                                                              
129800      IF ((DCS-CDC  OR DCS-DDC) AND (DIST07-NON-VCC-OWNED))               
129900         PERFORM LAE-BERAKNA-PRAVCOST                                     
130000         PERFORM LAF-SKAPA-TRANSAKTION-SC                                 
130100      END-IF                                                              
130200     END-IF                                                               
130300     .                                                                    
130400     EJECT                                                                
130500 LB-SKRIV-FAKT-HUVUD-TULL  SECTION.                                       
130600     MOVE 'LB-SKRIV-FAKT-HUVUD'   TO WS-SEKTION                           
130700**   SKRIVS  EJ FÖR EU                                                    
130800                                                                          
130900     MOVE BILL-IDFAKT             TO W-IDFAKT-M7                          
131000     MOVE BILL-IDORDNR7           TO W-IDORDNR-M7                         
131100     MOVE BILL-IDKOLLI            TO W-IDKOLLI-M7                         
131200     MOVE BILL-IDPRODNR           TO W-IDPRODNR-M7                        
131300                                                                          
131400     PERFORM IMS-GHU-WDM701                                               
131500     IF SEGMENT-SAKNAS                                                    
131600                                                                          
131700      INITIALIZE HUV-WDM701-CTX                                           
131800      MOVE BILL-IDFAKT            TO HUV-IDFAKT                           
131900      MOVE BILL-IDORDNR7          TO HUV-IDORDNR7                         
132000      MOVE BILL-IDPRODNR          TO HUV-IDPRODNR                         
132100      MOVE BILL-KDORDKL           TO HUV-KDORDKL                          
132200      MOVE BILL-IDPARTNR          TO HUV-IDPARTNR                         
132300                                     W-IDPARTNR                           
132400                                                                          
132500*     FÖRETAG ÄR ALLTID 57 FÖR FAKT. FRÅN SVERIGE                         
132600      MOVE WC-IDFTG-PV            TO W-IDFTG                              
132700                                                                          
132800      PERFORM IMS-GU-WDB101                                               
132900      IF SEGMENT-FINNS                                                    
133000        MOVE BET-ADBETRAD-1       TO HUV-ADKOPARE-RAD1                    
133100        MOVE BET-ADBETRAD-2       TO HUV-ADKOPARE-RAD2                    
133200        MOVE BET-BEBETRAD-1       TO HUV-BEKOPARE-RAD1                    
133300        MOVE BET-BEBETRAD-2       TO HUV-BEKOPARE-RAD2                    
133400      END-IF                                                              
133500*                                                                         
133600*--   FÖR IMPORTÖRER SOM FÅR LEV. FRÅN DUBAI (T.EX) MÅSTE                 
133700*     VI ANVÄNDA PARMAID OCH IDFTG SOM FINNS PÅ RESP. DISTR.              
133800*     I FILEN TILL MIC (TULLDATABASEN INITIERAS I DETTA PGM.)             
133900*                                                                         
134000      IF NDC-AE                                                           
134100        MOVE BILL-IDDISTR         TO W-IDDISTR-B2                         
134200        MOVE BILL-IDKUNDNR        TO W-IDKUNDNR-B2                        
134300        PERFORM IMS-GU-WDB201                                             
134400        IF SEGMENT-FINNS                                                  
134500          MOVE GMT-IDPARTNR       TO W-IDPARTNR                           
134600                                     HUV-IDPARTNR                         
134700          MOVE GMT-IDFTG          TO W-IDFTG                              
134800          PERFORM IMS-GU-WDB101                                           
134900          IF SEGMENT-FINNS                                                
135000                                                                          
135100            MOVE BET-ADBETRAD-1   TO HUV-ADKOPARE-RAD1                    
135200            MOVE BET-ADBETRAD-2   TO HUV-ADKOPARE-RAD2                    
135300            MOVE BET-BEBETRAD-1   TO HUV-BEKOPARE-RAD1                    
135400            MOVE BET-BEBETRAD-2   TO HUV-BEKOPARE-RAD2                    
135500          END-IF                                                          
135600        END-IF                                                            
135700      END-IF                                                              
135800      MOVE BILL-IDDISTR           TO HUV-IDDISTR                          
135900      MOVE BILL-IDKUNDNR          TO HUV-IDKUNDNR                         
136000      MOVE BILL-IDSHIPM           TO HUV-IDSKEPPN                         
136100      MOVE BILL-IDTULL            TO HUV-IDTULL                           
136200      IF HUV-IDTULFTG = SPACE                                             
136300         MOVE '00'                TO HUV-IDTULFTG                         
136400      END-IF                                                              
136500      MOVE SPACE                  TO HUV-IDUSER                           
136600      MOVE BILL-KDFAKTYP          TO HUV-KDFAKTYP                         
136700                                                                          
136800      PERFORM LBA-TA-FRAM-WDE1-INFO                                       
136900                                                                          
137000      MOVE BILL-KDFRAKT           TO HUV-KDFRAKT                          
137100      MOVE BILL-DAFINDOC          TO HUV-TIFAKT                           
137200      MOVE BILL-FLORDSPE          TO HUV-FLORDSPE                         
137300      MOVE BILL-IDKOLLI           TO HUV-IDKOLLI                          
137400      MOVE BILL-IDDC              TO HUV-IDDC                             
137500      MOVE SPACE                  TO HUV-FLCONTAIN                        
137600                                                                          
137700***************************************************************           
137800*** NU FÖLJER EN MASSA TRIX FÖR ATT KLARA AV ATT INLEVERANS               
137900*** PÅ SINA CLEARING-FAKTUROR KAN HA SAMMA KOLLINR FLERA                  
138000*** GÅNGER. DET LIGGER DESSUTOM LOGIK I DESSA KOLLINR ÖVER                
138100*** 900. 902 BETYDER ATT DET FINNS 2 KOLLIN, MEN MAN HAR                  
138200*** BARA RAPPORTERAT ETT. OSV... ******************************           
138300***************************************************************           
138400                                                                          
138500      IF BILL-IDDC NOT = DCS-IDDC                                         
138600         MOVE BILL-IDDC           TO W-IDDC-B6                            
138700         PERFORM IMS-GU-WDB601                                            
138800      END-IF                                                              
138900      IF   BILL-KDFAKTYP = 'K' AND DCS-CDC-TR                             
139000       IF HUV-IDKOLLI > 900                                               
139100         IF HUV-IDKOLLI NOT = SPAR-IDKOLLI                                
139200           MOVE 1000      TO W-RAKNARE                                    
139300         ELSE                                                             
139400           ADD 1000       TO W-RAKNARE                                    
139500         END-IF                                                           
139600         MOVE HUV-IDKOLLI TO SPAR-IDKOLLI                                 
139700         ADD W-RAKNARE    TO HUV-IDKOLLI                                  
139800         PERFORM IMS-ISRT-WDM701                                          
139900       ELSE                                                               
140000         PERFORM IMS-ISRT-WDM701                                          
140100       END-IF                                                             
140200      ELSE                                                                
140300       PERFORM IMS-ISRT-WDM701                                            
140400      END-IF                                                              
140500     END-IF                                                               
140600     .                                                                    
140700     EJECT                                                                
140800 LBA-TA-FRAM-WDE1-INFO SECTION.                                           
140900                                                                          
141000     MOVE BILL-IDSHIPM           TO W-IDSHIPM                             
141100     PERFORM IMS-GHU-WDE101                                               
141200                                                                          
141300     IF SEGMENT-FINNS                                                     
141900       MOVE SHIP-IDLBBET         TO HUV-IDLBBET                           
142000       MOVE SHIP-BELEVVIL        TO HUV-BELEVVIL                          
142100                                                                          
142300       MOVE BILL-IDDISTR         TO W-IDDISTR                             
142400       MOVE BILL-IDKUNDNR        TO W-IDKUNDNR                            
142500                                                                          
142600       PERFORM IMS-GU-WDE111                                              
142700       MOVE SGMT-PRKURS          TO HUV-PRKURS                            
142800                                                                          
142900***    The additional costs can be different depending on                 
143000*      the customer (WDE111), that's why we need to read                  
143100*      every WDE122 under each WDE111.                                    
143200*                                                                         
143501       PERFORM IMS-GNP-WDE122                                             
143600                                                                          
143700       IF SEGMENT-FINNS                                                   
143800         MOVE TILL-IDBOKN        TO HUV-IDBOKN                            
143900         MOVE TILL-PRFRAKT       TO HUV-PRFRAKT                           
144000         MOVE TILL-PRFOERS       TO HUV-PRFOERS                           
144100         MOVE TILL-PRLEGKST      TO HUV-PRLEGKST                          
144200         MOVE TILL-PREMBHNT      TO HUV-PREMBHNT                          
144300         MOVE TILL-PRAVDRAG      TO HUV-PRAVDRAG                          
144310         MOVE TILL-IDSIGILL      TO HUV-IDSIGILL                          
144400       END-IF                                                             
144500                                                                          
144600***    read E121 for some more case information                           
144700*                                                                         
144801       MOVE BILL-IDPRODNR        TO W-IDPRODNR                            
144901       MOVE BILL-IDKOLLI         TO W-IDKOLLI                             
145000       PERFORM IMS-GU-WDE121                                              
145100                                                                          
145200       IF SEGMENT-FINNS                                                   
145300         MOVE SKOLLI-VKORDBTO-KOLLI                                       
145400                                 TO HUV-VKORDBTO-KOLLI                    
145500         MOVE SKOLLI-KDEMBTYP    TO HUV-KDEMBTYP                          
145600       END-IF                                                             
145700                                                                          
145800     END-IF                                                               
145900     .                                                                    
146000     EJECT                                                                
146100 LC-SKRIV-FAKT-RAD-TULL  SECTION.                                         
146200     MOVE 'LC-SKRIV-FAKT-RAD'      TO WS-SEKTION                          
146300                                                                          
146400                                                                          
146500     MOVE BILL-IDFAKT            TO W-IDFAKT-M8                           
146600     MOVE BILL-IDKOLLI           TO W-IDKOLLI-M8                          
146700     MOVE BILL-IDPRODNR          TO W-IDPRODNR-M8                         
146800     MOVE BILL-IDARTNR           TO W-IDARTNR-M8                          
146900     MOVE BILL-IDPURAD           TO W-IDRADNR-M8                          
147000                                                                          
147100     IF BILL-FLLSBOK NOT = JA                                             
147200        MOVE BILL-IDARTNR        TO W-IDARTNR                             
147300        PERFORM IMS-GU-WDK601                                             
147400        PERFORM IMS-GNP-WDK611                                            
147500     END-IF                                                               
147600                                                                          
147700      INITIALIZE RAD-WDM801                                               
147800                                                                          
147900      MOVE BILL-IDFAKT            TO RAD-IDFAKT                           
148000      MOVE BILL-IDPRODNR          TO RAD-IDPRODNR                         
148100      MOVE BILL-IDKOLLI           TO RAD-IDKOLLI                          
148200      MOVE BILL-IDARTNR           TO RAD-IDARTNR                          
148300      MOVE BILL-IDPURAD           TO RAD-IDRADNR                          
148400      MOVE CLAG-IDSTATNR(3)       TO RAD-IDSTATNR                         
148500      IF RAD-IDSTATNR = ZERO                                              
148600        MOVE GEN-IDSTATNR         TO RAD-IDSTATNR                         
148700      END-IF                                                              
148800      MOVE BILL-KDARTURS          TO RAD-KDARTURS                         
148900                                                                          
149000      MOVE BILL-KVLEVART          TO RAD-KVLEVART                         
149100      MOVE 'SEK'                  TO RAD-KDVALISO                         
149200      IF BILL-KDVALISO     = SPACE OR = 'SEK'                             
149300        CONTINUE                                                          
149400      ELSE                                                                
149500        MOVE BILL-KDVALISO        TO RAD-KDVALISO                         
149600      END-IF                                                              
149700      MOVE BILL-PRARTNTO          TO RAD-PRARTNTO                         
149800      MOVE BILL-IDDISTR           TO TEST-IDDISTR                         
149900                                                                          
150000      IF DIST79-DEALER-PRICE OR                                           
150100         DIST79-ECOM-PRICE                                                
150200        MOVE BILL-PRARTNTO-LOC    TO RAD-PRARTNTO                         
150300      END-IF                                                              
150400                                                                          
150500      MOVE BILL-VKART-NTO-KG      TO RAD-VKART-NTO-KG                     
150600      MOVE BILL-PRAVCOST-BILLIT   TO RAD-PRAVCOST                         
150700      MOVE BILL-KDVALISO-AVC      TO RAD-KDVALISO-AVC                     
150800                                                                          
150810      MOVE SPACES                 TO RAD-IDREFDDS                         
150820                                     RAD-KDORSAK                          
150900*************************************************************             
151000*** NU FÖLJER EN MASSA TRIX FÖR ATT KLARA AV ATT INLEVERANS               
151100*** PÅ SINA CLEARING-FAKTUROR KAN HA SAMMA KOLLINR FLERA                  
151200*** GÅNGER. DET LIGGER DESSUTOM LOGIK I DESSA KOLLINR ÖVER                
151300*** 900. 902 BETYDER ATT DET FINNS 2 KOLLIN, MEN MAN HAR                  
151400*** BARA RAPPORTERAT ETT. OSV ... ***************************             
151500*******************************************************                   
151600                                                                          
151700      IF BILL-IDDC NOT = DCS-IDDC                                         
151800         MOVE BILL-IDDC           TO W-IDDC-B6                            
151900         PERFORM IMS-GU-WDB601                                            
152000      END-IF                                                              
152100      IF   BILL-KDFAKTYP = 'K' AND DCS-CDC-TR                             
152200        IF RAD-IDKOLLI > 900                                              
152300          IF RAD-IDARTNR NOT = SPAR-IDARTNR                               
152400            MOVE 1000             TO  W-RAKNARE                           
152500          ELSE                                                            
152600            ADD  1000             TO W-RAKNARE                            
152700          END-IF                                                          
152800          MOVE RAD-IDARTNR        TO SPAR-IDARTNR                         
152900          ADD W-RAKNARE           TO RAD-IDKOLLI                          
153000          PERFORM IMS-ISRT-WDM801                                         
153100        ELSE                                                              
153200          PERFORM IMS-ISRT-WDM801                                         
153300        END-IF                                                            
153400      ELSE                                                                
153500        PERFORM IMS-ISRT-WDM801                                           
153600      END-IF                                                              
153700     .                                                                    
153800     EJECT                                                                
153900 LD-ORDER-BYTERENOVOR  SECTION.                                           
154000                                                                          
154100     MOVE BILL-IDARTNR                    TO TEST-IDARTNR                 
154200                                             WS-IDARTNR                   
154300     IF BYT03-OBJEKT                                                      
154400       MOVE WS-IDARTNR                    TO W-IDARTNR-WDA9               
154500       PERFORM IMS-GHU-WDA901                                             
154600       IF SEGMENT-SAKNAS                                                  
154700         MOVE WS-IDARTNR                  TO UPB-IDARTNR                  
154800         MOVE ART-IDFKNGRP                TO UPB-IDFKNGRP                 
154900         PERFORM IMS-ISRT-WDA901                                          
155000       END-IF                                                             
155100                                                                          
155200       MOVE 'AAMMDD'                      TO DAT-KDDATFORM                
155300       MOVE FUNCTION CURRENT-DATE (3:6)   TO DAT-I-TIDATUM                
155400                                                                          
155500       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
155600                           DAT-O-TIDATUM DAT-KDSVAR                       
155700                                                                          
155800       IF DAT-KDSVAR-OK                                                   
155900         MOVE 200000                      TO WS-DAAAPP                    
156000         ADD DAT-TIAAPP                   TO WS-DAAAPP                    
156100         MOVE WS-DAAAPP                   TO W-DAAAPP-WDA9                
156200       ELSE                                                               
156300         DISPLAY 'FELAKTIGT DATUM'                                        
156400         CALL FELLOG                                                      
156500       END-IF                                                             
156600       MOVE BILL-IDDISTR                  TO TEST-IDDISTR                 
156700       PERFORM IMS-GHU-WDA912                                             
156800       IF SEGMENT-SAKNAS                                                  
156900         PERFORM IMS-GHU-WDA901                                           
157000         MOVE WS-DAAAPP                   TO UPA-DAAAPP                   
157100         MOVE ZERO                        TO UPA-SUINVEST-DC              
157200                                             UPA-SULEVANT-DC              
157300                                             UPA-SUMOTT-CP                
157400                                             UPA-SUSKROT-DC               
157500         IF DIST18-SKROT OR DIST18-SCRAP-NDC                              
157600           ADD BILL-KVLEVART              TO UPA-SUSKROT-DC               
157700         ELSE                                                             
157800           IF NOT DIS134-BYTESRENOV AND NOT DIS134-BYTESREN-NA            
157900             ADD BILL-KVLEVART            TO UPA-SULEVANT-DC              
158000           END-IF                                                         
158100         END-IF                                                           
158200         PERFORM IMS-ISRT-WDA912                                          
158300       ELSE                                                               
158400         IF DIST18-SKROT OR DIST18-SCRAP-NDC                              
158500           ADD BILL-KVLEVART              TO UPA-SUSKROT-DC               
158600         ELSE                                                             
158700           IF NOT DIS134-BYTESRENOV AND NOT DIS134-BYTESREN-NA            
158800             ADD BILL-KVLEVART            TO UPA-SULEVANT-DC              
158900           END-IF                                                         
159000         END-IF                                                           
159100         PERFORM IMS-REPL-WDA912                                          
159200       END-IF                                                             
159300                                                                          
159400       IF DIS134-BYTESRENOV OR DIS134-BYTESREN-NA                         
159500         MOVE BILL-IDDISTR                TO W-3161-IDDISTR               
159600         PERFORM IMS-GHU-WDGX3161                                         
159700         IF SEGMENT-SAKNAS                                                
159800           MOVE '3161'                    TO 3161-IDHTYP                  
159900           MOVE BILL-IDDISTR              TO 3161-IDDISTR                 
160000           MOVE LOW-VALUE                 TO 3161-LOWVALUE                
160100           PERFORM IMS-ISRT-WDGX3161                                      
160200         END-IF                                                           
160300         MOVE BILL-IDKUNDRF               TO WS-IDKUNDRF                  
160400         MOVE WS-IDKUNDRF-1-5             TO 3162-IDORDER                 
160500         MOVE FUNCTION CURRENT-DATE (1:8) TO 3162-DAORDREG                
160600         MOVE WS-IDARTNR                  TO 3162-IDARTNR                 
160700         IF WS-IDKUNDRF-1-5 = WS-SPAR-IDKUNDRF-1-5                        
160800           ADD  1                         TO WS-IDRADNR                   
160900           MOVE WS-IDRADNR                TO 3162-IDRADNR                 
161000         ELSE                                                             
161100           MOVE 1                         TO WS-IDRADNR                   
161200           MOVE WS-IDRADNR                TO 3162-IDRADNR                 
161300           MOVE WS-IDKUNDRF-1-5           TO WS-SPAR-IDKUNDRF-1-5         
161400         END-IF                                                           
161500         MOVE BILL-KVLEVART               TO 3162-KVAVIS                  
161600         MOVE ZERO                        TO 3162-KVAVBART                
161700                                             3162-DAREGDAT                
161800                                             3162-TIREGTID                
161900         MOVE SPACE                       TO 3162-IDUSER                  
162000         PERFORM IMS-ISRT-WDGX3162                                        
162100       END-IF                                                             
162200                                                                          
162300     END-IF                                                               
162400     .                                                                    
162500     EJECT                                                                
162600 M-UPPDATERA-FAKT-HIST section.                                           
162700     MOVE 'M-UPPDATERA-FAKT-HIST' TO WS-SEKTION                           
162800                                                                          
162900     MOVE BILL-IDFAKT             TO W-IDFAKT                             
163000     MOVE BILL-IDPRODNR           TO W-IDPRODNR-L511                      
163100     MOVE BILL-IDKOLLI            TO W-IDKOLLI-L511                       
163200     MOVE BILL-IDARTNR            TO W-IDARTNR-L521                       
163300     MOVE BILL-IDPURAD            TO W-IDPURAD-L521                       
163400     PERFORM IMS-GHU-WDL501                                               
163500     IF SEGMENT-FINNS                                                     
163600       IF BILL-IDARTNR = 0                                                
163700*        additional cost line                                             
163800         ADD BILL-SUBTO-LINE      TO FAK-SUFKTTILL                        
163900         PERFORM IMS-REPL-WDL501                                          
164000       ELSE                                                               
164100         IF FAK-KDFAKTYP = SPACE                                          
164200           MOVE BILL-FLDIRLEV     TO FAK-FLDIRLEV                         
164300           MOVE BILL-KDFAKTYP     TO FAK-KDFAKTYP                         
164400           MOVE BILL-KDVAT        TO FAK-KDVAT                            
164500           MOVE BILL-IDANALYS     TO FAK-IDANALYS                         
164600           MOVE BILL-IDKONTO      TO FAK-IDKONTO                          
164700           MOVE BILL-IDKST        TO FAK-IDKST                            
164800           MOVE BILL-FLSTUDSUP    TO FAK-FLSTUDSUP                        
164900*          det kanske blir fler datael....                                
165000           PERFORM IMS-REPL-WDL501                                        
165100         END-IF                                                           
165200         PERFORM IMS-GHNP-WDL511                                          
165300         IF SEGMENT-FINNS                                                 
165400           PERFORM MC-FLYTTA-FAKT-RAD                                     
165500           PERFORM IMS-ISRT-WDL521                                        
165600         ELSE                                                             
165700           PERFORM MB-FLYTTA-FAKT-KLI                                     
165800           PERFORM IMS-ISRT-WDL511                                        
165900           PERFORM MC-FLYTTA-FAKT-RAD                                     
166000           PERFORM IMS-ISRT-WDL521                                        
166100         END-IF                                                           
166200       END-IF                                                             
166300     ELSE                                                                 
166400*                                                                         
166500       IF BILL-IDARTNR = 0                                                
166600*        additional cost line                                             
166700         MOVE BILL-SUBTO-LINE     TO FAK-SUFKTTILL                        
166800*                                                                         
166900         PERFORM MA-FLYTTA-FAKT-HUV                                       
167000         PERFORM IMS-ISRT-WDL501                                          
167100       ELSE                                                               
167200         MOVE ZERO                TO FAK-SUFKTTILL                        
167300         PERFORM MA-FLYTTA-FAKT-HUV                                       
167400         PERFORM IMS-ISRT-WDL501                                          
167500*                                                                         
167600         PERFORM MB-FLYTTA-FAKT-KLI                                       
168000         PERFORM IMS-ISRT-WDL511                                          
169000*                                                                         
170000         PERFORM MC-FLYTTA-FAKT-RAD                                       
180000         PERFORM IMS-ISRT-WDL521                                          
181000       END-IF                                                             
182000     END-IF                                                               
183000     .                                                                    
184000     EJECT                                                                
185000 MA-FLYTTA-FAKT-HUV SECTION.                                              
186000     MOVE 'MA-FLYTTA-FAKT-HUV   ' TO WS-SEKTION                           
187000                                                                          
188000     MOVE BILL-IDFAKT             TO FAK-IDFAKT                           
189000     MOVE BILL-IDDC-BILLIT        TO FAK-IDDC                             
190000     MOVE BILL-IDDC               TO FAK-IDDC-LEV                         
191000     MOVE BILL-FLDIRLEV           TO FAK-FLDIRLEV                         
192000     MOVE BILL-KDFAKTYP           TO FAK-KDFAKTYP                         
193000     MOVE BILL-KDVALISO           TO FAK-KDVALISO                         
194000     MOVE BILL-KDVAT              TO FAK-KDVAT                            
194100     MOVE BILL-IDANALYS           TO FAK-IDANALYS                         
194200     MOVE BILL-IDKONTO            TO FAK-IDKONTO                          
194300     MOVE BILL-IDKST              TO FAK-IDKST                            
194400     MOVE BILL-IDPARTNR           TO FAK-IDPARTNR                         
194500     MOVE BILL-IDDISTR            TO FAK-IDDISTR                          
194600     MOVE BILL-FLSTUDSUP          TO FAK-FLSTUDSUP                        
194700     MOVE BILL-SUBTO-TOT          TO FAK-SUBTO-TOT                        
194800     MOVE BILL-SUNTO-TOT          TO FAK-SUNTO-TOT                        
194900     MOVE BILL-SUNTO-PART-LOC     TO FAK-SUNTO-PART-LOC                   
195000     MOVE BILL-SUBTO-TOT-PART-LOC TO FAK-SUBTO-TOT-PART-LOC               
195100     MOVE BILL-SUNTO-TOT-LOC      TO FAK-SUNTO-TOT-LOC                    
195200     MOVE BILL-SUVAT-BILLIT-TOT-LOC                                       
195300                                  TO FAK-SUVAT-BILLIT-TOT-LOC             
195400     MOVE BILL-SUBTO-TOT-LOC      TO FAK-SUBTO-TOT-LOC                    
195500     MOVE BILL-KDVALISO-LOC       TO FAK-KDVALISO-LOC                     
195600     MOVE BILL-PRKURS-LOC         TO FAK-PRKURS-LOC                       
195700     MOVE BILL-KDTECKEN-LOC       TO FAK-KDTECKEN                         
195800     MOVE BILL-SUNTO-LOCC         TO FAK-SUNTO-LOCC                       
195900     MOVE BILL-SUNTO-PART-RECALC                                          
196000                                  TO FAK-SUNTO-PART-RECALC                
196100     MOVE BILL-SUBTO-TOT-PART-RECALC                                      
196200                                  TO FAK-SUBTO-TOT-PART-RECALC            
196300     MOVE BILL-SUNTO-TOT-RECALC   TO FAK-SUNTO-TOT-RECALC                 
196400     MOVE BILL-SUBTO-TOT-RECALC   TO FAK-SUBTO-TOT-RECALC                 
196500     MOVE BILL-KDVALISO-RECALC    TO FAK-KDVALISO-RECALC                  
196600     MOVE BILL-PRKURS-RECALC      TO FAK-PRKURS-RECALC                    
196700     MOVE BILL-KDTECKEN-RECALC    TO FAK-KDTECKEN-RECALC                  
196800     MOVE BILL-SUNTO-LOCC-RECALC  TO FAK-SUNTO-LOCC-RECALC                
196900     .                                                                    
197000     EJECT                                                                
197100 MB-FLYTTA-FAKT-KLI SECTION.                                              
197200     MOVE 'MB-FLYTTA-FAKT-KLI   ' TO WS-SEKTION                           
197300                                                                          
197400     MOVE BILL-IDPRODNR           TO FAKC-IDPRODNR                        
197500     MOVE BILL-IDKOLLI            TO FAKC-IDKOLLI                         
197600     MOVE BILL-IDDISTR            TO FAKC-IDDISTR                         
197700     MOVE BILL-IDKUNDNR           TO FAKC-IDKUNDNR                        
197800     MOVE '0000000   '            TO FAKC-IDKUNDRF                        
197900     MOVE BILL-IDKUNDRF (1:5)     TO FAKC-IDORDNR7                        
198000     MOVE BILL-DAFINDOC           TO FAKC-TIFAKT                          
198100     MOVE BILL-TIREGDAT           TO FAKC-TIORDREG                        
198200     MOVE BILL-IDSHIPM            TO FAKC-IDSHIPM                         
198300     MOVE BILL-TISKEPPN           TO FAKC-TISKEPPN                        
198400     MOVE BILL-KDFRAKT            TO FAKC-KDFRAKT                         
198500     MOVE BILL-KDKOLLI            TO FAKC-KDKOLLI                         
198600     MOVE BILL-KDORDKL            TO FAKC-KDORDKL                         
198700     MOVE BILL-VKORDBTO-KOLLI     TO FAKC-VKORDBTO-KOLLI                  
198800     MOVE BILL-VKORDNTO-KOLLI     TO FAKC-VKORDNTO-KOLLI                  
198900     .                                                                    
199000     EJECT                                                                
199100 MC-FLYTTA-FAKT-RAD SECTION.                                              
199200     MOVE 'MC-FLYTTA-FAKT-RAD   ' TO WS-SEKTION                           
199300                                                                          
199400     MOVE BILL-IDARTNR            TO FAKL-IDARTNR                         
199500     MOVE BILL-IDPURAD            TO FAKL-IDPURAD                         
199600     MOVE BILL-BEART              TO FAKL-BEART                           
199700     MOVE BILL-IDBORD             TO FAKL-IDBORD                          
199800     MOVE BILL-IDLEVNR            TO FAKL-IDLEVNR                         
199900     MOVE BILL-IDUSER-OREG        TO FAKL-IDUSER-OREG                     
200000     MOVE BILL-IDUSER-PACK        TO FAKL-IDUSER-PACK                     
200100     MOVE BILL-KVAVBART           TO FAKL-KVAVBART                        
200200     MOVE BILL-KVBEART            TO FAKL-KVBEART-Q                       
200300     MOVE BILL-KVLEVART           TO FAKL-KVLEVART                        
200400     MOVE BILL-KVORDRAD           TO FAKL-KVORDRAD                        
200500     MOVE BILL-PRARTNTO           TO FAKL-PRARTNTO                        
200600     MOVE BILL-PRAVCOST-BILLIT    TO FAKL-PRAVCOST                        
200700     MOVE BILL-KDVALISO-AVC       TO FAKL-KDVALISO-AVC                    
200800     MOVE BILL-PRARTNTO-LOC       TO FAKL-PRARTNTO-LOC                    
200900     MOVE BILL-VKARTNTO           TO FAKL-VKARTNTO                        
201000     MOVE BILL-VKART-NTO-KG       TO FAKL-VKART-NTO-KG                    
201100     MOVE BILL-ADLAGOMR           TO FAKL-ADLAGOMR                        
201200     MOVE BILL-FLPCOO             TO FAKL-FLPCOO                          
201300     MOVE BILL-KDARTURS           TO FAKL-KDARTURS                        
201400     MOVE BILL-SUBTO-LINE         TO FAKL-SUBTO-LINE                      
201500     MOVE BILL-SUNTO-LINE         TO FAKL-SUNTO-LINE                      
201600     MOVE BILL-SUVAT-LINE         TO FAKL-SUVAT-LINE                      
201700     MOVE BILL-IDFKNGRP           TO FAKL-IDFKNGRP                        
201800     MOVE BILL-KDARTRAB           TO FAKL-KDARTRAB                        
201900     MOVE BILL-KDPRODSL           TO FAKL-KDPRODSL                        
202000     MOVE BILL-PRAVCOST-CORE      TO FAKL-PRAVCOST-CORE                   
202100     MOVE BILL-PRKURS-BET         TO FAKL-PRKURS-BET                      
202200     MOVE BILL-PRKURS-FAKT        TO FAKL-PRKURS-FAKT                     
202300     MOVE BILL-PRKURS-FIKTIV      TO FAKL-PRKURS-FIKTIV                   
202400     MOVE BILL-KDFAKSTA-EXP       TO FAKL-KDFAKSTA-EXP                    
202500     MOVE BILL-KDVALISO-BET       TO FAKL-KDVALISO-BET                    
202600     MOVE BILL-KDVALISO-NTO       TO FAKL-KDVALISO-NTO                    
202700     .                                                                    
202800     EJECT                                                                
202900 Z-FINIT SECTION.                                                         
203000     MOVE 'Z-FINIT'   TO WS-SEKTION                                       
203100                                                                          
203200     CLOSE W47665                                                         
203300                                                                          
203400*    NOLLA ÅTERSTARTINFORMATIONEN                                         
203500     PERFORM IMS-LAS-ATERSTART                                            
203600     MOVE +0                   TO 4580-KVPOST                             
203700     ACCEPT 4580-TIUPPDAT FROM DATE                                       
203800     ACCEPT 4580-TIUPPTID FROM TIME                                       
203900                                                                          
204000     PERFORM IMS-REPL-ATERSTART                                           
204100                                                                          
204200     SKIP2                                                                
204300     MOVE 'S' TO POSTSUM-OPKOD                                            
204400     CALL POSTSUM USING POSTSUM-PARM                                      
204500     .                                                                    
204600     EJECT                                                                
204700 S01-LAES-W47665  SECTION.                                                
204800                                                                          
204900     READ W47665 INTO IN-AREA                                             
205000     AT END                                                               
205100        SET END-OF-W47665 TO TRUE                                         
205200     NOT AT END                                                           
205300        MOVE 'W47665'       TO POSTSUM-FDNAMN                             
205400        MOVE 'W47665D1'     TO POSTSUM-DDNAMN2                            
205500        MOVE SPACE          TO POSTSUM-TRANSTYP                           
205600        CALL POSTSUM USING POSTSUM-PARM                                   
205700     END-READ                                                             
205800     .                                                                    
205900     EJECT                                                                
206000 S09-HAMTA-ARTS  SECTION.                                                 
206100     MOVE 'S09-HAMTA-ARTS'       TO WS-SEKTION                            
206200                                                                          
206300     MOVE BILL-IDARTNR           TO W-IDARTNR                             
206400                                    W-IDARTNR-CORE                        
206500                                    TEST-IDARTNR                          
206600     PERFORM IMS-GHU-WDK711                                               
206700     IF SEGMENT-SAKNAS                                                    
206800       PERFORM S09A-LAGG-UPP-SLAGSEG                                      
206900     END-IF                                                               
207000     IF BYT19-BYTES OR BYT19-RADIO                                        
207100       IF BYT19-BYTES                                                     
207200         ADD +6000            TO W-IDARTNR-CORE                           
207300       ELSE                                                               
207400         IF BYT19-RADIO                                                   
207500           ADD +1000          TO W-IDARTNR-CORE                           
207600         END-IF                                                           
207700       END-IF                                                             
207800       PERFORM IMS-GU-WDK711-CORE                                         
207900       IF SEGMENT-SAKNAS                                                  
208000         MOVE W-IDARTNR-CORE  TO W-IDARTNR                                
208100         PERFORM S09A-LAGG-UPP-SLAGSEG                                    
208200         MOVE BILL-IDARTNR   TO W-IDARTNR                                 
208300         MOVE ZERO           TO CORE-SLAG-PRAVCOST                        
208400       END-IF                                                             
208500     ELSE                                                                 
208600       MOVE ZERO             TO CORE-SLAG-PRAVCOST                        
208700     END-IF                                                               
208800     .                                                                    
208900     SKIP2                                                                
209000 S09A-LAGG-UPP-SLAGSEG  SECTION.                                          
209100                                                                          
209200     MOVE ALL '+'      TO WDK7-W005WDK7                                   
209300     MOVE 'WDK711'     TO WDK7-IDSEGM                                     
209400     MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                                
209500     MOVE W-IDDC       TO WDK7-IDDC-KFB                                   
209600                          WDK7-IDDC                                       
209700                                                                          
209800     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-PCB                  
209900                                       WDK7-PCB                           
210000                                                                          
210100     IF W-IDDC NOT = DCS-IDDC                                             
210200        MOVE W-IDDC           TO W-IDDC-B6                                
210300        PERFORM IMS-GU-WDB601                                             
210400     END-IF                                                               
210500     .                                                                    
210600     EJECT                                                                
210700                                                                          
210800 S28-IMS-ISRT-WDL901  SECTION.                                            
210900     MOVE 'S28-IMS-ISRT-WDL901'  TO WS-SEKTION                            
211000                                                                          
211100     PERFORM IMS-ISRT-WDL901                                              
211200     PERFORM UNTIL SEGMENT-FINNS                                          
211300        SUBTRACT 1            FROM LOGG-IDSEKVNR                          
211400        PERFORM IMS-ISRT-WDL901                                           
211500                                                                          
211600     END-PERFORM                                                          
211700     .                                                                    
211800     EJECT                                                                
211900* --- IMS SEKTIONER ---                                                   
212000                                                                          
212100 IMS-GU-WDK601 SECTION.                                                   
212200     MOVE 'IMS-GU-WDK601'       TO WS-SEKTION                             
212300                                                                          
212400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
212500          DELIMITED BY SIZE INTO SSA1                                     
212600     MOVE '    '                TO GODK-STATUSKODER                       
212700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
212800     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
212900     PERFORM IMS-STATUSKONTROLL                                           
213000     .                                                                    
213100     SKIP3                                                                
213200 IMS-GNP-WDK611 SECTION.                                                  
213300     MOVE 'IMS-GNP-WDK611'       TO WS-SEKTION                            
213400                                                                          
213500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
213600          DELIMITED BY SIZE INTO SSA1                                     
213700     MOVE 'WDK611 '             TO SSA2                                   
213800     MOVE '    '                TO GODK-STATUSKODER                       
213900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
214000     MOVE WDK6-STATUS-CODE      TO STATUS-WS                              
214100     PERFORM IMS-STATUSKONTROLL                                           
214200     .                                                                    
214300     SKIP3                                                                
214400 IMS-GHNP-WDK611 SECTION.                                                 
214500     MOVE 'IMS-GHNP-WDK611'      TO WS-SEKTION                            
214600*                                                                         
214700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
214800          DELIMITED BY SIZE INTO SSA1                                     
214900     MOVE 'WDK611 '           TO SSA2                                     
215000     MOVE '    '              TO GODK-STATUSKODER                         
215100     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1 SSA2             
215200     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
215300     PERFORM IMS-STATUSKONTROLL                                           
215400     .                                                                    
215500     SKIP3                                                                
215600 IMS-REPL-WDK611 SECTION.                                                 
215700     MOVE 'IMS-REPL-WDK611'      TO WS-SEKTION                            
215800                                                                          
215900     MOVE '  '                TO GODK-STATUSKODER                         
216000     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
216100     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
216200     PERFORM IMS-STATUSKONTROLL                                           
216300     .                                                                    
216400     EJECT                                                                
216500 IMS-GHU-WDK711 SECTION.                                                  
216600     MOVE 'IMS-GHU-WDK711'       TO WS-SEKTION                            
216700                                                                          
216800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
216900          DELIMITED BY SIZE INTO SSA1                                     
217000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
217100          DELIMITED BY SIZE INTO SSA2                                     
217200     MOVE '  GE'                 TO GODK-STATUSKODER                      
217300     CALL CBLTDLI USING GHU  WDK7-PCB DLI-IO-WDK711 SSA1 SSA2             
217400     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
217500     PERFORM IMS-STATUSKONTROLL                                           
217600     .                                                                    
217700     SKIP3                                                                
217800 IMS-GU-WDK711-CORE SECTION.                                              
217900     MOVE 'IMS-GU-WDK711-CORE'    TO WS-SEKTION                           
218000                                                                          
218100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-CORE-X ')'                    
218200          DELIMITED BY SIZE INTO SSA1                                     
218300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
218400          DELIMITED BY SIZE INTO SSA2                                     
218500     MOVE '  GE'                 TO GODK-STATUSKODER                      
218600     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK711-CORE SSA1 SSA2         
218700     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
218800     PERFORM IMS-STATUSKONTROLL                                           
218900     .                                                                    
219000     SKIP3                                                                
219100 IMS-REPL-WDK711 SECTION.                                                 
219200     MOVE 'IMS-REPL-WDK711'      TO WS-SEKTION                            
219300                                                                          
219400     MOVE '  '                   TO GODK-STATUSKODER                      
219500     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
219600     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
219700     PERFORM IMS-STATUSKONTROLL                                           
219800     .                                                                    
219900     EJECT                                                                
220000 IMS-GU-WDB101 SECTION.                                                   
220100     MOVE 'IMS-GU-WDB101'       TO WS-SEKTION                             
220200*                                                                         
220300     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
220400          DELIMITED BY SIZE INTO SSA1                                     
220500     MOVE '    '                TO GODK-STATUSKODER                       
220600     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
220700     MOVE WDB1-STATUS-CODE      TO STATUS-WS                              
220800     PERFORM IMS-STATUSKONTROLL                                           
220900     .                                                                    
221000     EJECT                                                                
221100 IMS-GU-WDB201              SECTION.                                      
221200     MOVE 'IMS-GU-WDB201'   TO WS-SEKTION                                 
221300                                                                          
221400     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
221500          DELIMITED BY SIZE INTO SSA1                                     
221600     MOVE '  GE'              TO GODK-STATUSKODER                         
221700     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
221800     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
221900     PERFORM IMS-STATUSKONTROLL                                           
222000     .                                                                    
222100     EJECT                                                                
222200 IMS-GHU-WDM701  SECTION.                                                 
222300     MOVE 'IMS-GHU-WDM701'       TO WS-SEKTION                            
222400                                                                          
222500     STRING 'WDM701  (WDM701KY =' W-WDM701KY-X ')'                        
222600          DELIMITED BY SIZE INTO SSA1                                     
222700     MOVE '  GE'                 TO GODK-STATUSKODER                      
222800     CALL CBLTDLI USING GHU WDM7-PCB DLI-IO-WDM701 SSA1                   
222900     MOVE WDM7-STATUS-CODE       TO STATUS-WS                             
223000     PERFORM IMS-STATUSKONTROLL                                           
223100     .                                                                    
223200     SKIP3                                                                
223300 IMS-ISRT-WDM701 SECTION.                                                 
223400     MOVE 'IMS-ISRT-WDM701'      TO WS-SEKTION                            
223500                                                                          
223600     MOVE 'WDM701 ' TO SSA1                                               
223700     MOVE '  II'                 TO GODK-STATUSKODER                      
223800     CALL CBLTDLI USING ISRT WDM7-PCB DLI-IO-WDM701 SSA1                  
223900     MOVE WDM7-STATUS-CODE       TO STATUS-WS                             
224000     PERFORM IMS-STATUSKONTROLL                                           
224100     .                                                                    
224200     SKIP3                                                                
224300 IMS-ISRT-WDM801 SECTION.                                                 
224400     MOVE 'IMS-ISRT-WDM801'      TO WS-SEKTION                            
224500                                                                          
224600     MOVE 'WDM801 ' TO SSA1                                               
224700     MOVE '  II'                 TO GODK-STATUSKODER                      
224800     CALL CBLTDLI USING ISRT WDM8-PCB DLI-IO-WDM801 SSA1                  
224900     MOVE WDM8-STATUS-CODE       TO STATUS-WS                             
225000     PERFORM IMS-STATUSKONTROLL                                           
225100     .                                                                    
225200     SKIP3                                                                
225300 IMS-ISRT-WDR801  SECTION.                                                
225400     MOVE 'IMS-ISRT-WDR801'      TO WS-SEKTION                            
225500                                                                          
225600     MOVE '  II'                 TO GODK-STATUSKODER                      
225700     MOVE 'WDR801   ' TO SSA1                                             
225800     CALL CBLTDLI USING ISRT WDR8-PCB DLI-IO-WDR801 SSA1                  
225900     MOVE WDR8-STATUS-CODE       TO STATUS-WS                             
226000     PERFORM IMS-STATUSKONTROLL                                           
226100     .                                                                    
226200     EJECT                                                                
226300 IMS-ISRT-WDL901 SECTION.                                                 
226400     MOVE 'IMS-ISRT-WDL901'      TO WS-SEKTION                            
226500                                                                          
226600     MOVE 'WDL901 ' TO SSA1                                               
226700     MOVE '  II'                 TO GODK-STATUSKODER                      
226800     CALL CBLTDLI USING ISRT WDL9-PCB DLI-IO-WDL901 SSA1                  
226900     MOVE WDL9-STATUS-CODE       TO STATUS-WS                             
227000     PERFORM IMS-STATUSKONTROLL                                           
227100     .                                                                    
227200     SKIP3                                                                
227300 IMS-LAS-ATERSTART  SECTION.                                              
227400     MOVE 'IMS-LAS-ATERSTART'     TO WS-SEKTION                           
227500                                                                          
227600     MOVE '4579'         TO IDHTYP                                        
227700     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
227800     MOVE 'W4766500'     TO NYCKEL-VALFRI(1:8)                            
227900     STRING 'WDR401  (WDGXKEY  =' WDGX01 ')'                              
228000                    DELIMITED BY SIZE INTO SSA1                           
228100     MOVE 'WDR470   '    TO SSA2                                          
228200     MOVE '  GE'           TO GODK-STATUSKODER                            
228300     CALL CBLTDLI USING GHU 4579-PCB 4580-IO-AREA SSA1 SSA2               
228400     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
228500     PERFORM IMS-STATUSKONTROLL                                           
228600     .                                                                    
228700     SKIP2                                                                
228800 IMS-REPL-ATERSTART SECTION.                                              
228900     MOVE 'IMS-REPL-ATERSTART'     TO WS-SEKTION                          
229000                                                                          
229100     MOVE '  '             TO GODK-STATUSKODER                            
229200     CALL CBLTDLI USING REPL 4579-PCB 4580-IO-AREA                        
229300     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
229400     PERFORM IMS-STATUSKONTROLL                                           
229500     .                                                                    
229600     SKIP2                                                                
229700 IMS-ISRT-ATERSTART SECTION.                                              
229800     MOVE 'IMS-ISRT-ATERSTART'   TO WS-SEKTION                            
229900                                                                          
230000     MOVE '4579'         TO IDHTYP                                        
230100     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
230200     MOVE 'W4766500'     TO NYCKEL-VALFRI(1:8)                            
230300     STRING 'WDR401  (WDGXKEY  =' WDGX01 ')'                              
230400                    DELIMITED BY SIZE INTO SSA1                           
230500     MOVE 'WDR470   '    TO SSA2                                          
230600     MOVE '  '           TO GODK-STATUSKODER                              
230700     CALL CBLTDLI USING ISRT 4579-PCB 4580-IO-AREA SSA1 SSA2              
230800     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
230900     PERFORM IMS-STATUSKONTROLL                                           
231000     .                                                                    
231100 IMS-RESTART SECTION.                                                     
231200     MOVE 'IMS-RESTART '         TO WS-SEKTION                            
231300                                                                          
231400     MOVE SPACE    TO CHKP-MSG-IO-AREA                                    
231500     MOVE '  '     TO GODK-STATUSKODER                                    
231600     CALL CBLTDLI USING XRST MSG-PCB                                      
231700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
231800                        CHKP-AREA-LENGTH CHKP-AREA                        
231900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
232000     PERFORM IMS-STATUSKONTROLL                                           
232100     .                                                                    
232200     SKIP3                                                                
232300 IMS-CHECKPOINT SECTION.                                                  
232400     MOVE 'IMS-CHECKPOINT'       TO WS-SEKTION                            
232500                                                                          
232600     MOVE SPACE    TO CHKP-MSG-IO-AREA                                    
232700     MOVE '  XD'   TO GODK-STATUSKODER                                    
232800     CALL CBLTDLI USING CHKP MSG-PCB                                      
232900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
233000                        CHKP-AREA-LENGTH CHKP-AREA                        
233100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
233200     PERFORM IMS-STATUSKONTROLL                                           
233300                                                                          
233400     IF IMS-NOT-OK                                                        
233500       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO ERRTEXT-STR         
233600       DISPLAY ERRTEXT                                                    
233700       CALL FELLOG                                                        
233800     END-IF                                                               
233900     .                                                                    
234000     EJECT                                                                
234100 IMS-GHU-WDE101 SECTION.                                                  
234200                                                                          
234300     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
234400          DELIMITED BY SIZE INTO SSA1                                     
234500     MOVE '  GE' TO GODK-STATUSKODER                                      
234600     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
234700     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
234800     PERFORM IMS-STATUSKONTROLL                                           
234900     .                                                                    
235000     EJECT                                                                
235100 IMS-GU-WDE111 SECTION.                                                   
235200                                                                          
235300     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
235400          DELIMITED BY SIZE INTO SSA1                                     
235500     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
235600          DELIMITED BY SIZE INTO SSA2                                     
235700     MOVE '    ' TO GODK-STATUSKODER                                      
235800     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE111 SSA1 SSA2               
235900     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
235910     PERFORM IMS-STATUSKONTROLL                                           
235920     .                                                                    
235930     EJECT                                                                
236000 IMS-GU-WDE121 SECTION.                                                   
236100     MOVE 'IMS-GU-WDE121'      TO WS-SEKTION                              
236200                                                                          
236300     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
236400          DELIMITED BY SIZE INTO SSA1                                     
236500     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
236600          DELIMITED BY SIZE INTO SSA2                                     
236700     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
236800          DELIMITED BY SIZE INTO SSA3                                     
236900     MOVE '  GE' TO GODK-STATUSKODER                                      
237000     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE121 SSA1 SSA2 SSA3          
237100     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
237200     PERFORM IMS-STATUSKONTROLL                                           
237300     .                                                                    
237400     EJECT                                                                
238810 IMS-GNP-WDE122 SECTION.                                                  
238820                                                                          
238830     MOVE 'WDE122  '          TO SSA1                                     
238840     MOVE '  GE' TO GODK-STATUSKODER                                      
238850     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE122 SSA1                   
238860     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
238870     PERFORM IMS-STATUSKONTROLL                                           
238880     .                                                                    
238890     EJECT                                                                
238900 IMS-GU-WDK712 SECTION.                                                   
239000     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
239100          DELIMITED BY SIZE INTO SSA1                                     
239200     STRING 'WDK712  (IDLAND  = ' W-IDLAND-X ')'                          
239300          DELIMITED BY SIZE INTO SSA2                                     
239400     MOVE '  GE' TO GODK-STATUSKODER                                      
239500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
239600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
239700     PERFORM IMS-STATUSKONTROLL                                           
239800     .                                                                    
239900     SKIP3                                                                
240000 IMS-GU-WDB601    SECTION.                                                
240100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
240200          DELIMITED BY SIZE INTO SSA1                                     
240300     MOVE '  GE' TO GODK-STATUSKODER                                      
240400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
240500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
240600     PERFORM IMS-STATUSKONTROLL                                           
240700     IF SEGMENT-SAKNAS                                                    
240800         MOVE SPACE TO DCS-KDDC                                           
240900     END-IF                                                               
241000     .                                                                    
241100 IMS-GHU-WDA901 SECTION.                                                  
241200                                                                          
241300     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-WDA9-X ')'                    
241400          DELIMITED BY SIZE INTO SSA1                                     
241500     MOVE '  GE'           TO GODK-STATUSKODER                            
241600     CALL CBLTDLI USING GHU WDA9-PCB DLI-IO-WDA901 SSA1                   
241700     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
241800     PERFORM IMS-STATUSKONTROLL                                           
241900     .                                                                    
242000 IMS-GHU-WDA912 SECTION.                                                  
242100                                                                          
242200     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-WDA9-X ')'                    
242300          DELIMITED BY SIZE INTO SSA1                                     
242400     STRING 'WDA912  (DAAAPPR  =' W-DAAAPP-WDA9-X ')'                     
242500          DELIMITED BY SIZE INTO SSA2                                     
242600     MOVE '  GE'           TO GODK-STATUSKODER                            
242700     CALL CBLTDLI USING GHU WDA9-PCB DLI-IO-WDA912 SSA1                   
242800                                                   SSA2                   
242900     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
243000     PERFORM IMS-STATUSKONTROLL                                           
243100     .                                                                    
243200 IMS-ISRT-WDA901 SECTION.                                                 
243300                                                                          
243400     MOVE 'WDA901   '      TO SSA1                                        
243500     MOVE '  '             TO GODK-STATUSKODER                            
243600     CALL CBLTDLI USING ISRT WDA9-PCB DLI-IO-WDA901 SSA1                  
243700     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
243800     PERFORM IMS-STATUSKONTROLL                                           
243900     .                                                                    
244000 IMS-ISRT-WDA912 SECTION.                                                 
244100                                                                          
244200     MOVE 'WDA912   '      TO SSA1                                        
244300     MOVE '  '             TO GODK-STATUSKODER                            
244400     CALL CBLTDLI USING ISRT WDA9-PCB DLI-IO-WDA912 SSA1                  
244500     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
244600     PERFORM IMS-STATUSKONTROLL                                           
244700     .                                                                    
244800 IMS-REPL-WDA912 SECTION.                                                 
244900                                                                          
245000     MOVE '  '             TO GODK-STATUSKODER                            
245100     CALL CBLTDLI USING REPL WDA9-PCB DLI-IO-WDA912                       
245200     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
245300     PERFORM IMS-STATUSKONTROLL                                           
245400     .                                                                    
245500     EJECT                                                                
245600 IMS-GHU-WDGX3161 SECTION.                                                
245700                                                                          
245800     STRING 'WDR401  (WDGXKEY  =' W-3161-WDGXKEY-X ')'                    
245900          DELIMITED BY SIZE INTO SSA1                                     
246000     MOVE '  GE'           TO GODK-STATUSKODER                            
246100     CALL CBLTDLI USING GHU  3161-PCB DLI-IO-WDGX3161 SSA1                
246200     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
246300     PERFORM IMS-STATUSKONTROLL                                           
246400     .                                                                    
246500 IMS-ISRT-WDGX3161 SECTION.                                               
246600                                                                          
246700     MOVE 'WDR401   '      TO SSA1                                        
246800     MOVE '  II'           TO GODK-STATUSKODER                            
246900     CALL CBLTDLI USING ISRT 3161-PCB DLI-IO-WDGX3161 SSA1                
247000     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
247100     PERFORM IMS-STATUSKONTROLL                                           
247200     .                                                                    
247300 IMS-ISRT-WDGX3162 SECTION.                                               
247400                                                                          
247500     MOVE 'WDGX3162 '      TO SSA1                                        
247600     MOVE '  '             TO GODK-STATUSKODER                            
247700     CALL CBLTDLI USING ISRT 3161-PCB DLI-IO-WDGX3162 SSA1                
247800     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
247900     PERFORM IMS-STATUSKONTROLL                                           
248000     .                                                                    
248100     EJECT                                                                
248200 IMS-GHU-WDL501 SECTION.                                                  
248300                                                                          
248400     STRING 'WDL501  (IDFAKT   =' W-IDFAKT-X ')'                          
248500          DELIMITED BY SIZE INTO SSA1                                     
248600     MOVE '  GE' TO GODK-STATUSKODER                                      
248700     CALL CBLTDLI USING GHU WDL5-PCB DLI-IO-WDL501 SSA1                   
248800     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
248900     PERFORM IMS-STATUSKONTROLL                                           
249000     .                                                                    
249100     EJECT                                                                
249200 IMS-REPL-WDL501 SECTION.                                                 
249300                                                                          
249400     MOVE '  '             TO GODK-STATUSKODER                            
249500     CALL CBLTDLI USING REPL WDL5-PCB DLI-IO-WDL501                       
249600     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
249700     PERFORM IMS-STATUSKONTROLL                                           
249800     .                                                                    
249900     EJECT                                                                
250000 IMS-ISRT-WDL501 SECTION.                                                 
250100                                                                          
250200     MOVE 'WDL501   '      TO SSA1                                        
250300     MOVE '  '             TO GODK-STATUSKODER                            
250400     CALL CBLTDLI USING ISRT WDL5-PCB DLI-IO-WDL501 SSA1                  
250500     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
250600     PERFORM IMS-STATUSKONTROLL                                           
250700     .                                                                    
250800     EJECT                                                                
250900 IMS-GHNP-WDL511 SECTION.                                                 
251000                                                                          
251100     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
251200          DELIMITED BY SIZE INTO SSA1                                     
251300     MOVE '  GE' TO GODK-STATUSKODER                                      
251400     CALL CBLTDLI USING GHNP WDL5-PCB DLI-IO-WDL511 SSA1                  
251500     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
252000     PERFORM IMS-STATUSKONTROLL                                           
252100     .                                                                    
252200     EJECT                                                                
252300 IMS-ISRT-WDL511 SECTION.                                                 
252400                                                                          
252500     STRING 'WDL501  (IDFAKT   =' W-IDFAKT-X ')'                          
252600          DELIMITED BY SIZE INTO SSA1                                     
252700     MOVE 'WDL511   '      TO SSA2                                        
252800     MOVE '  '             TO GODK-STATUSKODER                            
252900     CALL CBLTDLI USING ISRT WDL5-PCB DLI-IO-WDL511 SSA1 SSA2             
253000     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
253100     PERFORM IMS-STATUSKONTROLL                                           
253200     .                                                                    
253300 IMS-ISRT-WDL521 SECTION.                                                 
253400                                                                          
253500     STRING 'WDL501  (IDFAKT   =' W-IDFAKT-X ')'                          
253600          DELIMITED BY SIZE INTO SSA1                                     
253700     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
253800          DELIMITED BY SIZE INTO SSA2                                     
253900     MOVE 'WDL521   '      TO SSA3                                        
254000     MOVE '  '             TO GODK-STATUSKODER                            
254100     CALL CBLTDLI USING ISRT WDL5-PCB DLI-IO-WDL521 SSA1 SSA2 SSA3        
254200     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
254300     PERFORM IMS-STATUSKONTROLL                                           
254400     .                                                                    
254500     EJECT                                                                
254600 IMS-STATUSKONTROLL SECTION.                                              
254700                                                                          
254800     SET STATUS-IX TO 1                                                   
254900     SEARCH GODK-STATUS                                                   
255000       AT END                                                             
255100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
255200           DELIMITED BY SIZE INTO FELTEXT                                 
255300         CALL FELLOG                                                      
255400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
255500         CONTINUE                                                         
255600     END-SEARCH                                                           
255700     .                                                                    
