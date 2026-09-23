000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W4403000.                                        
000400 AUTHOR.                 INGER NILSSON.                                   
000500 DATE-WRITTEN.           JUNI 1988.                                       
000600                                                                          
000700     REMARKS.                                                             
000800******************************************************************        
000900* DOSKALLE CREATION FOR KDROPACK 3 AND DIFF ADDRESS FOR CDC               
000910******************************************************************        
000920* READS ACTION DATABSE 4505 AND 4506 WHENEVER THERE IS AN INCREASE        
000930* IN STOCK FOR A PARTICULAR PARTNO ON SCREEN 5106                         
000940* AND THEN CALLS WDK6 AND UPDATE STOCK ON WDK6                            
000950*                                                                         
000960* IT READS KDSTRAD AS 2 I.E. IT IS READY TO BE RELEASED                   
000970* FROM WDA5 AND WHEN DOSKALLE IS TO BE CREATED THEN MOVES                 
000980* KDSTRAD AS 4                                                            
000990******************************************************************        
001000* READS BO LINE FROM WDA5 WITH KDROPACK AS 3 AND KDSTARAD 2               
001100* KDSTARAD IS STATUS LINE CODE                                            
001200* KDSTARAD = 1 (TPO NOT USED IN W4403000)                                 
001210* KDSTARAD = 2 (SCREEN 4572 BACKORDERD READ AS 2 IN W440300)              
001220* KDSTARAD = 3 (SCREEN 4573 RESERVED MOVED IN W4403000)                   
001230* KDSTARAD = 4 (SCREEN 4574 RELEASED MOVED IN W411BIPA)                   
001240******************************************************************        
001250* THEN CALLS SUBPROGRAM W440EMOH                                          
001260* THERE IT STORES ORDER DETAILS IN A INTERNAL TABLE                       
001270* THIS INTERNAL TABLE HAS VALUES (DIST+CUST+CLASS+ADDRESS)                
001280*                                                                         
001290* IN SUBPROGRAM W440EMOH BELOW STEPS ARE FOLLOWED                         
001300* 1. IF NEW ORDER IS NOT MATCHED WITH THE ORDERS PRESENT IN TABL          
001400*    THEN IT CREATES A DOSKALLE (CREATING A NEW ORDERHEAD) I.E. IT        
001500*    CALLS 4251 WITH THE SAME DETAILS BUT A NEW ORDER NO AND              
001600*    KDSTRAD IS 4 I.E. ORDER IS RELEASED FOUND ON 4574                    
001700* 2. IF MATCHED THEN NEW DOSKALLE IS NOT CREATED IT SAVES THE             
001800*    ORDERNO FROM THE FIRST ORDER IN BERARDREF(WDA5)AND IN                
001900*    W411BIPA IT CHECKS IF ORDERS HAVE SAME BERADREF THEN THEY            
002000*    SHOULD BE ATTACHED TOGTHER BECAUSE THEY HAVE SIMILAR DETAILS         
002100*    CAN BE FOUND WITH OLDODERNO ON 4574 AND ATTACHED TO SAYS THE         
002200*    NEW ORDERNO , MORE THEN ONE ORDER CAN BE ATTACHED TOGTHER            
002300*                                                                         
002400******************************************************************        
002500*                                                                         
002600*    FUNKTION:                                                            
002700*        TÄCKNING AV RESTORDER.                                           
002800*                                                                         
002900*        INDATA. WDA5 WDK6                                                
003000*                WDG2 (STYRREG)                                           
003100*                WDK9                                                     
003200*                WDR4 (TÄCKN.TRANS)                                       
003300*                WDK7                                                     
003400*                                                                         
003500*        UTDATA. WDG6                                                     
003600*                                                                         
003700*        UPPDATERA WDA5 WDK6 WDG6 WDG2 WDR4                               
003800*                                                                         
003900*    STORY 2375089 ADD IDSYSTEM VOUI, ECOM                                
004000*                                                                         
004100     EJECT                                                                
004200 ENVIRONMENT DIVISION.                                                    
004300                                                                          
004400 INPUT-OUTPUT SECTION.                                                    
004500                                                                          
004600 FILE-CONTROL.                                                            
004700     SKIP2                                                                
004800     EJECT                                                                
004900 DATA DIVISION.                                                           
005000                                                                          
005100 FILE SECTION.                                                            
005200     SKIP3                                                                
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005600*    -COPY WY2000W9                                                       
005700 77  IDPGM                   PIC X(8)   VALUE 'W4403000'.                 
005800 77  FELTEXT                 PIC X(80)  VALUE SPACE.                      
005900     SKIP2                                                                
006000*    ---- ARBETSVARIABLER                                                 
006100*                                                                         
006200*01  -COPY WWPRODSL                                                       
006300*                                                                         
006400 77  CHKP-ID                 PIC X(08)  VALUE 'W4403000'.                 
006500 77  CHKP-ANT                PIC S9(3)  COMP-3 VALUE +0.                  
006600 77  CHKP-MAX                PIC S9(3)  COMP-3 VALUE 50.                  
006700 77  MSG-IO-AREA-LENGTH-1    PIC S9(9)  VALUE +32 COMP SYNC.              
006800 77  MSG-IO-AREA-1           PIC X(32)  VALUE SPACE.                      
006900 77  CHKP-AREA-1-LENGTH      PIC S9(9)  VALUE +32 COMP SYNC.              
007000 77  CHKP-AREA-1             PIC X(32)  VALUE SPACE.                      
007100                                                                          
007200 77  W-SUM-KATEGORINORMAL    PIC S9(12)V9(3) COMP-3 VALUE ZERO.           
007300 77  W-SUM-KATEGORINORMAL-PRIO PIC S9(12)V9(3) COMP-3 VALUE ZERO.         
007400 77  W-TACKNINGSGRAD         PIC S9(03)V9(5) COMP-3 VALUE ZERO.           
007500 77  W-REST                  PIC S9(07) COMP-3 VALUE ZERO.                
007600 77  W-DISPONIBELT           PIC S9(07) COMP-3 VALUE ZERO.                
007700 77  W-DISPONIBELT-TOT       PIC S9(09) COMP-3 VALUE ZERO.                
007800 77  W-RO-ATT-TACKA          PIC S9(07) COMP-3 VALUE ZERO.                
007900 77  W-BERAKNA-TG            PIC X VALUE SPACE.                           
008000 77  W-KVAR-ATT-TACKA        PIC S9(07) COMP-3 VALUE ZERO.                
008100 77  W-KVTACKT               PIC S9(07) COMP-3 VALUE ZERO.                
008200 77  W-MIN-GRANS-RAD         PIC S9(07) COMP-3 VALUE ZERO.                
008300 77  W-ANTAL-ARTIKLAR        PIC S9(07) COMP-3 VALUE ZERO.                
008400 77  W-KVART                 PIC S9(07) COMP-3 VALUE ZERO.                
008500 77  W-SAMMANSLAGNING-RAD    PIC X.                                       
008600 77  W-TIAAMMDD              PIC 9(06).                                   
008700 77  W-TIDDD                 PIC 9(03).                                   
008800 77  W-TIAA                  PIC 9(02).                                   
008900 77  W-ANTAL-DAGAR-TACK      PIC 9(05).                                   
009000 77  W-ANTAL-RO-DAGAR        PIC 9(05).                                   
009100 77  W-KATEGORI-REST         PIC S9(09).                                  
009200 77  SW-TACKBART             PIC X              VALUE SPACE.              
009300 77  SW-RO-PA-ARTIKEL-SLUT   PIC X              VALUE 'J'.                
009400 77  WS-RELEASE-WIP          PIC X              VALUE 'N'.                
009500                                                                          
009600                                                                          
009700 77  W-IDKUNDRF-WIP          PIC X(10)          VALUE SPACE.              
009800 77  WS-TIRFS                PIC 9(6).                                    
009900                                                                          
010000 01  WS-DAT                         PIC 9(6).                             
010100 01  FILLER REDEFINES WS-DAT.                                             
010200     03 WS-YEAR                     PIC 9(2).                             
010300     03 WS-MONTH                    PIC 9(2).                             
010400     03 WS-DAYS                     PIC 9(2).                             
010500                                                                          
010600 77  WS-IDKONTO-DISPLAY      PIC 9(10)  VALUE ZERO.                       
010700                                                                          
010800 01  WS-IDORDNR-NUM                          PIC 9(7).                    
010900 01  WS-IDORDNR REDEFINES WS-IDORDNR-NUM     PIC X(7).                    
011000 01  WS-IDDISTR-NUM                          PIC 9(4).                    
011100 01  WS-IDDISTR REDEFINES WS-IDDISTR-NUM     PIC X(4).                    
011200 01  WS-IDKUNDNR-NUM                         PIC 9(6).                    
011300 01  WS-IDKUNDNR REDEFINES WS-IDKUNDNR-NUM   PIC X(6).                    
011400*                                                                         
011500 01  W-IDKUNDRF.                                                          
011600     03  W-IDORDNR           PIC 9(05).                                   
011700     03  FILLER              PIC X(05).                                   
011800*                                                                         
011900 01  W-SALDO-WDK6.                                                        
012000     03  W-KDERS             PIC S9(3) COMP-3 VALUE ZERO.                 
012100     03  W-KVLS              PIC S9(7) COMP-3.                            
012200     03  W-KVAKS-CDC         PIC S9(7) COMP-3.                            
012300     03  W-KVAKS-PAV         PIC S9(7) COMP-3.                            
012400     03  W-KVAKS-T           PIC S9(7) COMP-3.                            
012500     03  W-KVRESS            PIC S9(7) COMP-3.                            
012600     03  W-KVROS             PIC S9(7) COMP-3.                            
012700     03  W-KVSPANT           PIC S9(7) COMP-3.                            
012800     03  W-KVUTRS            PIC S9(7) COMP-3.                            
012900*                                                                         
013000 01  W-SUM-SALDO-WDK6.                                                    
013100     03  W-SUM-KVROS         PIC S9(9) COMP-3 VALUE ZERO.                 
013200     03  W-SUM-KVRESS        PIC S9(9) COMP-3 VALUE ZERO.                 
013300*                                                                         
013400 01  W-METOD                 PIC X.                                       
013500     88  W-METOD-1           VALUE '1'.                                   
013600     88  W-METOD-2           VALUE '2'.                                   
013700*                                                                         
013800 01  AVBRYT-KATEGORI-W       PIC X.                                       
013900     88  W-AVBRYT-KATEGORI   VALUE 'J'.                                   
014000*                                                                         
014100 01  W-TOT-BEHOV             PIC S9(7)V9(1) COMP-3 VALUE ZERO.            
014200*                                                                         
014300 01  SW-SATSORDER            PIC X.                                       
014400     88  SATS-ORDER          VALUE 'J'.                                   
014500     EJECT                                                                
014600*------------------------------- ARB.FÄLT FÖR TÄCKNING                    
014700 01      TACKW.                                                           
014800*                                                                         
014900  03     TACK-KDTAKORS       PIC S9(3)               COMP-3.              
015000   88    TACK-KDTAKORS-R32               VALUE +1, +2, +3.                
015100  03     TACK-KVDISP         PIC S9(7)               COMP-3.              
015200  03     TACK-TYP            PIC S9(1)               COMP-3.              
015300  03     TACK-TYP-MAXI       PIC S9(1)   VALUE +1    COMP-3.              
015400*                                MAXI; TÄCKER ALLA RO SOM C-LAGRET        
015500*                                KAN TÄCKA, DVS ÄVEN DE SOM KAN           
015600*                                RO-CLEARAS FRÅN ANDRA C-LAGRET.          
015700*                                                                         
015800  03     TACK-TYP-MINI       PIC S9(1)   VALUE +2    COMP-3.              
015900*                                MINI; TÄCKER ENDAST DE TILL              
016000*                                C-LAGRET TVÅNGSSTYRDA RO,                
016100*                                DVS ENDAST DE RO SOM EJ KAN              
016200*                                RO-CLEARAS TILL ANDRA C-LAGRET.          
016300*                                                                         
016400  03     TACK-TYP-EGET       PIC S9(1)   VALUE +3    COMP-3.              
016500*                                EGET; TÄCKER DE RO SOM TILLHÖR           
016600*                                DET EGNA C-LAGRET, VARKEN MER            
016700*                                ELLER MINDRE.                            
016800     SKIP3                                                                
016900*------------------------------- ARB.FÄLT FÖR SUB-PGM W440RROT            
017000 01      RROTW.                                                           
017100*                                                                         
017200   03    RROTW-TIDISPIN      PIC S9(7)               COMP-3.              
017300     EJECT                                                                
017400*                                                                         
017500 01  W-KATEGORI-TABELL.                                                   
017600     02  FILLER OCCURS 25.                                                
017700       03  TAB-KDRAPRIO       PIC S9(03)      COMP-3.                     
017800       03  TAB-FLPRIO         PIC X.                                      
017900       03  TAB-KVVECKOR-TECK  PIC S9(03)      COMP-3.                     
018000       03  TAB-REROFORD       PIC S9(03)      COMP-3.                     
018100       03  TAB-KVART          PIC S9(07)      COMP-3.                     
018200       03  TAB-KATEGORINORMAL PIC S9(09)V9(3) COMP-3.                     
018300       03  TAB-KATEGORI       PIC S9(09)      COMP-3.                     
018400                                                                          
018500*IDORDNR TABLE CREATED FROM W411ODNR                                      
018600 01  W-IDORDNR-TABELL.                                                    
018700     02  FILLER OCCURS 100.                                               
018800       03  TAB-IDORDNR        PIC 9(7)  VALUE ZERO.                       
018900*    ---- KONSTANTER                                                      
019000                                                                          
019100 77  JA                      PIC X       VALUE 'J'.                       
019200 77  NEJ                     PIC X       VALUE 'N'.                       
019300 77  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16 COMP SYNC.             
019400     SKIP2                                                                
019500*    ---- INDEXFÄLT                                                       
019600                                                                          
019700 77  ORD-IX                  PIC 99      VALUE ZERO.                      
019800 77  MAX-ORD-IX              PIC 99      VALUE 50.                        
019900 77  IX1-PRIO                PIC S9(3)   VALUE +0   COMP SYNC.            
020000 77  IX2                     PIC S9(3)   VALUE +0   COMP SYNC.            
020100 77  IX3                     PIC S9(3)   VALUE +0   COMP SYNC.            
020200 77  RFS-IX                  PIC S9(4)   VALUE +0   COMP SYNC.            
020300 77  MAX-RFS-IX              PIC S9(4)   VALUE +4   COMP SYNC.            
020400*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
020500     SKIP3                                                                
020600 01  DYNAMISKA-SUBPROGRAM.                                                
020700   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
020800   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
020900   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
021000   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
021100   03  VIMSREGT              PIC X(8)    VALUE 'VIMSREGT'.                
021200   03  W440RROT              PIC X(8)    VALUE 'W440RROT'.                
021300   03  W440EMOH              PIC X(8)    VALUE 'W440EMOH'.                
021400   03  W411ORDN              PIC X(8)    VALUE 'W411ORDN'.                
021500   03  W006KOM               PIC X(8)    VALUE 'W006KOM '.                
021600   03  WORKDAY               PIC X(8)    VALUE 'WORKDAY '.                
021700     EJECT                                                                
021800*    ----  PARAMETRAR TILL DATUMKORT                                      
021900                                                                          
022000 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
022100     SKIP3                                                                
022200*    -COPY WDATAREA                                                       
022300     EJECT                                                                
022400*    ----  PARAMETRAR TILL VIMSREGTIME FÖR KOLL OM BMP EL BATCH           
022500                                                                          
022600 01  FILLER.                                                              
022700     03  IMS-VIMSREGT        PIC S9(9)    COMP SYNC.                      
022800         88  BMP                          VALUE +8.                       
022900         88  BATCH                        VALUE +16 THRU +64.             
023000     EJECT                                                                
023100 01  FILLER                  PIC X(16)   VALUE 'W440RROTC0'.              
023200                                                                          
023300*    -COPY W440RROT                                                       
023400     EJECT                                                                
023500 01  FILLER                  PIC X(16)   VALUE 'W411EMOH'.                
023600*    --- PARAMETRAR TILL SUBPROGRAM W440EMOH                              
023700*01 -COPY W440EMOH                                                        
023800     EJECT                                                                
023900 01  FILLER                  PIC X(16)   VALUE 'W411ORDN'.                
024000*    --- PARAMETRAR TILL SUBPROGRAM W411ORDN                              
024100*01 -COPY W411ORDN                                                        
024200     EJECT                                                                
024300                                                                          
024400*    --- PARAMETRAR TILL SUBPROGRAM WORKDAY                               
024500*01 -COPY WORKAREA                                                        
024600                                                                          
024700*    ---- POST-AREOR OCH IMS KOMMUNIKATIONS-AREOR                         
024800     SKIP3                                                                
024900*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
025000                                                                          
025100 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
025200     SKIP3                                                                
025300*      --- VALID IDDC CODES                                               
025400*                                                                         
025500*01    -COPY WWDCKONS                                                     
025600       EJECT                                                              
025700*    ---- STATUSKOD FRÅN IMS                                              
025800                                                                          
025900 01  STATUS-WS               PIC XX.                                      
026000     88  SEGMENT-SLUT                     VALUE 'GB'.                     
026100     88  SEGMENT-FINNS                    VALUE '  '.                     
026200     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
026300     88  IMS-EJ-OK                        VALUE 'XD'.                     
026400     SKIP3                                                                
026500 01  GODK-STATUSKODER.                                                    
026600   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
026700     SKIP3                                                                
026800 01  SSA1                    PIC X(230).                                  
026900 01  SSA2                    PIC X(230).                                  
027000 01  SSA3                    PIC X(230).                                  
027100     EJECT                                                                
027200*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
027300                                                                          
027400 01  FILLER                  PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.          
027500 01  NYCKLAR-TILL-DLI.                                                    
027600                                                                          
027700   03  W-WDGX-4501-KEY-X.                                                 
027800     05  W-IDHTYP-4501       PIC X(04) VALUE '4501'.                      
027900     05  FILLER              PIC X(26) VALUE LOW-VALUE.                   
028000                                                                          
028100   03  W-WDGX-4505-KEY-X.                                                 
028200     05  W-IDHTYP-4505       PIC X(04) VALUE '4505'.                      
028300     05  W-IDDC-4505         PIC X(02) VALUE '11'.                        
028400     05  FILLER              PIC X(24) VALUE LOW-VALUE.                   
028500                                                                          
028600   03  W5-WDA5A1KY-X.                                                     
028700     05  W5-IDARTNR          PIC S9(09) COMP-3.                           
028800     05  W5-IDDC             PIC X(02)  VALUE '11'.                       
028900     05  W5-KDRAPRIO         PIC S9(03) COMP-3.                           
029000     05  FILLER              PIC X(31)  VALUE LOW-VALUE.                  
029100                                                                          
029200   03  W6-WDA5A1KY-X.                                                     
029300     05  W6-IDARTNR          PIC S9(09) COMP-3.                           
029400     05  W6-IDDC             PIC X(02)  VALUE '11'.                       
029500     05  W6-KDRAPRIO         PIC S9(03) COMP-3.                           
029600     05  FILLER              PIC X(31)  VALUE HIGH-VALUE.                 
029700                                                                          
029800   03  W1-WDA5ASEQ-X.                                                     
029900     05  W1-IDARTNR          PIC S9(09) COMP-3 VALUE ZERO.                
030000     05  W1-IDDC             PIC X(02)  VALUE '11'.                       
030100     05  W1-KDRAPRIO         PIC S9(03) COMP-3 VALUE ZERO.                
030200                                                                          
030300   03  W2-WDA5ASEQ-X.                                                     
030400     05  W2-IDARTNR          PIC S9(09) COMP-3 VALUE ZERO.                
030500     05  W2-IDDC             PIC X(02)  VALUE '11'.                       
030600     05  W2-KDRAPRIO         PIC S9(03) COMP-3 VALUE ZERO.                
030700                                                                          
030800   03  W-KDSTARAD-X.                                                      
030900     05  W-KDSTARAD          PIC X        VALUE SPACE.                    
031000                                                                          
031100   03  W-IDDISTR-X.                                                       
031200     05  W-IDDISTR           PIC S9(5)    COMP-3 VALUE ZERO.              
031300                                                                          
031400   03  W-IDARTNR-X.                                                       
031500     05  W-IDARTNR           PIC S9(9)    COMP-3 VALUE ZERO.              
031600                                                                          
031700   03  W3-WDA501KY-X.                                                     
031800     05  W3-IDDISTR          PIC S9(5)    COMP-3 VALUE ZERO.              
031900     05  W3-IDKUNDNR         PIC S9(7)    COMP-3 VALUE ZERO.              
032000     05  W3-IDKUNDRF         PIC X(10)    VALUE SPACE.                    
032100     05  W3-IDARTNR          PIC S9(9)    COMP-3 VALUE ZERO.              
032200     05  W3-IDLOPNR          PIC S9(3)    COMP-3 VALUE ZERO.              
032300                                                                          
032400   03  W4-WDA501KY-X.                                                     
032500     05  W4-IDDISTR          PIC S9(5)    COMP-3 VALUE ZERO.              
032600     05  W4-IDKUNDNR         PIC S9(7)    COMP-3 VALUE ZERO.              
032700     05  W4-IDKUNDRF         PIC X(10)    VALUE SPACE.                    
032800     05  W4-IDARTNR          PIC S9(9)    COMP-3 VALUE ZERO.              
032900     05  W4-IDLOPNR          PIC S9(3)    COMP-3 VALUE ZERO.              
033000                                                                          
033100   03  W-IDGMT-X.                                                         
033200     05 W-IDDISTR-WDB2       PIC S9(5) VALUE ZERO COMP-3.                 
033300     05 W-IDKUNDNR-WDB2      PIC S9(7) VALUE ZERO COMP-3.                 
033400                                                                          
033500     EJECT                                                                
033600*01  -COPY W0003                                                          
033700     EJECT                                                                
033800 01  FILLER                  PIC X(16) VALUE 'RY4-POSTER VR'.             
033900     SKIP2                                                                
034000*01  AREA -COPY W440300     -PRE  RY4-                                    
034100     EJECT                                                                
034200* ---         DLI INOUT OUTPUT AREA                                       
034300* ---         DLI-IO-AREA                                                 
034400 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ZZAC01'.           
034500 01  DLI-IO-ZZAC01.                                                       
034600*  03  WLZZAC01 -COPY WDGZ01  -PRE  LOGG-                                 
034700     EJECT                                                                
034800                                                                          
034900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ARTC01'.           
035000 01  DLI-IO-ARTC01.                                                       
035100*  03  WLARTC01 -COPY WDK601                                              
035200     EJECT                                                                
035300                                                                          
035400 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ARTC11'.           
035500 01  DLI-IO-ARTC11.                                                       
035600*  03  WLARTC11 -COPY WDK611                                              
035700     EJECT                                                                
035800                                                                          
035900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-GX4501'.           
036000 01  DLI-IO-GX4501.                                                       
036100*  03  WDGX01   -COPY WDGX01                                              
036200     EJECT                                                                
036300                                                                          
036400 01  FILLER                    PIC X(16) VALUE 'DLI-IO-GX4502'.           
036500 01  DLI-IO-GX4502.                                                       
036600*  03  WDGX4502 -COPY WDGX4502                                            
036700     EJECT                                                                
036800                                                                          
036900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-GX4506'.           
037000 01  DLI-IO-GX4506.                                                       
037100*  03  WDGX4506 -COPY WDGX4506                                            
037200     EJECT                                                                
037300                                                                          
037400 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ORDQ01'.           
037500 01  DLI-IO-ORDQ01.                                                       
037600*  03  WLORDQ01 -COPY WDA5A1                                              
037700     EJECT                                                                
037800                                                                          
037900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-ORDP01'.           
038000 01  DLI-IO-ORDP01.                                                       
038100*  03  WLORDP01 -COPY WDA501                                              
038200     EJECT                                                                
038300                                                                          
038400 01  FILLER                    PIC X(16) VALUE 'WDA501 RESERV.'.          
038500 01  DLI-IO-ORDP01-RES.                                                   
038600*  03  WLORDP01 -COPY WDA501 -PRE RES                                     
038700     EJECT                                                                
038800                                                                          
038900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDB2  '.           
039000 01  DLI-IO-WDB2.                                                         
039100*  03  WDB2  -COPY WDB201                                                 
039200                                                                          
039300 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK701'.           
039400 01  DLI-IO-WDK701.                                                       
039500*  03  WDK7  -COPY WDK701                                                 
039600     EJECT                                                                
039700                                                                          
039800 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK711'.           
039900 01  DLI-IO-WDK711.                                                       
040000*  03  WDK7 -COPY WDK711                                                  
040100                                                                          
040200                                                                          
040300*    ---  MSG INPUT-OUTPUT AREA                                           
040400*01  -COPY WMSGAREA                                                       
040500     EJECT                                                                
040600 01  FILLER                 PIC X(16)   VALUE 'KOM-OHUV-AREA'.            
040700 01  OHUV-AREA.                                                           
040800*    03   -COPY W4I25101   -PRE OHUV-                                     
040900     EJECT                                                                
041000 01  FILLER                 PIC X(16)   VALUE 'KOM-RAD-AREA'.             
041100 01  ORAD-AREA.                                                           
041200*    05   -COPY W4I25201   -PRE ORAD-                                     
041300     EJECT                                                                
041400*    --- AREOR FÖR W006KOM SUBMODUL                                       
041500 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
041600*01  -COPY WMSGKOM                                                        
041700     EJECT                                                                
041800 LINKAGE SECTION.                                                         
041900     SKIP2                                                                
042000*01  -COPY W0009      -PRE  MSG-                                          
042100     EJECT                                                                
042200 01  0693-PCB                     PIC X.                                  
042300 01  WDP8-PCB                     PIC X.                                  
042400*01  -COPY W0008      -PRE  ARTC-                                         
042500       05  FILLER                PIC X.                                   
042600     EJECT                                                                
042700*01  -COPY W0008      -PRE  ORDQ-                                         
042800       05  FILLER                PIC X.                                   
042900     EJECT                                                                
043000*01  -COPY W0008      -PRE  ORDP-                                         
043100       05  FILLER                PIC X.                                   
043200     EJECT                                                                
043300*01  -COPY W0008      -PRE  ORDP1-                                        
043400       05  FILLER                PIC X.                                   
043500     EJECT                                                                
043600*01  -COPY W0008      -PRE  ORDP2-                                        
043700       05  FILLER                PIC X.                                   
043800     EJECT                                                                
043900*01  -COPY W0008      -PRE  ZZAC-                                         
044000       05  FILLER                PIC X.                                   
044100     EJECT                                                                
044200*01  -COPY W0008      -PRE  XXJM-                                         
044300       05  FILLER                PIC X.                                   
044400     EJECT                                                                
044500*01  -COPY W0008      -PRE  4505-                                         
044600       05  FILLER                PIC X.                                   
044700*01  -COPY W0008      -PRE  WDB2-                                         
044800       05  FILLER                PIC X.                                   
044900*01  -COPY W0008      -PRE  WDK7-                                         
045000       05  FILLER                PIC X.                                   
045100     SKIP3                                                                
045200 01  ARTM-PCB                    PIC X.                                   
045300     SKIP3                                                                
045400 01  ARTS-PCB                    PIC X.                                   
045500     SKIP3                                                                
045600     EJECT                                                                
045700 01  ORDN-XXKP-PCB               PIC X.                                   
045800 01  ORDN-ORQL-PCB               PIC X.                                   
045900 01  ORDN-PROC-PCB               PIC X.                                   
046000 01  ORDN-ORQI-PCB               PIC X.                                   
046100 01  EMOH-WDQ2-PCB               PIC X.                                   
046200     SKIP3                                                                
046300                                                                          
046400                                                                          
046500 PROCEDURE DIVISION  USING MSG-PCB  0693-PCB  WDP8-PCB                    
046600                           ARTC-PCB ORDQ-PCB                              
046700                           ORDP-PCB ORDP1-PCB ORDP2-PCB ZZAC-PCB          
046800                           XXJM-PCB 4505-PCB  WDB2-PCB  WDK7-PCB          
046900                           ARTM-PCB ARTS-PCB                              
047000                           ORDN-XXKP-PCB ORDN-ORQL-PCB                    
047100                           ORDN-PROC-PCB ORDN-ORQI-PCB                    
047200                           EMOH-WDQ2-PCB.                                 
047300 STYRDEL SECTION.                                                         
047400                                                                          
047500     ENTRY 'DLITCBL' USING MSG-PCB  0693-PCB  WDP8-PCB                    
047600                           ARTC-PCB ORDQ-PCB                              
047700                           ORDP-PCB ORDP1-PCB ORDP2-PCB ZZAC-PCB          
047800                           XXJM-PCB 4505-PCB  WDB2-PCB  WDK7-PCB          
047900                           ARTM-PCB ARTS-PCB                              
048000                           ORDN-XXKP-PCB ORDN-ORQL-PCB                    
048100                           ORDN-PROC-PCB ORDN-ORQI-PCB                    
048200                           EMOH-WDQ2-PCB.                                 
048300                                                                          
048400     PERFORM A-INIT                                                       
048500                                                                          
048600     PERFORM B-LAES-STYRREG-TILL-TABELL                                   
048700                                                                          
048800     PERFORM IMS-GET-450501-TAECKTRANS                                    
048900     PERFORM IMS-GET-450511-TAECKTRANS                                    
049000     PERFORM UNTIL SEGMENT-SAKNAS                                         
049100                                                                          
049200       MOVE 4506-IDARTNR TO W-IDARTNR                                     
049300       PERFORM IMS-GET-ARTC01                                             
049400       IF SEGMENT-FINNS                                                   
049500           PERFORM IMS-GHU-ARTC11                                         
049600           PERFORM C-DISPONIBELT-SALDO                                    
049700           PERFORM I-STYR-TACKNING                                        
049800           PERFORM H-UPPD-SALDO-WDK6                                      
049900                                                                          
050000           IF BMP AND CHKP-ANT   >  CHKP-MAX                              
050100              PERFORM J-TAG-CHECKPOINT                                    
050200           ELSE                                                           
050300             PERFORM IMS-DELETE-450511-TAECKTRANS                         
050400           END-IF                                                         
050500       END-IF                                                             
050600         PERFORM IMS-GET-450511-TAECKTRANS                                
050700     END-PERFORM                                                          
050800                                                                          
050900     MOVE ZERO TO RETURN-CODE                                             
051000     GOBACK                                                               
051100     .                                                                    
051200     EJECT                                                                
051300 A-INIT SECTION.                                                          
051400     SKIP2                                                                
051500     CALL VIMSREGT                                                        
051600     MOVE RETURN-CODE  TO IMS-VIMSREGT                                    
051700                                                                          
051800     IF BMP                                                               
051900        PERFORM IMS-RESTART                                               
052000     END-IF                                                               
052100     MOVE ZERO         TO CHKP-ANT                                        
052200                                                                          
052300     MOVE 'IDAG  '     TO DAT-KDDATFORM                                   
052400     CALL WDATKONV USING  DAT-KDDATFORM                                   
052500                          DAT-I-TIDATUM                                   
052600                          DAT-O-TIDATUM                                   
052700                          DAT-KDSVAR                                      
052800     MOVE DAT-TIAAMMDD TO W-TIAAMMDD                                      
052900                          WS-DAT                                          
053000     MOVE DAT-TIDDD    TO W-TIDDD                                         
053100     MOVE DAT-TIAA     TO W-TIAA                                          
053200                                                                          
053300     MOVE WC-CDC-SE    TO W-IDDC-4505                                     
053400                          W5-IDDC                                         
053500                          W6-IDDC                                         
053600                          W1-IDDC                                         
053700                          W2-IDDC                                         
053800     .                                                                    
053900     EJECT                                                                
054000 B-LAES-STYRREG-TILL-TABELL SECTION.                                      
054100     SKIP2                                                                
054200     PERFORM IMS-GET-XXJM01                                               
054300                                                                          
054400     PERFORM IMS-GNP-XXJM11                                               
054500     PERFORM UNTIL SEGMENT-SAKNAS OR IX1-PRIO = 25                        
054600       ADD +1                  TO IX1-PRIO                                
054700       MOVE 4502-KDRAPRIO      TO TAB-KDRAPRIO      (IX1-PRIO)            
054800       MOVE 4502-FLPRIO        TO TAB-FLPRIO        (IX1-PRIO)            
054900       MOVE 4502-KVVECKOR-TECK TO TAB-KVVECKOR-TECK (IX1-PRIO)            
055000       MOVE 4502-REROFORD      TO TAB-REROFORD      (IX1-PRIO)            
055100                                                                          
055200                                                                          
055300       PERFORM IMS-GNP-XXJM11                                             
055400     END-PERFORM                                                          
055500     IF SEGMENT-FINNS                                                     
055600        DISPLAY '***KATEGORI-TABELL FÖR LITEN***'                         
055700        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
055800     END-IF                                                               
055900     .                                                                    
056000     EJECT                                                                
056100 C-DISPONIBELT-SALDO SECTION.                                             
056200                                                                          
056300* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
056400*                                                                         
056500*    DISPONIBELT RÄKNAS UT.                                               
056600*                                                                         
056700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
056800                                                                          
056900     MOVE NEJ  TO SW-TACKBART                                             
057000     MOVE +0   TO W-DISPONIBELT                                           
057100                  W-KVLS                                                  
057200                  W-KVAKS-CDC  W-KVAKS-PAV  W-KVAKS-T                     
057300                  W-KVRESS     W-KVROS                                    
057400                  W-KVSPANT                                               
057500                  W-KVUTRS                                                
057600                  W-SUM-KVROS  W-SUM-KVRESS                               
057700                                                                          
057800     MOVE +0   TO TACK-KVDISP                                             
057900                  RROTW-TIDISPIN                                          
058000                                                                          
058100     IF CLAG-PRARTSTD NOT = +0 AND                                        
058200        CLAG-KDLEVSP  NOT = +20                                           
058300         IF CLAG-KDLEVSP NOT = +21                                        
058400           MOVE CLAG-KDERS TO W-KDERS                                     
058500           MOVE CLAG-KVLS  TO W-KVLS                                      
058600           MOVE CLAG-KVAKS-CDC TO W-KVAKS-CDC                             
058700           MOVE CLAG-KVAKS-PAV TO W-KVAKS-PAV                             
058800           MOVE CLAG-KVAKS-T TO W-KVAKS-T                                 
058900           MOVE CLAG-KVRESS TO W-KVRESS                                   
059000           IF CLAG-KVRESS < ZERO                                          
059100             MOVE ZERO     TO W-KVRESS                                    
059200           END-IF                                                         
059300           MOVE CLAG-KVROS TO W-KVROS                                     
059400           MOVE CLAG-KVSPANT TO W-KVSPANT                                 
059500           MOVE CLAG-KVUTRS TO W-KVUTRS                                   
059600           MOVE CLAG-TIDISPIN TO RROTW-TIDISPIN                           
059700           MOVE JA         TO SW-TACKBART                                 
059800         END-IF                                                           
059900         IF SW-TACKBART = JA                                              
060000           COMPUTE TACK-KVDISP =    W-KVLS                                
060100                                  - W-KVUTRS                              
060200                                  - W-KVRESS                              
060300                                  - W-KVSPANT                             
060400         END-IF                                                           
060500     END-IF                                                               
060600     .                                                                    
060700     EJECT                                                                
060800 D-METODBESTAMNING SECTION.                                               
060900                                                                          
061000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
061100*    BERÄKNA TÄCKBART OCH NORMALNIVÅ PER KATEGORI OCH         *           
061200*    BESTÄMMER METOD.                                         *           
061300* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
061400                                                                          
061500     MOVE +0 TO W-SUM-KATEGORINORMAL                                      
061600                W-SUM-KATEGORINORMAL-PRIO                                 
061700                W-TACKNINGSGRAD                                           
061800                                                                          
061900     COMPUTE W-DISPONIBELT-TOT = W-DISPONIBELT                            
062000* * * * * * * * * * * * * * *  + W-KVAKS-CDC                              
062100                                                                          
062200     MOVE LOW-VALUE            TO W5-WDA5A1KY-X                           
062300     MOVE HIGH-VALUE           TO W6-WDA5A1KY-X                           
062400     MOVE 4506-IDARTNR         TO W5-IDARTNR W6-IDARTNR                   
062500     MOVE WC-CDC-SE            TO W5-IDDC    W6-IDDC                      
062600     MOVE '2'                  TO W-KDSTARAD                              
062700     MOVE +1 TO IX2                                                       
062800     PERFORM UNTIL IX2 > IX1-PRIO                                         
062900       MOVE    TAB-KDRAPRIO (IX2) TO W5-KDRAPRIO                          
063000                                     W6-KDRAPRIO                          
063100       MOVE    ZERO               TO TAB-KVART(IX2)                       
063200                                                                          
063300       PERFORM DA-ACK-KVART                                               
063400                                                                          
063500       COMPUTE TAB-KATEGORINORMAL(IX2) =                                  
063600              (TAB-KVART (IX2) * TAB-REROFORD (IX2)) / 100                
063700       ADD TAB-KATEGORINORMAL (IX2) TO W-SUM-KATEGORINORMAL               
063800                                                                          
063900       IF TAB-FLPRIO (IX2) = JA                                           
064000          ADD TAB-KATEGORINORMAL (IX2) TO                                 
064100              W-SUM-KATEGORINORMAL-PRIO                                   
064200       END-IF                                                             
064300                                                                          
064400       ADD +1 TO IX2                                                      
064500     END-PERFORM                                                          
064600                                                                          
064700     IF W-SUM-KATEGORINORMAL > +0                                         
064800       IF W-DISPONIBELT-TOT NOT < W-RO-ATT-TACKA                          
064900          MOVE '1'  TO W-METOD                                            
065000          COMPUTE W-TACKNINGSGRAD =                                       
065100                  W-DISPONIBELT / W-SUM-KATEGORINORMAL                    
065200          END-COMPUTE                                                     
065300       ELSE                                                               
065400         IF W-DISPONIBELT-TOT / W-SUM-KATEGORINORMAL < +1                 
065500           MOVE '2' TO W-METOD                                            
065600           COMPUTE W-TACKNINGSGRAD =                                      
065700                   W-DISPONIBELT / W-SUM-KATEGORINORMAL-PRIO              
065800             ON SIZE ERROR                                                
065900               MOVE +0 TO W-TACKNINGSGRAD                                 
066000           END-COMPUTE                                                    
066100         ELSE                                                             
066200           MOVE '1' TO W-METOD                                            
066300           COMPUTE W-TACKNINGSGRAD =                                      
066400                   W-DISPONIBELT / W-SUM-KATEGORINORMAL                   
066500           END-COMPUTE                                                    
066600         END-IF                                                           
066700       END-IF                                                             
066800     END-IF                                                               
066900     .                                                                    
067000     EJECT                                                                
067100 DA-ACK-KVART SECTION.                                                    
067200*****************************************************************         
067300*                                                                         
067400*    TÄCKBART ACKUMULERAS                                                 
067500*                                                                         
067600*****************************************************************         
067700                                                                          
067800     PERFORM IMS-GET-ORDQ01                                               
067900     PERFORM UNTIL SEGMENT-SAKNAS                                         
068000       ADD     SEQA-KVART TO TAB-KVART (IX2)                              
068100       PERFORM IMS-GET-ORDQ01                                             
068200     END-PERFORM                                                          
068300     .                                                                    
068400     EJECT                                                                
068500 E-KATEGORIFORDELNING SECTION.                                            
068600     SKIP2                                                                
068700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
068800*    BERÄKNA DISPONIBELT/KATEGORI.                            *           
068900*                                                             *           
069000*    METOD-1                                                  *           
069100*       VID FÖRDELNING TAS INGEN HÄNSYN TILL PRIORITERADE     *           
069200*       KATEGORIER.                                           *           
069300*    METOD-2                                                  *           
069400*       VID FÖRDELNING GER MAN FÖRST TILL DE PRIORITERADE     *           
069500*       KATEGORIERNA.                                         *           
069600*                                                             *           
069700*    OM DISPONIBELT ÄR STÖRRE ÄN TÄCKBART FÖR EN KATEGORI     *           
069800*    GER MAN MELLANSKILLNADEN TILL NÄSTA KATEGORI.            *           
069900* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
070000                                                                          
070100     MOVE +0 TO W-REST                                                    
070200     MOVE +1 TO IX2                                                       
070300     PERFORM UNTIL IX2 > IX1-PRIO                                         
070400      IF  W-METOD-1                                                       
070500      OR (W-METOD-2 AND TAB-FLPRIO (IX2) = JA)                            
070600         IF  W-DISPONIBELT            > +0                                
070700         AND TAB-KATEGORINORMAL (IX2) > +0                                
070800            COMPUTE TAB-KATEGORI (IX2) ROUNDED = W-REST  +                
070900           (TAB-KATEGORINORMAL (IX2) * W-TACKNINGSGRAD) + 0.4999          
071000            MOVE +0                  TO W-REST                            
071100                                                                          
071200            IF TAB-KATEGORI (IX2) > TAB-KVART (IX2)                       
071300               COMPUTE W-REST =                                           
071400                       TAB-KATEGORI (IX2) - TAB-KVART (IX2)               
071500               MOVE TAB-KVART (IX2) TO TAB-KATEGORI (IX2)                 
071600            END-IF                                                        
071700                                                                          
071800            SUBTRACT TAB-KATEGORI (IX2) FROM W-DISPONIBELT                
071900            IF W-DISPONIBELT < +0                                         
072000               ADD W-DISPONIBELT     TO TAB-KATEGORI (IX2)                
072100            END-IF                                                        
072200         ELSE                                                             
072300            MOVE ZERO                TO TAB-KATEGORI (IX2)                
072400         END-IF                                                           
072500      END-IF                                                              
072600      ADD +1 TO IX2                                                       
072700     END-PERFORM                                                          
072800     EJECT                                                                
072900     IF  W-METOD-2                                                        
073000       MOVE +1 TO IX2                                                     
073100       PERFORM UNTIL IX2 > IX1-PRIO                                       
073200         IF TAB-FLPRIO (IX2) = NEJ                                        
073300            IF W-DISPONIBELT > +0                                         
073400              MOVE TAB-KVART (IX2) TO TAB-KATEGORI (IX2)                  
073500                                                                          
073600              SUBTRACT TAB-KATEGORI (IX2) FROM W-DISPONIBELT              
073700              IF W-DISPONIBELT < +0                                       
073800                 ADD W-DISPONIBELT  TO TAB-KATEGORI (IX2)                 
073900              END-IF                                                      
074000            ELSE                                                          
074100              MOVE +0               TO TAB-KATEGORI (IX2)                 
074200            END-IF                                                        
074300         END-IF                                                           
074400         ADD +1 TO IX2                                                    
074500       END-PERFORM                                                        
074600     END-IF                                                               
074700     .                                                                    
074800     EJECT                                                                
074900 F-EJ-FULL-TAECKNING SECTION.                                             
075000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
075100*    LÄSER ALLA RO-RADER PER KATEGORI, OTÄCKTA OCH I TIDSORDNING*         
075200*    BEHANDLA DE RADER SOM KAN TÄCKAS FRÅN HTR'S CLAGER.        *         
075300*    OM ORSAKSKOD ÄR LIKA MED INLÄGGNINGSFÖRDELNING SKALL ENDAST*         
075400*    HTR'S CLAGER RADER BEHANDLAS.                              *         
075500*                                                               *         
075600*    TIDSGRÄNS FINNS FÖR GAMMAL RO-RAD. GER TILL DE ÄLDSTA FÖRST*         
075700*                                                               *         
075800*    KONTROLLERA OM MAN KAN SLÅ IHOP TÄCKT RAD MED TIDIGARE     *         
075900*    TÄCKT OCH EJ BIPACKAD RAD.                                 *         
076000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
076100                                                                          
076200     MOVE 4506-IDARTNR      TO W1-IDARTNR                                 
076300     MOVE WC-CDC-SE         TO W1-IDDC                                    
076400     MOVE '2'               TO W-KDSTARAD                                 
076500     MOVE +1 TO IX2                                                       
076600     PERFORM UNTIL IX2 > IX1-PRIO                                         
076700       MOVE JA              TO W-BERAKNA-TG                               
076800       MOVE NEJ             TO AVBRYT-KATEGORI-W                          
076900       MOVE TAB-KVART (IX2) TO W-KVAR-ATT-TACKA                           
077000                                                                          
077100       MOVE TAB-KDRAPRIO (IX2) TO W1-KDRAPRIO                             
077200       PERFORM IMS-GHN-ORDQ01-ORDP01-01                                   
077300                                                                          
077400       PERFORM UNTIL SEGMENT-SAKNAS    OR                                 
077500                     W-AVBRYT-KATEGORI OR                                 
077600                     NOT (TAB-KATEGORI (IX2) > +0) OR                     
077700                     CHKP-ANT > CHKP-MAX                                  
077800                                                                          
077900         MOVE    NEJ TO W-SAMMANSLAGNING-RAD                              
078000*FOR WORKSHOP ORDER IF THE ORDERCLASS IS 2 OR 1 THEN WE RELEASE           
078100*THEM AND IF IT IS CLASS 3 IF TODAY DATE IS GREATER THAN                  
078200*REPDAT THEN RELEASE IT                                                   
078300         IF RAD-TIREPDAT NOT = ZERO                                       
078400             IF WS-DAT     > RAD-TIREPDAT OR                              
078500              ( RAD-KDORDKL =  1  OR  2 )                                 
078600                MOVE 'Y'   TO WS-RELEASE-WIP                              
078700             END-IF                                                       
078800         END-IF                                                           
078900         IF RAD-TIREPDAT = ZERO   OR                                      
079000            WS-RELEASE-WIP = 'Y'                                          
079100           PERFORM S03-SAMMANSLAGNING-RAD                                 
079200           PERFORM FA-BERAKNA-ANTAL-DAGAR                                 
079300           COMPUTE W-ANTAL-DAGAR-TACK                                     
079400                                 = TAB-KVVECKOR-TECK (IX2) * 7            
079500           IF W-ANTAL-DAGAR-TACK > W-ANTAL-RO-DAGAR                       
079600             PERFORM FB-NY-RAD                                            
079700           ELSE                                                           
079800             PERFORM FC-GAMMAL-RAD                                        
079900           END-IF                                                         
080000         END-IF                                                           
080100         MOVE    '2' TO W-KDSTARAD                                        
080200         PERFORM IMS-GHN-ORDQ01-ORDP01-01                                 
080300       END-PERFORM                                                        
080400                                                                          
080500       IF CHKP-ANT > CHKP-MAX                                             
080600         MOVE NEJ               TO SW-RO-PA-ARTIKEL-SLUT                  
080700       END-IF                                                             
080800                                                                          
080900       IF TAB-KATEGORI (IX2) > +0 AND                                     
081000          IX2                < IX1-PRIO                                   
081100         ADD +1 IX2             GIVING IX3                                
081200         ADD TAB-KATEGORI (IX2) TO     TAB-KATEGORI (IX3)                 
081300       END-IF                                                             
081400                                                                          
081500       ADD +1 TO IX2                                                      
081600     END-PERFORM                                                          
081700     .                                                                    
081800     EJECT                                                                
081900 FA-BERAKNA-ANTAL-DAGAR SECTION.                                          
082000     SKIP2                                                                
082100* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
082200*    BERÄKNA ANTAL DAGAR FRÅN RESTORDERDATUM.                   *         
082300* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
082400                                                                          
082500     MOVE RAD-DARODAT (3:6) TO DAT-I-TIDATUM                              
082600     MOVE 'AAMMDD'    TO DAT-KDDATFORM                                    
082700     CALL WDATKONV USING DAT-KDDATFORM                                    
082800                         DAT-I-TIDATUM                                    
082900                         DAT-O-TIDATUM                                    
083000                         DAT-KDSVAR                                       
083100     IF DAT-TIAA = W-TIAA                                                 
083200        COMPUTE W-ANTAL-RO-DAGAR = W-TIDDD - DAT-TIDDD                    
083300     ELSE                                                                 
083400        MOVE W-TIAA   TO TMP1-YY                                          
083500        MOVE DAT-TIAA TO TMP2-YY                                          
083600        PERFORM WY2000P9                                                  
083700        COMPUTE W-ANTAL-RO-DAGAR = 365 - DAT-TIDDD +                      
083800                               ((TMP1-YY - TMP2-YY - 1) * 365) +          
083900                                 W-TIDDD                                  
084000     END-IF                                                               
084100     .                                                                    
084200     EJECT                                                                
084300 FB-NY-RAD SECTION.                                                       
084400                                                                          
084500* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
084600*    BERÄKNA MIN-GRÄNSEN OCH TÄCKBART FÖR EN RO-RAD. TÄCKBART   *         
084700*    FÅR ALDRIG UNDERSTIGA MIN-GRÄNSEN UTOM FÖR SISTA RO-RADEN. *         
084800*    KONTROLLERA HEL FÖRPACKNING.                                         
084900*    ÄNDRAT FÖR DIVISION MED NOLL 970521 BO S.                            
085000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
085100                                                                          
085200     IF W-BERAKNA-TG = JA                                                 
085300        MOVE NEJ  TO W-BERAKNA-TG                                         
085400        IF  W-KVAR-ATT-TACKA = ZERO                                       
085500           MOVE +1 TO W-TACKNINGSGRAD                                     
085600        ELSE                                                              
085700          COMPUTE W-TACKNINGSGRAD =                                       
085800                  TAB-KATEGORI (IX2) / W-KVAR-ATT-TACKA                   
085900          IF W-TACKNINGSGRAD > +1                                         
086000             MOVE +1 TO W-TACKNINGSGRAD                                   
086100          END-IF                                                          
086200        END-IF                                                            
086300     END-IF                                                               
086400                                                                          
086500     COMPUTE W-MIN-GRANS-RAD ROUNDED =                                    
086600             (RAD-KVART * 0.25) + 0.4999                                  
086700                                                                          
086800     COMPUTE W-KVTACKT ROUNDED =                                          
086900             (RAD-KVART * W-TACKNINGSGRAD) + 0.4999                       
087000                                                                          
087100     IF W-KVTACKT < W-MIN-GRANS-RAD                                       
087200        MOVE W-MIN-GRANS-RAD TO W-KVTACKT                                 
087300     END-IF                                                               
087400                                                                          
087500     IF CLAG-KVQPACK-1 > +1                                               
087600       IF ART-KDSORT   = 'KG' OR 'M ' OR 'L ' OR                          
087700          RAD-KDKVBRYT = +0                                               
087800          IF W-KVTACKT < RAD-KVART                                        
087900             COMPUTE W-ANTAL-ARTIKLAR ROUNDED =                           
088000                    (W-KVTACKT / CLAG-KVQPACK-1) + 0.4999                 
088100             COMPUTE W-KVTACKT =                                          
088200                     W-ANTAL-ARTIKLAR * CLAG-KVQPACK-1                    
088300                                                                          
088400             IF W-KVTACKT > TAB-KATEGORI (IX2)                            
088500                MOVE TAB-KATEGORI (IX2) TO W-KVTACKT                      
088600                COMPUTE W-ANTAL-ARTIKLAR ROUNDED =                        
088700                       (W-KVTACKT / CLAG-KVQPACK-1) - 0.5                 
088800                COMPUTE W-KVTACKT =                                       
088900                        W-ANTAL-ARTIKLAR * CLAG-KVQPACK-1                 
089000             END-IF                                                       
089100          END-IF                                                          
089200       END-IF                                                             
089300     END-IF                                                               
089400                                                                          
089500     IF W-KVTACKT NOT > +0                                                
089600        CONTINUE                                                          
089700     ELSE                                                                 
089800        IF TAB-KATEGORI (IX2) > +0                                        
089900           IF W-SAMMANSLAGNING-RAD = JA                                   
090000              PERFORM FBA-SAMMANSLAGNING-NY-RAD                           
090100           ELSE                                                           
090200              PERFORM FBB-EJ-SAMMANSLAGNING-NY-RAD                        
090300           END-IF                                                         
090400        ELSE                                                              
090500           MOVE JA TO AVBRYT-KATEGORI-W                                   
090600        END-IF                                                            
090700     END-IF                                                               
090800     .                                                                    
090900     EJECT                                                                
091000 FBA-SAMMANSLAGNING-NY-RAD SECTION.                                       
091100                                                                          
091200     IF W-KVTACKT > TAB-KATEGORI (IX2)                                    
091300        MOVE W-TIAAMMDD TO RESRAD-TIRES                                   
091400        COMPUTE RESRAD-KVART = RESRAD-KVART + TAB-KATEGORI (IX2)          
091500        PERFORM IMS-REPLACE-ORDP01-ORDP2                                  
091600                                                                          
091700        MOVE TAB-KATEGORI (IX2) TO W-KVART                                
091800        PERFORM S01-SUM-SALDO-WDK6                                        
091900        PERFORM S02-SKAPA-TRANS                                           
092000                                                                          
092100        SUBTRACT TAB-KATEGORI (IX2) FROM RAD-KVART                        
092200        PERFORM IMS-REPLACE-ORDQ01-ORDP01                                 
092300                                                                          
092400        MOVE +0   TO TAB-KATEGORI (IX2)                                   
092500     ELSE                                                                 
092600       IF W-KVTACKT < RAD-KVART                                           
092700          MOVE W-TIAAMMDD TO RESRAD-TIRES                                 
092800          COMPUTE RESRAD-KVART = RESRAD-KVART +                           
092900                                 W-KVTACKT                                
093000          PERFORM IMS-REPLACE-ORDP01-ORDP2                                
093100                                                                          
093200          MOVE W-KVTACKT       TO W-KVART                                 
093300          PERFORM S01-SUM-SALDO-WDK6                                      
093400          PERFORM S02-SKAPA-TRANS                                         
093500                                                                          
093600          SUBTRACT W-KVTACKT FROM RAD-KVART                               
093700          PERFORM IMS-REPLACE-ORDQ01-ORDP01                               
093800                                                                          
093900          SUBTRACT W-KVTACKT FROM TAB-KATEGORI (IX2)                      
094000       ELSE                                                               
094100          MOVE W-TIAAMMDD TO RESRAD-TIRES                                 
094200          COMPUTE RESRAD-KVART = RESRAD-KVART +                           
094300                                 RAD-KVART                                
094400          PERFORM IMS-REPLACE-ORDP01-ORDP2                                
094500                                                                          
094600          MOVE RAD-KVART       TO W-KVART                                 
094700          PERFORM S01-SUM-SALDO-WDK6                                      
094800          PERFORM S02-SKAPA-TRANS                                         
094900                                                                          
095000          SUBTRACT RAD-KVART FROM TAB-KATEGORI (IX2)                      
095100                                                                          
095200          MOVE RAD-IDDISTR   TO W3-IDDISTR                                
095300          MOVE RAD-IDKUNDNR  TO W3-IDKUNDNR                               
095400          MOVE RAD-IDKUNDRF  TO W3-IDKUNDRF                               
095500          MOVE RAD-IDARTNR   TO W3-IDARTNR                                
095600          MOVE RAD-IDLOPNR   TO W3-IDLOPNR                                
095700          PERFORM IMS-GHU-ORDP01-ORDP1                                    
095800                                                                          
095900          PERFORM IMS-DELETE-ORDP01                                       
096000       END-IF                                                             
096100     END-IF                                                               
096200     .                                                                    
096300     EJECT                                                                
096400 FBB-EJ-SAMMANSLAGNING-NY-RAD SECTION.                                    
096500                                                                          
096600     IF W-KVTACKT > TAB-KATEGORI (IX2)                                    
096700        SUBTRACT TAB-KATEGORI (IX2) FROM RAD-KVART                        
096800        PERFORM IMS-REPLACE-ORDQ01-ORDP01                                 
096900                                                                          
097000        MOVE TAB-KATEGORI (IX2)  TO W-KVART                               
097100        PERFORM S01-SUM-SALDO-WDK6                                        
097200        PERFORM S02-SKAPA-TRANS                                           
097300                                                                          
097400        MOVE '3'             TO RAD-KDSTARAD                              
097500        MOVE TAB-KATEGORI (IX2)                                           
097600                             TO RAD-KVART                                 
097700        MOVE W-TIAAMMDD      TO RAD-TIRES                                 
097800*CALL FOR W440EMOH                                                        
097900      IF RAD-KDROPACK  = '3'                                              
098000         MOVE RAD-IDDISTR     TO EMOH-IDDISTR                             
098100         MOVE RAD-IDKUNDNR    TO EMOH-IDKUNDNR                            
098200         MOVE RAD-IDORDNR5    TO EMOH-IDORDNR7                            
098300         MOVE RAD-KDORDKL     TO EMOH-KDORDKL                             
098400         MOVE RAD-IDDC        TO EMOH-IDDC                                
098500         CALL W440EMOH   USING   EMOH-W440EMOH                            
098600                                 EMOH-WDQ2-PCB                            
098700                                                                          
098800         IF EMOH-KDSVAR-CREATE                                            
098900           PERFORM S11-SKAPA-ORDERHUVUD                                   
099000           MOVE TAB-IDORDNR(ORD-IX)                                       
099100                              TO RAD-IDARBREF                             
099200         END-IF                                                           
099300         IF EMOH-KDSVAR-EXISTS                                            
099400           MOVE EMOH-IXHALV                                               
099500                              TO ORD-IX                                   
099600           MOVE TAB-IDORDNR(ORD-IX)                                       
099700                              TO RAD-IDARBREF                             
099800         END-IF                                                           
099900      END-IF                                                              
100000**SKAPA 'DÖSKALLAR' OM DET ÄR EN VERKSTADSORDER                           
100100**IF THE SWITCH IS SET THEN DOSKALLE THE ORDER                            
100200      IF WS-RELEASE-WIP = 'Y'                                             
100300          MOVE 'N'             TO WS-RELEASE-WIP                          
100400         PERFORM S11-SKAPA-ORDERHUVUD                                     
100500      END-IF                                                              
100600      PERFORM S10-INSERT-ORDP01                                           
100700                                                                          
100800      MOVE +0                  TO TAB-KATEGORI (IX2)                      
100900     ELSE                                                                 
101000       IF W-KVTACKT < RAD-KVART                                           
101100          SUBTRACT W-KVTACKT FROM    RAD-KVART                            
101200          PERFORM IMS-REPLACE-ORDQ01-ORDP01                               
101300                                                                          
101400          MOVE W-KVTACKT           TO W-KVART                             
101500          PERFORM S01-SUM-SALDO-WDK6                                      
101600          PERFORM S02-SKAPA-TRANS                                         
101700          MOVE '3'             TO RAD-KDSTARAD                            
101800          MOVE W-KVTACKT       TO RAD-KVART                               
101900          MOVE W-TIAAMMDD      TO RAD-TIRES                               
102000*CALL FOR W440EMOH                                                        
102100          IF RAD-KDROPACK  = '3'                                          
102200             MOVE RAD-IDDISTR     TO EMOH-IDDISTR                         
102300             MOVE RAD-IDKUNDNR    TO EMOH-IDKUNDNR                        
102400             MOVE RAD-IDORDNR5    TO EMOH-IDORDNR7                        
102500             MOVE RAD-KDORDKL     TO EMOH-KDORDKL                         
102600             MOVE RAD-IDDC        TO EMOH-IDDC                            
102700             CALL W440EMOH   USING   EMOH-W440EMOH                        
102800                                     EMOH-WDQ2-PCB                        
102900                                                                          
103000                                                                          
103100             IF EMOH-KDSVAR-CREATE                                        
103200               PERFORM S11-SKAPA-ORDERHUVUD                               
103300               MOVE TAB-IDORDNR(ORD-IX)                                   
103400                                  TO RAD-IDARBREF                         
103500             END-IF                                                       
103600             IF EMOH-KDSVAR-EXISTS                                        
103700               MOVE EMOH-IXHALV                                           
103800                                  TO ORD-IX                               
103900               MOVE TAB-IDORDNR(ORD-IX)                                   
104000                                  TO RAD-IDARBREF                         
104100             END-IF                                                       
104200          END-IF                                                          
104300**SKAPA 'DÖSKALLAR' OM DET ÄR EN VERKSTADSORDER                           
104400          IF WS-RELEASE-WIP = 'Y'                                         
104500              MOVE 'N'             TO WS-RELEASE-WIP                      
104600              PERFORM S11-SKAPA-ORDERHUVUD                                
104700          END-IF                                                          
104800          PERFORM S10-INSERT-ORDP01                                       
104900                                                                          
105000                                                                          
105100          SUBTRACT W-KVTACKT FROM TAB-KATEGORI (IX2)                      
105200       ELSE                                                               
105300          MOVE RAD-KVART           TO W-KVART                             
105400          PERFORM S01-SUM-SALDO-WDK6                                      
105500          PERFORM S02-SKAPA-TRANS                                         
105600          MOVE '3'             TO RAD-KDSTARAD                            
105700          MOVE W-TIAAMMDD      TO RAD-TIRES                               
105800*CALL FOR W440EMOH                                                        
105900          IF RAD-KDROPACK  = '3'                                          
106000            MOVE RAD-IDDISTR     TO EMOH-IDDISTR                          
106100            MOVE RAD-IDKUNDNR    TO EMOH-IDKUNDNR                         
106200            MOVE RAD-IDORDNR5    TO EMOH-IDORDNR7                         
106300            MOVE RAD-KDORDKL     TO EMOH-KDORDKL                          
106400            MOVE RAD-IDDC        TO EMOH-IDDC                             
106500            CALL W440EMOH   USING   EMOH-W440EMOH                         
106600                                    EMOH-WDQ2-PCB                         
106700                                                                          
106800            IF EMOH-KDSVAR-CREATE                                         
106900              PERFORM S11-SKAPA-ORDERHUVUD                                
107000              MOVE TAB-IDORDNR(ORD-IX)                                    
107100                                 TO RAD-IDARBREF                          
107200            END-IF                                                        
107300            IF EMOH-KDSVAR-EXISTS                                         
107400              MOVE EMOH-IXHALV                                            
107500                                 TO ORD-IX                                
107600              MOVE TAB-IDORDNR(ORD-IX)                                    
107700                                 TO RAD-IDARBREF                          
107800            END-IF                                                        
107900          END-IF                                                          
108000**SKAPA 'DÖSKALLAR' OM DET ÄR EN VERKSTADSORDER                           
108100          IF WS-RELEASE-WIP = 'Y'                                         
108200              MOVE 'N'             TO WS-RELEASE-WIP                      
108300              PERFORM S11-SKAPA-ORDERHUVUD                                
108400          END-IF                                                          
108500          PERFORM IMS-REPLACE-ORDQ01-ORDP01                               
108600                                                                          
108700          SUBTRACT RAD-KVART FROM TAB-KATEGORI (IX2)                      
108800       END-IF                                                             
108900     END-IF                                                               
109000     .                                                                    
109100     EJECT                                                                
109200 FC-GAMMAL-RAD SECTION.                                                   
109300                                                                          
109400     MOVE +0 TO W-KATEGORI-REST                                           
109500     IF CLAG-KVQPACK-1 > +1                                               
109600        IF ART-KDSORT   = 'KG' OR 'M ' OR 'L ' OR                         
109700           RAD-KDKVBRYT = +0                                              
109800           IF TAB-KATEGORI (IX2) < RAD-KVART                              
109900              MOVE TAB-KATEGORI (IX2) TO W-KATEGORI-REST                  
110000              COMPUTE W-ANTAL-ARTIKLAR ROUNDED =                          
110100                   (TAB-KATEGORI (IX2) / CLAG-KVQPACK-1) - 0.5            
110200              COMPUTE TAB-KATEGORI (IX2) =                                
110300                      W-ANTAL-ARTIKLAR * CLAG-KVQPACK-1                   
110400              SUBTRACT TAB-KATEGORI (IX2) FROM W-KATEGORI-REST            
110500           END-IF                                                         
110600        END-IF                                                            
110700     END-IF                                                               
110800                                                                          
110900     IF TAB-KATEGORI (IX2) NOT > +0                                       
111000        MOVE W-KATEGORI-REST TO TAB-KATEGORI (IX2)                        
111100     ELSE                                                                 
111200        IF W-SAMMANSLAGNING-RAD = JA                                      
111300           IF TAB-KATEGORI (IX2) NOT < RAD-KVART                          
111400              MOVE W-TIAAMMDD TO RESRAD-TIRES                             
111500              COMPUTE RESRAD-KVART = RESRAD-KVART + RAD-KVART             
111600              PERFORM IMS-REPLACE-ORDP01-ORDP2                            
111700                                                                          
111800              MOVE RAD-KVART       TO W-KVART                             
111900              PERFORM S01-SUM-SALDO-WDK6                                  
112000              PERFORM S02-SKAPA-TRANS                                     
112100                                                                          
112200              SUBTRACT RAD-KVART FROM W-KVAR-ATT-TACKA                    
112300              SUBTRACT RAD-KVART FROM TAB-KATEGORI (IX2)                  
112400                                                                          
112500              MOVE RAD-IDDISTR   TO W3-IDDISTR                            
112600              MOVE RAD-IDKUNDNR  TO W3-IDKUNDNR                           
112700              MOVE RAD-IDKUNDRF  TO W3-IDKUNDRF                           
112800              MOVE RAD-IDARTNR   TO W3-IDARTNR                            
112900              MOVE RAD-IDLOPNR   TO W3-IDLOPNR                            
113000              PERFORM IMS-GHU-ORDP01-ORDP1                                
113100                                                                          
113200              PERFORM IMS-DELETE-ORDP01                                   
113300           ELSE                                                           
113400              MOVE W-TIAAMMDD TO RESRAD-TIRES                             
113500              COMPUTE RESRAD-KVART = RESRAD-KVART +                       
113600                                     TAB-KATEGORI (IX2)                   
113700              PERFORM IMS-REPLACE-ORDP01-ORDP2                            
113800                                                                          
113900              MOVE TAB-KATEGORI (IX2) TO W-KVART                          
114000              PERFORM S01-SUM-SALDO-WDK6                                  
114100              PERFORM S02-SKAPA-TRANS                                     
114200                                                                          
114300              SUBTRACT TAB-KATEGORI (IX2) FROM RAD-KVART                  
114400              PERFORM IMS-REPLACE-ORDQ01-ORDP01                           
114500                                                                          
114600              SUBTRACT TAB-KATEGORI (IX2) FROM W-KVAR-ATT-TACKA           
114700              MOVE W-KATEGORI-REST TO TAB-KATEGORI (IX2)                  
114800           END-IF                                                         
114900        ELSE                                                              
115000           IF TAB-KATEGORI (IX2) NOT < RAD-KVART                          
115100              MOVE RAD-KVART     TO W-KVART                               
115200              PERFORM S01-SUM-SALDO-WDK6                                  
115300              PERFORM S02-SKAPA-TRANS                                     
115400              MOVE '3'             TO RAD-KDSTARAD                        
115500              MOVE W-TIAAMMDD      TO RAD-TIRES                           
115600                                                                          
115700              SUBTRACT RAD-KVART FROM W-KVAR-ATT-TACKA                    
115800              SUBTRACT RAD-KVART FROM TAB-KATEGORI (IX2)                  
115900*CALL FOR W440EMOH                                                        
116000              IF RAD-KDROPACK  = '3'                                      
116100                MOVE RAD-IDDISTR     TO EMOH-IDDISTR                      
116200                MOVE RAD-IDKUNDNR    TO EMOH-IDKUNDNR                     
116300                MOVE RAD-IDORDNR5    TO EMOH-IDORDNR7                     
116400                MOVE RAD-KDORDKL     TO EMOH-KDORDKL                      
116500                MOVE RAD-IDDC        TO EMOH-IDDC                         
116600                CALL W440EMOH   USING   EMOH-W440EMOH                     
116700                                        EMOH-WDQ2-PCB                     
116800                                                                          
116900                                                                          
117000                IF EMOH-KDSVAR-CREATE                                     
117100                  PERFORM S11-SKAPA-ORDERHUVUD                            
117200                  MOVE TAB-IDORDNR(ORD-IX)                                
117300                                     TO RAD-IDARBREF                      
117400                END-IF                                                    
117500                IF EMOH-KDSVAR-EXISTS                                     
117600                  MOVE EMOH-IXHALV                                        
117700                                     TO ORD-IX                            
117800                  MOVE TAB-IDORDNR(ORD-IX)                                
117900                                     TO RAD-IDARBREF                      
118000                END-IF                                                    
118100              END-IF                                                      
118200**SKAPA 'DÖSKALLAR' OM DET ÄR EN VERKSTADSORDER                           
118300              IF WS-RELEASE-WIP = 'Y'                                     
118400                  MOVE 'N'             TO WS-RELEASE-WIP                  
118500                  PERFORM S11-SKAPA-ORDERHUVUD                            
118600              END-IF                                                      
118700              PERFORM IMS-REPLACE-ORDQ01-ORDP01                           
118800                                                                          
118900           ELSE                                                           
119000              SUBTRACT TAB-KATEGORI (IX2) FROM RAD-KVART                  
119100              PERFORM IMS-REPLACE-ORDQ01-ORDP01                           
119200                                                                          
119300              MOVE TAB-KATEGORI (IX2)  TO W-KVART                         
119400              PERFORM S01-SUM-SALDO-WDK6                                  
119500              PERFORM S02-SKAPA-TRANS                                     
119600              MOVE '3'                 TO RAD-KDSTARAD                    
119700              MOVE TAB-KATEGORI (IX2)  TO RAD-KVART                       
119800              MOVE W-TIAAMMDD          TO RAD-TIRES                       
119900*CALL FOR W440EMOH                                                        
120000              IF RAD-KDROPACK  = '3'                                      
120100                MOVE RAD-IDDISTR       TO EMOH-IDDISTR                    
120200                MOVE RAD-IDKUNDNR      TO EMOH-IDKUNDNR                   
120300                MOVE RAD-IDORDNR5      TO EMOH-IDORDNR7                   
120400                MOVE RAD-KDORDKL       TO EMOH-KDORDKL                    
120500                MOVE RAD-IDDC          TO EMOH-IDDC                       
120600                CALL W440EMOH   USING     EMOH-W440EMOH                   
120700                                          EMOH-WDQ2-PCB                   
120800                                                                          
120900                                                                          
121000                IF EMOH-KDSVAR-CREATE                                     
121100                  PERFORM S11-SKAPA-ORDERHUVUD                            
121200                  MOVE TAB-IDORDNR(ORD-IX)                                
121300                                       TO RAD-IDARBREF                    
121400                END-IF                                                    
121500                IF EMOH-KDSVAR-EXISTS                                     
121600                  MOVE EMOH-IXHALV                                        
121700                                       TO ORD-IX                          
121800                  MOVE TAB-IDORDNR(ORD-IX)                                
121900                                       TO RAD-IDARBREF                    
122000                END-IF                                                    
122100              END-IF                                                      
122200**SKAPA 'DÖSKALLAR' OM DET ÄR EN VERKSTADSORDER                           
122300              IF WS-RELEASE-WIP = 'Y'                                     
122400                  MOVE 'N'             TO WS-RELEASE-WIP                  
122500                  PERFORM S11-SKAPA-ORDERHUVUD                            
122600              END-IF                                                      
122700              PERFORM S10-INSERT-ORDP01                                   
122800                                                                          
122900                                                                          
123000              SUBTRACT TAB-KATEGORI (IX2) FROM W-KVAR-ATT-TACKA           
123100              MOVE W-KATEGORI-REST TO TAB-KATEGORI (IX2)                  
123200           END-IF                                                         
123300        END-IF                                                            
123400     END-IF                                                               
123500     .                                                                    
123600     EJECT                                                                
123700                                                                          
123800 G-FULL-TAECKNING SECTION.                                                
123900* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
124000*    LÄSER ALLA OTÄCKTA RO-RADER I PRIORITETSORDNING.           *         
124100*    BEHANDLA DE RADER SOM KAN TÄCKAS FRÅN HTR'S CLAGER.        *         
124200*    OM ORSAKSKOD ÄR LIKA MED INLÄGGNINGSFÖRDELNING SKALL       *         
124300*    ENDAST HTR'S CLAGER RADER BEHANDLAS.                       *         
124400*                                                               *         
124500*    GER SÅ LÄNGE DISPONIBELT RÄCKER.                           *         
124600*                                                               *         
124700*    KONTROLLERA OM MAN KAN SLÅ IHOP TÄCKT RAD MED TIDIGARE     *         
124800*    TÄCKT OCH EJ BIPACKAD RAD.                                 *         
124900* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
125000                                                                          
125100     MOVE LOW-VALUE    TO W1-WDA5ASEQ-X                                   
125200     MOVE HIGH-VALUE   TO W2-WDA5ASEQ-X                                   
125300     MOVE 4506-IDARTNR TO W1-IDARTNR W2-IDARTNR                           
125400     MOVE WC-CDC-SE    TO W1-IDDC    W2-IDDC                              
125500     MOVE '2'          TO W-KDSTARAD                                      
125600     PERFORM IMS-GHN-ORDQ01-ORDP01-02                                     
125700                                                                          
125800     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
125900                   W-DISPONIBELT = +0 OR                                  
126000                   CHKP-ANT > CHKP-MAX                                    
126100                                                                          
126200       MOVE    NEJ TO W-SAMMANSLAGNING-RAD                                
126300*FOR WORKSHOP ORDER IF THE ORDERCLASS IS 2 OR 1 THEN WE RELEASE           
126400*THEM AND IF IT IS CLASS 3 IF TODAY DATE IS GREATER THAN                  
126500*REPDAT THEN RELEASE IT                                                   
126600       IF RAD-TIREPDAT NOT = ZERO                                         
126700           IF WS-DAT     > RAD-TIREPDAT OR                                
126800             ( RAD-KDORDKL =  1  OR  2 )                                  
126900               MOVE 'Y'   TO WS-RELEASE-WIP                               
127000           END-IF                                                         
127100       END-IF                                                             
127200       IF RAD-TIREPDAT     = ZERO OR                                      
127300          WS-RELEASE-WIP    = 'Y'                                         
127400         PERFORM S03-SAMMANSLAGNING-RAD                                   
127500                                                                          
127600         IF CLAG-KVQPACK-1 > +1                                           
127700           IF ART-KDSORT = 'KG' OR 'M ' OR 'L ' OR                        
127800              RAD-KDKVBRYT = +0                                           
127900             IF W-DISPONIBELT < RAD-KVART                                 
128000               COMPUTE W-ANTAL-ARTIKLAR ROUNDED =                         
128100                      (W-DISPONIBELT / CLAG-KVQPACK-1) - 0.5              
128200               COMPUTE W-DISPONIBELT ROUNDED =                            
128300                       W-ANTAL-ARTIKLAR * CLAG-KVQPACK-1                  
128400             END-IF                                                       
128500           END-IF                                                         
128600         END-IF                                                           
128700         IF W-DISPONIBELT > +0                                            
128800           PERFORM GA-TAECKNING-RAD                                       
128900         END-IF                                                           
129000                                                                          
129100         MOVE '2' TO W-KDSTARAD                                           
129200       END-IF                                                             
129300         PERFORM IMS-GHN-ORDQ01-ORDP01-02                                 
129400     END-PERFORM                                                          
129500                                                                          
129600     IF CHKP-ANT > CHKP-MAX                                               
129700       MOVE NEJ TO SW-RO-PA-ARTIKEL-SLUT                                  
129800     END-IF                                                               
129900     .                                                                    
130000     EJECT                                                                
130100 GA-TAECKNING-RAD SECTION.                                                
130200                                                                          
130300     IF W-SAMMANSLAGNING-RAD = JA                                         
130400       IF W-DISPONIBELT NOT < RAD-KVART                                   
130500          COMPUTE RESRAD-KVART = RESRAD-KVART +                           
130600                                 RAD-KVART                                
130700          MOVE W-TIAAMMDD    TO RESRAD-TIRES                              
130800          PERFORM IMS-REPLACE-ORDP01-ORDP2                                
130900                                                                          
131000          MOVE RAD-KVART     TO W-KVART                                   
131100          PERFORM S01-SUM-SALDO-WDK6                                      
131200          PERFORM S02-SKAPA-TRANS                                         
131300                                                                          
131400          SUBTRACT RAD-KVART FROM  W-DISPONIBELT                          
131500                                                                          
131600          MOVE RAD-IDDISTR   TO W3-IDDISTR                                
131700          MOVE RAD-IDKUNDNR  TO W3-IDKUNDNR                               
131800          MOVE RAD-IDKUNDRF  TO W3-IDKUNDRF                               
131900          MOVE RAD-IDARTNR   TO W3-IDARTNR                                
132000          MOVE RAD-IDLOPNR   TO W3-IDLOPNR                                
132100          PERFORM IMS-GHU-ORDP01-ORDP1                                    
132200                                                                          
132300          PERFORM IMS-DELETE-ORDP01                                       
132400       ELSE                                                               
132500          COMPUTE RESRAD-KVART = RESRAD-KVART +                           
132600                                 W-DISPONIBELT                            
132700          MOVE W-TIAAMMDD      TO RESRAD-TIRES                            
132800          PERFORM IMS-REPLACE-ORDP01-ORDP2                                
132900                                                                          
133000          MOVE W-DISPONIBELT   TO W-KVART                                 
133100          PERFORM S01-SUM-SALDO-WDK6                                      
133200          PERFORM S02-SKAPA-TRANS                                         
133300                                                                          
133400          SUBTRACT W-DISPONIBELT FROM RAD-KVART                           
133500          PERFORM IMS-REPLACE-ORDQ01-ORDP01                               
133600                                                                          
133700          MOVE +0              TO W-DISPONIBELT                           
133800       END-IF                                                             
133900     ELSE                                                                 
134000       IF W-DISPONIBELT NOT < RAD-KVART                                   
134100          MOVE RAD-KVART       TO W-KVART                                 
134200          PERFORM S01-SUM-SALDO-WDK6                                      
134300          PERFORM S02-SKAPA-TRANS                                         
134400          MOVE '3'             TO RAD-KDSTARAD                            
134500          MOVE W-TIAAMMDD      TO RAD-TIRES                               
134600*CALL FOR W440EMOH                                                        
134700          IF RAD-KDROPACK  = '3'                                          
134800             MOVE RAD-IDDISTR     TO EMOH-IDDISTR                         
134900             MOVE RAD-IDKUNDNR    TO EMOH-IDKUNDNR                        
135000             MOVE RAD-IDORDNR5    TO EMOH-IDORDNR7                        
135100             MOVE RAD-KDORDKL     TO EMOH-KDORDKL                         
135200             MOVE RAD-IDDC        TO EMOH-IDDC                            
135300             CALL W440EMOH   USING   EMOH-W440EMOH                        
135400                                     EMOH-WDQ2-PCB                        
135500                                                                          
135600                                                                          
135700             IF EMOH-KDSVAR-CREATE                                        
135800               PERFORM S11-SKAPA-ORDERHUVUD                               
135900               MOVE TAB-IDORDNR(ORD-IX)                                   
136000                                  TO RAD-IDARBREF                         
136100             END-IF                                                       
136200             IF EMOH-KDSVAR-EXISTS                                        
136300                MOVE EMOH-IXHALV                                          
136400                                  TO ORD-IX                               
136500                MOVE TAB-IDORDNR(ORD-IX)                                  
136600                                  TO RAD-IDARBREF                         
136700             END-IF                                                       
136800          END-IF                                                          
136900          IF WS-RELEASE-WIP = 'Y'                                         
137000              MOVE 'N'             TO WS-RELEASE-WIP                      
137100              PERFORM S11-SKAPA-ORDERHUVUD                                
137200          END-IF                                                          
137300          PERFORM IMS-REPLACE-ORDQ01-ORDP01                               
137400          SUBTRACT RAD-KVART     FROM  W-DISPONIBELT                      
137500       ELSE                                                               
137600          SUBTRACT W-DISPONIBELT FROM RAD-KVART                           
137700          PERFORM IMS-REPLACE-ORDQ01-ORDP01                               
137800                                                                          
137900          MOVE W-DISPONIBELT    TO W-KVART                                
138000          PERFORM S01-SUM-SALDO-WDK6                                      
138100          PERFORM S02-SKAPA-TRANS                                         
138200          MOVE '3'              TO RAD-KDSTARAD                           
138300          MOVE W-DISPONIBELT    TO RAD-KVART                              
138400          MOVE W-TIAAMMDD       TO RAD-TIRES                              
138500                                                                          
138600*CALL FOR W440EMOH                                                        
138700          IF RAD-KDROPACK  = '3'                                          
138800             MOVE RAD-IDDISTR   TO EMOH-IDDISTR                           
138900             MOVE RAD-IDKUNDNR  TO EMOH-IDKUNDNR                          
139000             MOVE RAD-IDORDNR5  TO EMOH-IDORDNR7                          
139100             MOVE RAD-KDORDKL   TO EMOH-KDORDKL                           
139200             MOVE RAD-IDDC      TO EMOH-IDDC                              
139300             CALL W440EMOH USING EMOH-W440EMOH                            
139400                                 EMOH-WDQ2-PCB                            
139500                                                                          
139600             IF EMOH-KDSVAR-CREATE                                        
139700               PERFORM S11-SKAPA-ORDERHUVUD                               
139800               MOVE TAB-IDORDNR(ORD-IX)                                   
139900                                TO RAD-IDARBREF                           
140000             END-IF                                                       
140100             IF EMOH-KDSVAR-EXISTS                                        
140200               MOVE EMOH-IXHALV TO ORD-IX                                 
140300                                                                          
140400               MOVE TAB-IDORDNR(ORD-IX)                                   
140500                                TO RAD-IDARBREF                           
140600             END-IF                                                       
140700          END-IF                                                          
140800**SKAPA 'DÖSKALLAR' OM DET ÄR EN VERKSTADSORDER                           
140900          IF WS-RELEASE-WIP = 'Y'                                         
141000              MOVE 'N'             TO WS-RELEASE-WIP                      
141100             PERFORM S11-SKAPA-ORDERHUVUD                                 
141200          END-IF                                                          
141300          PERFORM S10-INSERT-ORDP01                                       
141400                                                                          
141500          MOVE +0   TO W-DISPONIBELT                                      
141600       END-IF                                                             
141700     END-IF                                                               
141800     .                                                                    
141900     EJECT                                                                
142000 H-UPPD-SALDO-WDK6 SECTION.                                               
142100                                                                          
142200     IF W-SUM-KVROS  = +0 AND                                             
142300        W-SUM-KVRESS = +0                                                 
142400         CONTINUE                                                         
142500     ELSE                                                                 
142600        COMPUTE CLAG-KVROS  = CLAG-KVROS  - W-SUM-KVROS                   
142700        COMPUTE CLAG-KVRESS = CLAG-KVRESS + W-SUM-KVRESS                  
142800                                                                          
142900        PERFORM IMS-REPLACE-ARTC11                                        
143000     END-IF                                                               
143100     .                                                                    
143200     EJECT                                                                
143300 I-STYR-TACKNING SECTION.                                                 
143400*****************************************************************         
143500*                                                                         
143600*    STYRNING AV TÄCKNING.                                                
143700*      R32 ELLER EJ, SAMT LTK STYR TÄCKNINGEN.                            
143800*                                                                         
143900*****************************************************************         
144000                                                                          
144100     MOVE 4506-KDTAKORS          TO TACK-KDTAKORS                         
144200                                                                          
144300     IF  TACK-KDTAKORS-R32                                                
144400                                                                          
144500       IF TACK-KVDISP > 4506-KVANTMOT                                     
144600*        *--- FÖR R32 MAXIMERAS DISPONIBELT TILL INLEV. ANTAL             
144700*        *    (DVS TILL 4506-KVANTMOT).                                   
144800         MOVE 4506-KVANTMOT TO TACK-KVDISP                                
144900       ELSE                                                               
145000*        *--- BERÄKNAT DISPONIBELT ÖVERSTIGER EJ INLEV. ANTAL             
145100*        *    OCH BEGRÄNSAS DÄRFÖR EJ AV 4506-KVANTMOT.                   
145200         CONTINUE                                                         
145300       END-IF                                                             
145400       PERFORM IA-TACK-R32                                                
145500     ELSE                                                                 
145600*      *--- EJ R32                                                        
145700       MOVE ART-KDPRODSL         TO TEST-KDPRODSL                         
145800       IF KDPRODSL-BIMA                                                   
145900          CONTINUE                                                        
146000       ELSE                                                               
146100          PERFORM S20-KOLLA-OM-SATSORDER                                  
146200          IF SATS-ORDER                                                   
146300             CONTINUE                                                     
146400          ELSE                                                            
146500             PERFORM S06-BER-RANS-DISP                                    
146600          END-IF                                                          
146700       END-IF                                                             
146800       PERFORM IC-TACK-EJR32                                              
146900     END-IF                                                               
147000     .                                                                    
147100     EJECT                                                                
147200 IA-TACK-R32 SECTION.                                                     
147300*****************************************************************         
147400*    R32-TÄCKNING                                                         
147500*****************************************************************         
147600                                                                          
147700     IF  TACK-KVDISP > ZERO                                               
147800       EVALUATE TACK-KDTAKORS                                             
147900         WHEN  +3                                                         
148000*--------------------------------------- EGET-TÄCK C1                     
148100           MOVE    TACK-TYP-EGET   TO TACK-TYP                            
148200           MOVE    W-KVROS         TO W-RO-ATT-TACKA                      
148300           MOVE    TACK-KVDISP     TO W-DISPONIBELT                       
148400           PERFORM S05-UTFOR-T-OMG                                        
148500           MOVE    W-DISPONIBELT   TO TACK-KVDISP                         
148600                                                                          
148700         WHEN  +2                                                         
148800*--------------------------------------- MINI-TÄCK C1                     
148900           MOVE    TACK-TYP-MINI   TO TACK-TYP                            
149000           MOVE    W-KVROS         TO W-RO-ATT-TACKA                      
149100           MOVE    TACK-KVDISP     TO W-DISPONIBELT                       
149200           PERFORM S05-UTFOR-T-OMG                                        
149300           MOVE    W-DISPONIBELT   TO TACK-KVDISP                         
149400                                                                          
149500*--------------------------------------- MAXI-TÄCK OM DISPONIBELT         
149600*                                        FINNS KVAR                       
149700           IF TACK-KVDISP > ZERO AND                                      
149800              W-KVROS      > ZERO                                         
149900             MOVE    TACK-TYP-MAXI   TO TACK-TYP                          
150000             MOVE    W-KVROS         TO W-RO-ATT-TACKA                    
150100             MOVE    TACK-KVDISP     TO W-DISPONIBELT                     
150200             PERFORM S05-UTFOR-T-OMG                                      
150300             MOVE    W-DISPONIBELT   TO TACK-KVDISP                       
150400           END-IF                                                         
150500                                                                          
150600         WHEN OTHER                                                       
150700*--------------------------------------- MAXI-TÄCK C1                     
150800           MOVE    TACK-TYP-MAXI   TO TACK-TYP                            
150900           MOVE    W-KVROS         TO W-RO-ATT-TACKA                      
151000           MOVE    TACK-KVDISP     TO W-DISPONIBELT                       
151100           PERFORM S05-UTFOR-T-OMG                                        
151200           MOVE    W-DISPONIBELT   TO TACK-KVDISP                         
151300       END-EVALUATE                                                       
151400     ELSE                                                                 
151500       CONTINUE                                                           
151600     END-IF                                                               
151700     .                                                                    
151800     EJECT                                                                
151900 IC-TACK-EJR32 SECTION.                                                   
152000*****************************************************************         
152100*    TÄCKNING ICKE-R32 DÅ LTK = 1                                         
152200*****************************************************************         
152300     IF TACK-KVDISP > ZERO AND                                            
152400        W-KVROS     > ZERO                                                
152500*--------------------------------------- MAXI-TÄCK                        
152600       MOVE    TACK-TYP-MAXI    TO TACK-TYP                               
152700       MOVE    W-KVROS          TO W-RO-ATT-TACKA                         
152800       MOVE    TACK-KVDISP      TO W-DISPONIBELT                          
152900       PERFORM S05-UTFOR-T-OMG                                            
153000       MOVE    W-DISPONIBELT    TO TACK-KVDISP                            
153100     ELSE                                                                 
153200       CONTINUE                                                           
153300     END-IF                                                               
153400     .                                                                    
153500     EJECT                                                                
153600 J-TAG-CHECKPOINT SECTION.                                                
153700                                                                          
153800     IF SW-RO-PA-ARTIKEL-SLUT = JA                                        
153900       PERFORM IMS-DELETE-450511-TAECKTRANS                               
154000     ELSE                                                                 
154100       COMPUTE 4506-KVANTMOT =   4506-KVANTMOT                            
154200                               - W-SUM-KVRESS                             
154300                                                                          
154400       IF 4506-KVANTMOT < ZERO                                            
154500         MOVE ZERO TO 4506-KVANTMOT                                       
154600       END-IF                                                             
154700       PERFORM IMS-REPLACE-450511-TAECKTRANS                              
154800       MOVE    JA  TO SW-RO-PA-ARTIKEL-SLUT                               
154900       DISPLAY 'AVBR TAECK:  ' 4506-IDARTNR '  REST:  '                   
155000       4506-KVANTMOT ' ORS: ' 4506-KDTAKORS                               
155100     END-IF                                                               
155101                                                                          
155110     DISPLAY 'CHKP-ANT: ' CHKP-ANT                                        
155200     PERFORM IMS-CHECKPOINT                                               
155300     MOVE    ZERO  TO CHKP-ANT                                            
155400     PERFORM IMS-GET-450501-TAECKTRANS                                    
155500     .                                                                    
155600     EJECT                                                                
155700 S01-SUM-SALDO-WDK6 SECTION.                                              
155800*****************************************************************         
155900*    SALDO FÖR UPPDATERING WDK6 ACKUMULERAS.                              
156000*                                                                         
156100*      W-KVROS SÄNKS FÖR ATT FÅ KORREKTA                                  
156200*      VÄRDEN INFÖR EVENTUELLT YTTERLIGARE TÄCKNINGS-OMGÅNGAR.            
156300*****************************************************************         
156400                                                                          
156500     ADD      W-KVART TO   W-SUM-KVROS                                    
156600     SUBTRACT W-KVART FROM W-KVROS                                        
156700     ADD      W-KVART TO   W-SUM-KVRESS                                   
156800     .                                                                    
156900     EJECT                                                                
157000 S02-SKAPA-TRANS SECTION.                                                 
157100                                                                          
157200     ACCEPT LOGG-TIAAMMDD FROM DATE                                       
157300     ACCEPT LOGG-TIKLOCK  FROM TIME                                       
157400     ADD +1                  TO LOGG-IDLOGLOP                             
157500     MOVE 'RY4'              TO RY4-IDPTYP                                
157600     MOVE RAD-IDDC           TO RY4-IDDC                                  
157700     MOVE RAD-IDDISTR        TO RY4-IDDISTR                               
157800     MOVE RAD-IDKUNDNR       TO RY4-IDKUNDNR                              
157900     MOVE RAD-IDKUNDRF (1:5) TO RY4-IDRONR                                
158000     MOVE RAD-IDARTNR        TO RY4-IDARTNR                               
158100     MOVE W-KVART            TO RY4-KVRO                                  
158200     MOVE RAD-KDORDKL        TO RY4-KDORDKL                               
158300     MOVE RAD-KDFAKTYP       TO RY4-KDFAKTYP                              
158400     MOVE RAD-KDVRINFO       TO RY4-KDVRINFO                              
158500                                                                          
158600     MOVE RY4-W440300     TO LOGG-LOGGPOST                                
158700                                                                          
158800     PERFORM IMS-INSERT-ZZAC01                                            
158900     PERFORM UNTIL SEGMENT-FINNS                                          
159000       ADD +1 TO LOGG-IDLOGLOP                                            
159100       IF LOGG-IDLOGLOP = 0                                               
159200          ADD +1  TO LOGG-TIKLOCK                                         
159300          MOVE +1 TO LOGG-IDLOGLOP                                        
159400       END-IF                                                             
159500       PERFORM IMS-INSERT-ZZAC01                                          
159600     END-PERFORM                                                          
159700     .                                                                    
159800     EJECT                                                                
159900 S03-SAMMANSLAGNING-RAD SECTION.                                          
160000     MOVE RAD-IDDISTR  TO W3-IDDISTR  W4-IDDISTR                          
160100     MOVE RAD-IDKUNDNR TO W3-IDKUNDNR W4-IDKUNDNR                         
160200     MOVE RAD-IDKUNDRF TO W3-IDKUNDRF W4-IDKUNDRF                         
160300     MOVE RAD-IDARTNR  TO W3-IDARTNR  W4-IDARTNR                          
160400     MOVE +0           TO W3-IDLOPNR                                      
160500     MOVE +999         TO W4-IDLOPNR                                      
160600     MOVE '3'          TO W-KDSTARAD                                      
160700                                                                          
160800     PERFORM IMS-GHU-ORDP01-ORDP2                                         
160900     PERFORM UNTIL SEGMENT-SAKNAS OR W-SAMMANSLAGNING-RAD = JA            
161000       IF RESRAD-KDFRAKT  = RAD-KDFRAKT    AND                            
161100          RESRAD-KDORDKL  = RAD-KDORDKL    AND                            
161200          RESRAD-PRARTNTO = RAD-PRARTNTO   AND                            
161300          RESRAD-PRARTNTO-LOC = RAD-PRARTNTO-LOC   AND                    
161400          RESRAD-PRARTNTO-LOCPREL = RAD-PRARTNTO-LOCPREL AND              
161500          RESRAD-KDTPOTYP = RAD-KDTPOTYP   AND                            
161600          RESRAD-DARODAT  > ZERO                                          
161700         MOVE    JA TO W-SAMMANSLAGNING-RAD                               
161800       ELSE                                                               
161900         PERFORM IMS-GHN-ORDP01-ORDP2                                     
162000       END-IF                                                             
162100     END-PERFORM                                                          
162200     .                                                                    
162300     EJECT                                                                
162400 S05-UTFOR-T-OMG    SECTION.                                              
162500*****************************************************************         
162600*                                                                         
162700*    KODEN I DENNA SECTION LÅG TIDIGARE I STYRDELEN,                      
162800*    MEN HAR NU BRUTITS UT TILL EN EGEN SECTION EFTERSOM                  
162900*    EN 4506-HTR NU KAN GE UPPHOV TILL FLERA                              
163000*    TÄCKNINGS-OMGÅNGAR MOT TIDIGARE ALLTID EN.                           
163100*                                                                         
163200*    VIKTIGA FÄLT I TÄCKNINGEN:                                           
163300*      W-RO-ATT-TACKA  MAX RO-KVANT SOM GÅR ATT TÄCKA                     
163400*      W-DISPONIBELT   DEN DISPONIBLA KVANT SOM RÄKNAS NER                
163500*      TACK-TYP        TYP AV TÄCKNINGS-OMGÅNG                            
163600*                      (EGET, MINI ELLER MAXI)                            
163700*                                                                         
163800*****************************************************************         
163900                                                                          
164000     PERFORM S08-ORDP-STPOS-WDA5ASEQ                                      
164100                                                                          
164200     PERFORM S20-KOLLA-OM-SATSORDER                                       
164300     IF SATS-ORDER                                                        
164400        PERFORM G-FULL-TAECKNING                                          
164500     ELSE                                                                 
164600        IF W-DISPONIBELT < W-RO-ATT-TACKA                                 
164700          PERFORM S09-ORDQ-STPOS-WDA5A1                                   
164800          PERFORM D-METODBESTAMNING                                       
164900          IF W-SUM-KATEGORINORMAL > +0                                    
165000            PERFORM E-KATEGORIFORDELNING                                  
165100                                                                          
165200            PERFORM F-EJ-FULL-TAECKNING                                   
165300            MOVE ZERO        TO W-DISPONIBELT                             
165400          END-IF                                                          
165500        ELSE                                                              
165600          PERFORM G-FULL-TAECKNING                                        
165700        END-IF                                                            
165800     END-IF                                                               
165900     .                                                                    
166000     EJECT                                                                
166100 S06-BER-RANS-DISP SECTION.                                               
166200*                                                                         
166300*    BERÄKNA RANSONERAD DISPONIBEL KVANT.                                 
166400*                                                                         
166500     MOVE SPACE                  TO RROT-W440RROT                         
166600                                                                          
166700     MOVE 4506-IDARTNR           TO RROT-IDARTNR                          
166800     MOVE 1                      TO RROT-KDLTK                            
166900     MOVE W-KVLS                 TO RROT-KVLS      (1)                    
167000     MOVE CLAG-KVPB-SATS         TO RROT-KVPB-SATS (1)                    
167100     MOVE CLAG-KVPB-SEP          TO RROT-KVPB-SEP  (1)                    
167200     MOVE CLAG-KVPB-TPO          TO RROT-KVPB-TPO  (1)                    
167300     MOVE W-KVRESS               TO RROT-KVRESS    (1)                    
167400     MOVE W-KVSPANT              TO RROT-KVSPANT   (1)                    
167500     MOVE W-KVUTRS               TO RROT-KVUTRS    (1)                    
167600     MOVE W-KVROS                TO RROT-KVROS     (1)                    
167700     MOVE RROTW-TIDISPIN         TO RROT-TIDISPIN  (1)                    
167800     MOVE CLAG-REDIRLEV          TO RROT-REDIRLEV  (1)                    
167900     MOVE W-KDERS                TO RROT-KDERS     (1)                    
168000                                                                          
168100     MOVE +0                     TO RROT-KVLS      (2)                    
168200                                    RROT-KVPB-SATS (2)                    
168300                                    RROT-KVPB-SEP  (2)                    
168400                                    RROT-KVPB-TPO  (2)                    
168500                                    RROT-KVRESS    (2)                    
168600                                    RROT-KVSPANT   (2)                    
168700                                    RROT-KVUTRS    (2)                    
168800                                    RROT-KVROS     (2)                    
168900                                    RROT-TIDISPIN  (2)                    
169000                                    RROT-REDIRLEV  (2)                    
169100                                    RROT-KDERS     (2)                    
169200                                                                          
169300     CALL W440RROT USING            RROT-W440RROT                         
169400                                    ARTM-PCB                              
169500                                    ARTS-PCB                              
169600                                                                          
169700     MOVE RROT-KVDISP (1)        TO TACK-KVDISP                           
169800     .                                                                    
169900     EJECT                                                                
170000 S08-ORDP-STPOS-WDA5ASEQ SECTION.                                         
170100*****************************************************************         
170200*                                                                         
170300*    POSITIONERAR FÖRE ARTIKELNS FÖRSTA WDA5ASEQ                          
170400*    GENOM ATT LÄSA GU MED IDARTNR + RESTEN LOW-VALUE.                    
170500*    NÖDVÄNDIGT EFTERSOM FLERA TÄCKNINGS-OMGÅNGAR KAN GÖRAS.              
170600*                                                                         
170700*****************************************************************         
170800                                                                          
170900     MOVE LOW-VALUE              TO W1-WDA5ASEQ-X                         
171000     MOVE 4506-IDARTNR           TO W1-IDARTNR                            
171100                                                                          
171200     PERFORM IMS-GET-ORDP-STPOS-ASEQ                                      
171300     .                                                                    
171400     EJECT                                                                
171500 S09-ORDQ-STPOS-WDA5A1 SECTION.                                           
171600*****************************************************************         
171700*                                                                         
171800*    POSITIONERAR FÖRE ARTIKELNS FÖRSTA WDA501                            
171900*    GENOM ATT LÄSA GU MED IDARTNR + RESTEN LOW-VALUE.                    
172000*    NÖDVÄNDIGT EFTERSOM FLERA TÄCKNINGS-OMGÅNGAR KAN GÖRAS.              
172100*                                                                         
172200*****************************************************************         
172300                                                                          
172400     MOVE LOW-VALUE              TO W5-WDA5A1KY-X                         
172500     MOVE 4506-IDARTNR           TO W5-IDARTNR                            
172600                                                                          
172700     PERFORM IMS-GET-ORDQ-STPOS-A1                                        
172800     .                                                                    
172900     EJECT                                                                
173000 S10-INSERT-ORDP01 SECTION.                                               
173100*****************************************************************         
173200*                                                                         
173300*    RO-RAD INSERTAS DÅ TIDIGARE RAD MÅST DELAS PGA DEL-TÄCKNING.         
173400*    LÖPNR STEGAS UPP TILLS INSERT LYCKAS I OCH MED ATT ETT               
173500*    LEDIGT LÖPNR PÅTRÄFFAS.                                              
173600*                                                                         
173700*****************************************************************         
173800                                                                          
173900     ADD +1                TO RAD-IDLOPNR                                 
174000     PERFORM IMS-INSERT-ORDP01                                            
174100     PERFORM UNTIL SEGMENT-FINNS                                          
174200       ADD +1             TO RAD-IDLOPNR                                  
174300       PERFORM IMS-INSERT-ORDP01                                          
174400     END-PERFORM                                                          
174500     .                                                                    
174600     EJECT                                                                
174700 S11-SKAPA-ORDERHUVUD  SECTION.                                           
174800*****************************************************************         
174900*                                                                         
175000*    RO-RAD FRÅN LDC VERKSTADSORDER SKALL BIPACKAS SÅ SNART SOM           
175100*    MÖJLIGT MED EN SEPARAT ORDER PER VERKSTADSORDER. HÄR SKAPAS          
175200*    ORDERHUVUDEN SOM KOMMER ATT BIPACKA RADEN I W411BIPA.                
175300*                                                                         
175400*****************************************************************         
175500                                                                          
175600     PERFORM S11A-SKAPA-ORDERNR                                           
175700     PERFORM S11B-SKAPA-TRANS-ORDERHUVUD                                  
175800     PERFORM S11C-SKAPA-HUVUD-ORDERRADER                                  
175900     PERFORM S12-SKICKA-TRANS                                             
176000                                                                          
176100                                                                          
176200     .                                                                    
176300                                                                          
176400 S11A-SKAPA-ORDERNR  SECTION.                                             
176500*****************************************************************         
176600*                                                                         
176700*    TA UT ETT ORDERNUMMER MHA W411ORDN.                                  
176800*                                                                         
176900*****************************************************************         
177000                                                                          
177100                                                                          
177200     MOVE 'W440'          TO ORDN-IDSYSTEM                                
177300     MOVE RAD-IDDISTR     TO ORDN-IDDISTR                                 
177400     MOVE RAD-IDKUNDNR    TO ORDN-IDKUNDNR                                
177500     MOVE ZERO            TO ORDN-IDORDNR-IN                              
177600                                                                          
177700     CALL W411ORDN USING ORDN-W411ORDN ORDN-XXKP-PCB ORDN-ORQL-PCB        
177800                                       ORDN-PROC-PCB ORDN-ORQI-PCB        
177900                                                                          
178000     MOVE ORDN-IDORDNR-UT TO WS-IDORDNR-NUM                               
178100     IF EMOH-KDSVAR-CREATE                                                
178200        MOVE EMOH-IXHALV  TO ORD-IX                                       
178300        MOVE WS-IDORDNR-NUM                                               
178400                          TO TAB-IDORDNR(ORD-IX)                          
178500     END-IF                                                               
178600     .                                                                    
178700 S11B-SKAPA-TRANS-ORDERHUVUD SECTION.                                     
178800     MOVE SPACE         TO MSG-KOM-WMSGKOM                                
178900     MOVE +54           TO MSG-KOM-KVLL                                   
179000     MOVE LOW-VALUE     TO MSG-KOM-KDZ1                                   
179100                           MSG-KOM-KDZ2                                   
179200     MOVE SPACE         TO MSG-KOM-KDTRANS                                
179300     MOVE 'W4I25101'    TO MSG-KOM-IDCPYTXT                               
179400     MOVE 'LDC-RO  '    TO MSG-KOM-IDSNDNOD                               
179500     MOVE 'W4403000'    TO MSG-KOM-IDSNDJOB                               
179600     ACCEPT MSG-KOM-TIREGDAT FROM DATE                                    
179700     ACCEPT MSG-KOM-TIKLOCK  FROM TIME                                    
179800     MOVE SPACE         TO MSG-KOM-IDMFSMED                               
179900                           MSG-KOM-KDSVAR                                 
180000                                                                          
180100     MOVE LENGTH OF OHUV-MID-W4I25101 TO MSG-KVLL                         
180200     ADD +17                TO MSG-KVLL                                   
180300     MOVE LOW-VALUE         TO MSG-KDZ1                                   
180400                               MSG-KDZ2                                   
180500     MOVE 'W4T251X'         TO MSG-KDTRANS-1                              
180600     MOVE '4251'            TO MSG-IDTRANS-1                              
180700     MOVE '1'               TO MSG-KDMFSFOR-1                             
180800                                                                          
180900     MOVE SPACE             TO OHUV-MID-W4I25101                          
181000     IF RAD-IDSYSTEM (1:3) = 'LYN'                                        
181100        MOVE 'LYND'         TO OHUV-MID-IDSYSTEM                          
181200     ELSE                                                                 
181300        IF RAD-IDSYSTEM (1:3) = 'POL'                                     
181400           MOVE 'POLD'      TO OHUV-MID-IDSYSTEM                          
181500        ELSE                                                              
181600           IF RAD-IDSYSTEM (1:3) = 'ECO'                                  
181700             MOVE 'ECOD'      TO OHUV-MID-IDSYSTEM                        
181800           ELSE                                                           
181900             IF RAD-IDSYSTEM (1:3) = 'VOU'                                
182000                MOVE 'VOUD'      TO OHUV-MID-IDSYSTEM                     
182100             ELSE                                                         
182200               IF RAD-IDSYSTEM (1:3) = 'TAD'                              
182300                  MOVE 'TADD'    TO OHUV-MID-IDSYSTEM                     
182400               ELSE                                                       
182410                 IF RAD-IDSYSTEM (1:3) = 'ACC'                            
182420                    MOVE 'ACCD'    TO OHUV-MID-IDSYSTEM                   
182430                 ELSE                                                     
182520                   IF RAD-IDSYSTEM (1:3) = 'APA'                          
182530                      MOVE 'APAD'  TO OHUV-MID-IDSYSTEM                   
182540                   ELSE                                                   
182550                     IF RAD-IDSYSTEM (1:3) = 'APB'                        
182551                        MOVE 'APBD' TO OHUV-MID-IDSYSTEM                  
182552                     ELSE                                                 
182553                       IF RAD-IDSYSTEM (1:3) = 'APC'                      
182554                          MOVE 'APCD' TO OHUV-MID-IDSYSTEM                
182555                       ELSE                                               
182556                         IF RAD-IDSYSTEM (1:3) = 'APD'                    
182557                            MOVE 'APDD'      TO OHUV-MID-IDSYSTEM         
182558                         ELSE                                             
182559                           IF RAD-IDSYSTEM (1:3) = 'APE'                  
182560                              MOVE 'APED'    TO OHUV-MID-IDSYSTEM         
182561                           ELSE                                           
182562                             IF RAD-IDSYSTEM (1:3) = 'APF'                
182563                               MOVE 'APFD'    TO OHUV-MID-IDSYSTEM        
182564                             ELSE                                         
182565                               IF RAD-IDSYSTEM (1:3) = 'APG'              
182566                                  MOVE 'APGD' TO OHUV-MID-IDSYSTEM        
182567                               ELSE                                       
182568                                 IF RAD-IDSYSTEM (1:3) = 'APH'            
182569                                    MOVE 'APHD'                           
182570                                              TO OHUV-MID-IDSYSTEM        
182571                                 ELSE                                     
182572                                  IF RAD-IDSYSTEM (1:3) = 'API'           
182573                                     MOVE 'APID'                          
182574                                              TO OHUV-MID-IDSYSTEM        
182575                                  ELSE                                    
182576                                    IF RAD-IDSYSTEM (1:3) = 'APJ'         
182577                                       MOVE 'APJD'                        
182578                                              TO OHUV-MID-IDSYSTEM        
182579                                    ELSE                                  
182580                                       MOVE 'LDCD'                        
182581                                              TO OHUV-MID-IDSYSTEM        
182582                                    END-IF                                
182583                                  END-IF                                  
182584                                 END-IF                                   
182585                               END-IF                                     
182586                             END-IF                                       
182587                           END-IF                                         
182588                         END-IF                                           
182589                       END-IF                                             
182590                      END-IF                                              
182591                    END-IF                                                
182592                 END-IF                                                   
182600               END-IF                                                     
182700             END-IF                                                       
182800           END-IF                                                         
182900        END-IF                                                            
183000     END-IF                                                               
183100     MOVE RAD-IDDISTR       TO WS-IDDISTR-NUM                             
183200     MOVE WS-IDDISTR        TO OHUV-MID-IDDISTR                           
183300     MOVE RAD-IDKUNDNR      TO WS-IDKUNDNR-NUM                            
183400     MOVE WS-IDKUNDNR       TO OHUV-MID-IDKUNDNR                          
183500     MOVE WS-IDORDNR        TO OHUV-MID-IDORDNR                           
183600     MOVE RAD-KDORDKL       TO OHUV-MID-KDORDKL                           
183700     IF RAD-IDKONTO = ZERO                                                
183800        MOVE SPACE          TO OHUV-MID-IDKONTO                           
183900     ELSE                                                                 
184000        MOVE RAD-IDKONTO    TO WS-IDKONTO-DISPLAY                         
184100        MOVE WS-IDKONTO-DISPLAY TO OHUV-MID-IDKONTO                       
184200        MOVE '57'               TO OHUV-MID-IDFTG                         
184300     END-IF                                                               
184400     MOVE RAD-IDKST         TO OHUV-MID-IDKST                             
184500*IF BIPACK CODE IS 3 AND NEW ORDERLINE IN WDA5                            
184600      IF EMOH-KDSVAR-CREATE                                               
184700        MOVE '3'            TO OHUV-MID-KDROPACK                          
184800        MOVE EMOH-BEGMT     TO OHUV-MID-BEGMT                             
184900        MOVE EMOH-ADGMT-GATA                                              
185000                            TO OHUV-MID-ADGMT-GATA                        
185100        MOVE EMOH-ADGMT-PADR                                              
185200                            TO OHUV-MID-ADGMT-PADR                        
185300        MOVE EMOH-BELAGINS-GRP                                            
185400                            TO OHUV-MID-BELAGINS                          
185500                                                                          
185600        IF (RAD-IDSYSTEM (1:3) = 'LYN' OR 'POL' OR 'ECO' OR 'VOU'         
185700                                       OR 'TAD' OR 'ACC' OR 'APA'         
185710                                       OR 'APB' OR 'APC' OR 'APD'         
185720                                       OR 'APE' OR 'APF' OR 'APG'         
185730                                       OR 'APH' OR 'API' OR 'APJ')        
185800*         *ADBET AND BEBET WILL CONTAIN CONTACT INFO                      
185900*         *FOR LYNK/POLESTAR AND SHALL BE COPIED FROM OLD ORDER           
186000          MOVE EMOH-ADBET                                                 
186100                            TO OHUV-MID-ADBET                             
186200          MOVE EMOH-BEBET                                                 
186300                            TO OHUV-MID-BEBET                             
186400        END-IF                                                            
186500                                                                          
186600        INITIALIZE             EMOH-KDSVAR                                
186700      ELSE                                                                
186800        MOVE 'P'            TO OHUV-MID-KDROPACK                          
186900      END-IF                                                              
187000     MOVE RAD-IDANALYS      TO OHUV-MID-IDANALYS                          
187100     MOVE RAD-IDDC          TO OHUV-MID-IDDC                              
187200     MOVE NEJ               TO OHUV-MID-FLAUTFAK                          
187300     MOVE NEJ               TO OHUV-MID-FLAUTPAC                          
187400                               OHUV-MID-FLEMBORD                          
187500                               OHUV-MID-FLOVRLEV                          
187600                               OHUV-MID-FLFORBI                           
187700     MOVE RAD-KDORDTYP-LDC  TO OHUV-MID-KDORDTYP-LDC                      
187800     PERFORM S11BA-SKAPA-RFSDATUM                                         
187900     MOVE WS-TIRFS          TO OHUV-MID-TIRFS                             
188000     MOVE RAD-TIREPDAT      TO OHUV-MID-TIREPDAT                          
188100     MOVE NEJ               TO OHUV-MID-FLORDTIL                          
188200                               OHUV-MID-IDGROSS                           
188300***FÖR ATT HÅLLA REDA PÅ ATT DET ÄR SKAPAT ETT ORDERHUVUD FÖR EN          
188400***SPECIFIK VERKSTADSORDER                                                
188500     MOVE RAD-TIREPDAT      TO W-IDKUNDRF-WIP                             
188600***                                                                       
188700     DISPLAY 'NEW ORDER DETAILS'                                          
188800     DISPLAY '******************'                                         
188900     DISPLAY'IDDISTR  -  'OHUV-MID-IDDISTR                                
189000     DISPLAY'IDKUNDNR -  'OHUV-MID-IDKUNDNR                               
189100     DISPLAY'IDORDNR  -  'OHUV-MID-IDORDNR                                
189200     DISPLAY'KDROPACK -  'RAD-KDROPACK                                    
189300     DISPLAY'IDARTNR  -  'RAD-IDARTNR                                     
189400     DISPLAY'TIREPDAT -  'RAD-TIREPDAT                                    
189500     DISPLAY'OLD-ORDERNO-'RAD-IDORDNR5                                    
189600***                                                                       
189700***                                                                       
189800     MOVE OHUV-AREA TO MSG-MID-OUT                                        
189900     PERFORM S12-SKICKA-TRANS                                             
190000     .                                                                    
190100     EJECT                                                                
190200 S11C-SKAPA-HUVUD-ORDERRADER SECTION.                                     
190300                                                                          
190400     MOVE 'W4I25201'    TO MSG-KOM-IDCPYTXT                               
190500     MOVE LENGTH OF ORAD-MID-W4I25201 TO MSG-KVLL                         
190600     ADD +17            TO MSG-KVLL                                       
190700     MOVE LOW-VALUE     TO MSG-KDZ1                                       
190800                           MSG-KDZ2                                       
190900     MOVE 'W4T252X'     TO MSG-KDTRANS-1                                  
191000     MOVE '4252'        TO MSG-IDTRANS-1                                  
191100     MOVE '1'           TO MSG-KDMFSFOR-1                                 
191200                                                                          
191300     MOVE SPACE         TO ORAD-MID-W4I25201                              
193410     IF RAD-IDSYSTEM (1:3) = 'LYN'                                        
193420        MOVE 'LYND'         TO OHUV-MID-IDSYSTEM                          
193430     ELSE                                                                 
193440        IF RAD-IDSYSTEM (1:3) = 'POL'                                     
193450           MOVE 'POLD'      TO OHUV-MID-IDSYSTEM                          
193460        ELSE                                                              
193470           IF RAD-IDSYSTEM (1:3) = 'ECO'                                  
193480             MOVE 'ECOD'      TO OHUV-MID-IDSYSTEM                        
193490           ELSE                                                           
193491             IF RAD-IDSYSTEM (1:3) = 'VOU'                                
193492                MOVE 'VOUD'      TO OHUV-MID-IDSYSTEM                     
193493             ELSE                                                         
193494               IF RAD-IDSYSTEM (1:3) = 'TAD'                              
193495                  MOVE 'TADD'    TO OHUV-MID-IDSYSTEM                     
193496               ELSE                                                       
193497                 IF RAD-IDSYSTEM (1:3) = 'ACC'                            
193498                    MOVE 'ACCD'    TO OHUV-MID-IDSYSTEM                   
193499                 ELSE                                                     
193500                   IF RAD-IDSYSTEM (1:3) = 'APA'                          
193501                      MOVE 'APAD'  TO OHUV-MID-IDSYSTEM                   
193502                   ELSE                                                   
193503                     IF RAD-IDSYSTEM (1:3) = 'APB'                        
193504                        MOVE 'APBD' TO OHUV-MID-IDSYSTEM                  
193505                     ELSE                                                 
193506                       IF RAD-IDSYSTEM (1:3) = 'APC'                      
193507                          MOVE 'APCD' TO OHUV-MID-IDSYSTEM                
193508                       ELSE                                               
193509                         IF RAD-IDSYSTEM (1:3) = 'APD'                    
193510                            MOVE 'APDD'      TO OHUV-MID-IDSYSTEM         
193511                         ELSE                                             
193512                           IF RAD-IDSYSTEM (1:3) = 'APE'                  
193513                              MOVE 'APED'    TO OHUV-MID-IDSYSTEM         
193514                           ELSE                                           
193515                             IF RAD-IDSYSTEM (1:3) = 'APF'                
193516                               MOVE 'APFD'    TO OHUV-MID-IDSYSTEM        
193517                             ELSE                                         
193518                               IF RAD-IDSYSTEM (1:3) = 'APG'              
193519                                  MOVE 'APGD' TO OHUV-MID-IDSYSTEM        
193520                               ELSE                                       
193521                                 IF RAD-IDSYSTEM (1:3) = 'APH'            
193522                                    MOVE 'APHD'                           
193523                                              TO OHUV-MID-IDSYSTEM        
193524                                 ELSE                                     
193525                                  IF RAD-IDSYSTEM (1:3) = 'API'           
193526                                     MOVE 'APID'                          
193527                                              TO OHUV-MID-IDSYSTEM        
193528                                  ELSE                                    
193529                                    IF RAD-IDSYSTEM (1:3) = 'APJ'         
193530                                       MOVE 'APJD'                        
193531                                              TO OHUV-MID-IDSYSTEM        
193532                                    ELSE                                  
193533                                       MOVE 'LDCD'                        
193534                                              TO OHUV-MID-IDSYSTEM        
193535                                    END-IF                                
193536                                  END-IF                                  
193537                                 END-IF                                   
193538                               END-IF                                     
193539                             END-IF                                       
193540                           END-IF                                         
193541                         END-IF                                           
193542                       END-IF                                             
193543                      END-IF                                              
193544                    END-IF                                                
193545                 END-IF                                                   
193546               END-IF                                                     
193547             END-IF                                                       
193548           END-IF                                                         
193549        END-IF                                                            
193550     END-IF                                                               
193560     MOVE WS-IDDISTR    TO ORAD-MID-IDDISTR                               
193600     MOVE WS-IDKUNDNR   TO ORAD-MID-IDKUNDNR                              
193700     MOVE WS-IDORDNR    TO ORAD-MID-IDORDNR                               
193800     MOVE SPACE         TO ORAD-MID-BEVOLREF                              
193900     MOVE 'J'           TO ORAD-MID-FLSLUT                                
194000     MOVE ORAD-AREA     TO MSG-MID-OUT                                    
194100     .                                                                    
194200     EJECT                                                                
194300 S12-SKICKA-TRANS SECTION.                                                
194400                                                                          
194500     CALL W006KOM USING MSG-PCB                                           
194600                        0693-PCB                                          
194700                        WDP8-PCB                                          
194800                        MSG-KOM-WMSGKOM                                   
194900                        MSG-IO-AREA                                       
195000     .                                                                    
195100     EJECT                                                                
195200 S11BA-SKAPA-RFSDATUM SECTION.                                            
195300                                                                          
195400     MOVE RAD-IDDISTR              TO W-IDDISTR-WDB2                      
195500     MOVE RAD-IDKUNDNR             TO W-IDKUNDNR-WDB2                     
195600     PERFORM IMS-GU-WDB201                                                
195700                                                                          
195800     MOVE RAD-IDDC                 TO WORK-IDDC                           
195900     MOVE +002                     TO WORK-KDCALL                         
196000     MOVE +001                     TO WORK-KVWORKD                        
196100     IF  RAD-TIREPDAT     = ZERO                                          
196200        MOVE WS-DAT                TO WORK-TIAAMMDD-FOM                   
196300     ELSE                                                                 
196400        MOVE RAD-TIREPDAT          TO WORK-TIAAMMDD-FOM                   
196500     END-IF                                                               
196600     CALL WORKDAY                  USING WORK-KDCALL                      
196700                                         WORK-DATE-AREA                   
196800                                         WORK-KDSVAR                      
196900     IF WORK-KDSVAR-FEL                                                   
197000        MOVE 'SECT S01-1, DATUM SAKNAS I WORKDAY'                         
197100                                   TO FELTEXT                             
197200        CALL ABEND                 USING RKOD-ABEND-UTAN-DUMP             
197300     ELSE                                                                 
197400       MOVE +003                   TO WORK-KDCALL                         
197500       MOVE GMT-KVDAGAR-RFS-DEF    TO WORK-KVWORKD                        
197600       PERFORM                                                            
197700       VARYING RFS-IX FROM 1 BY 1                                         
197800         UNTIL RFS-IX > MAX-RFS-IX                                        
197900         IF GMT-IDDC-RFS (RFS-IX) = WORK-IDDC                             
198000           MOVE GMT-KVDAGAR-RFS (RFS-IX)                                  
198100                                   TO WORK-KVWORKD                        
198200         END-IF                                                           
198300       END-PERFORM                                                        
198400       ADD +1  TO WORK-KVWORKD                                            
198500*      +1 FÖR ATT VARIABELN SKALL KUNNA INNEHÅLLA                         
198600*      ANTAL DAGAR FÖRE RFS.                                              
198700*      0 GER DÅ SAMMA DAG, 1 GER FÖRSTA ARBETSDAG FÖRE OSV...             
198800*      OM VI INTE ADDERAR +1 SKULLE VARIABELN SÄTTAS SÅ                   
198900*      1 GER SAMMA DAG, 2 FÖRSTA ARBETSDAG FÖRE OSV...                    
199000*                                                                         
199100                                                                          
199200       CALL WORKDAY                USING WORK-KDCALL                      
199300                                         WORK-DATE-AREA                   
199400                                         WORK-KDSVAR                      
199500       IF WORK-KDSVAR-FEL                                                 
199600          MOVE 'SECT S01-2, DATUM SAKNAS I WORKDAY'                       
199700                                   TO FELTEXT                             
199800          CALL ABEND               USING RKOD-ABEND-UTAN-DUMP             
199900       ELSE                                                               
200000         IF WORK-TIAAMMDD-FOM < WS-DAT                                    
200100           MOVE RAD-IDDC           TO WORK-IDDC                           
200200           MOVE +002               TO WORK-KDCALL                         
200300           MOVE +001               TO WORK-KVWORKD                        
200400           MOVE WS-DAT             TO WORK-TIAAMMDD-FOM                   
200500           CALL WORKDAY            USING WORK-KDCALL                      
200600                                         WORK-DATE-AREA                   
200700                                         WORK-KDSVAR                      
200800           IF WORK-KDSVAR-FEL                                             
200900              MOVE 'SECT S01-3, DATUM SAKNAS I WORKDAY'                   
201000                                   TO FELTEXT                             
201100              CALL ABEND           USING RKOD-ABEND-UTAN-DUMP             
201200           ELSE                                                           
201300              MOVE WORK-TIAAMMDD-TOM TO WS-TIRFS                          
201400           END-IF                                                         
201500         ELSE                                                             
201600           MOVE WORK-TIAAMMDD-FOM  TO WS-TIRFS                            
201700         END-IF                                                           
201800         IF WS-TIRFS = WS-DAT                                             
201900            MOVE ZERO              TO WS-TIRFS                            
202000*           OM VI SKICKAR DAGENS DATUM TILL 4251                          
202100*           KOMMER SEDAN W411TRAN ATT ANROPAS MED ZERO I TID              
202200*           OCH VI KOMMER ATT FÅ FÖRSTA TRANSPORT DEN DAGEN               
202300*           SKICKAR VI ZERO TILL 4251 KOMMER VI ATT FÅ FÖRSTA             
202400*           MÖJLIGA TRANSPORT (EVENTUELLT NÄSTA DAG)                      
202500*           PROBLEMET VI VILL LÖSA ÄR OM VI FÅR RFS = DAGENS              
202600*           KAN VI FÅ EN AVGÅNGSTID TIDIGARE ÄN ORDERN ÄR LAGD            
202700                                                                          
202800         END-IF                                                           
202900       END-IF                                                             
203000     END-IF                                                               
203100     .                                                                    
203200     EJECT                                                                
203300 S20-KOLLA-OM-SATSORDER  SECTION.                                         
203400******************************************************************        
203500*    FÖR SATSARTIKLAR SKALL FULL TÄCKNING GÖRAS OM                        
203600*    SATSBEOV / TOTALA BEHOVET = ELLER > 0,8                              
203700******************************************************************        
203800                                                                          
203900     MOVE NEJ TO SW-SATSORDER                                             
204000                                                                          
204100     IF ART-FLIART = JA                                                   
204200        IF CLAG-FLLSRDEL = NEJ                                            
204300           MOVE JA TO SW-SATSORDER                                        
204400        ELSE                                                              
204500           COMPUTE W-TOT-BEHOV = CLAG-KVPB-SATS + CLAG-KVPB-SEP           
204600                                                                          
204700           PERFORM IMS-GU-WDK701                                          
204800           IF SEGMENT-FINNS                                               
204900              PERFORM IMS-GNP-WDK711                                      
205000              PERFORM UNTIL SEGMENT-SAKNAS                                
205100                 IF SLAG-IDLEVNR = '1441'                                 
205200                    COMPUTE W-TOT-BEHOV =                                 
205300                       W-TOT-BEHOV + SLAG-KVPB-REF + SLAG-KVPBREOI        
205400                 END-IF                                                   
205500                 PERFORM IMS-GNP-WDK711                                   
205600              END-PERFORM                                                 
205700           END-IF                                                         
205800                                                                          
205900           IF CLAG-KVPB-SATS > ZERO                                       
206000              IF (CLAG-KVPB-SATS / W-TOT-BEHOV) NOT < 0.8                 
206100                 MOVE JA TO SW-SATSORDER                                  
206200              END-IF                                                      
206300           END-IF                                                         
206400        END-IF                                                            
206500                                                                          
206600        MOVE ZERO TO W-TOT-BEHOV                                          
206700     END-IF                                                               
206800     .                                                                    
206900     EJECT                                                                
207000*    ---- IMS SEKTIONER                                                   
207100                                                                          
207200 IMS-GET-XXJM01 SECTION.                                                  
207300                                                                          
207400     STRING 'WLXXJM01(WDGXKEY  =' W-WDGX-4501-KEY-X ')'                   
207500            DELIMITED BY SIZE INTO SSA1                                   
207600     MOVE '  '                  TO GODK-STATUSKODER                       
207700     CALL CBLTDLI USING GU XXJM-PCB DLI-IO-GX4501 SSA1                    
207800     MOVE XXJM-STATUS-CODE      TO STATUS-WS                              
207900     PERFORM IMS-STATUSKONTROLL                                           
208000     .                                                                    
208100     SKIP3                                                                
208200 IMS-GNP-XXJM11 SECTION.                                                  
208300                                                                          
208400     MOVE 'WLXXJM11 '           TO SSA1                                   
208500     MOVE '  GE'                TO GODK-STATUSKODER                       
208600     CALL CBLTDLI USING GNP XXJM-PCB DLI-IO-GX4502 SSA1                   
208700     MOVE XXJM-STATUS-CODE      TO STATUS-WS                              
208800     PERFORM IMS-STATUSKONTROLL                                           
208900     .                                                                    
209000     EJECT                                                                
209100 IMS-GET-450501-TAECKTRANS SECTION.                                       
209200                                                                          
209300     STRING 'WL450501(WDGXKEY  =' W-WDGX-4505-KEY-X ')'                   
209400            DELIMITED BY SIZE INTO SSA1                                   
209500     MOVE '  GE'                TO GODK-STATUSKODER                       
209600     CALL CBLTDLI USING GU 4505-PCB DLI-IO-GX4506 SSA1                    
209700     MOVE 4505-STATUS-CODE      TO STATUS-WS                              
209800     PERFORM IMS-STATUSKONTROLL                                           
209900     .                                                                    
210000     SKIP3                                                                
210100 IMS-GET-450511-TAECKTRANS SECTION.                                       
210200                                                                          
210300     MOVE 'WL450511 '           TO SSA1                                   
210400     MOVE '  GE'                TO GODK-STATUSKODER                       
210500     CALL CBLTDLI USING GHNP 4505-PCB DLI-IO-GX4506 SSA1                  
210600     MOVE 4505-STATUS-CODE      TO STATUS-WS                              
210700     PERFORM IMS-STATUSKONTROLL                                           
210800     .                                                                    
210900     SKIP3                                                                
211000 IMS-REPLACE-450511-TAECKTRANS SECTION.                                   
211100                                                                          
211200     ADD +1                  TO CHKP-ANT                                  
211300     MOVE '  '               TO GODK-STATUSKODER                          
211400     CALL CBLTDLI USING REPL  4505-PCB DLI-IO-GX4506                      
211500     MOVE 4505-STATUS-CODE   TO STATUS-WS                                 
211600     PERFORM IMS-STATUSKONTROLL                                           
211700     .                                                                    
211800     EJECT                                                                
211900 IMS-DELETE-450511-TAECKTRANS SECTION.                                    
212000                                                                          
212100     ADD +1                  TO CHKP-ANT                                  
212200     MOVE '  '               TO GODK-STATUSKODER                          
212300     CALL CBLTDLI USING DLET  4505-PCB DLI-IO-GX4506                      
212400     MOVE 4505-STATUS-CODE   TO STATUS-WS                                 
212500     PERFORM IMS-STATUSKONTROLL                                           
212600     .                                                                    
212700     SKIP3                                                                
212800 IMS-INSERT-ZZAC01 SECTION.                                               
212900                                                                          
213000     ADD +1                     TO CHKP-ANT                               
213100     MOVE 'WLZZAC01 '           TO SSA1                                   
213200     MOVE '  II'                TO GODK-STATUSKODER                       
213300     CALL CBLTDLI USING ISRT  ZZAC-PCB DLI-IO-ZZAC01 SSA1                 
213400     MOVE ZZAC-STATUS-CODE      TO STATUS-WS                              
213500     PERFORM IMS-STATUSKONTROLL                                           
213600     .                                                                    
213700     EJECT                                                                
213800 IMS-GET-ARTC01 SECTION.                                                  
213900                                                                          
214000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
214100            DELIMITED BY SIZE INTO SSA1                                   
214200     MOVE '  GE'                TO GODK-STATUSKODER                       
214300     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-ARTC01 SSA1                   
214400     MOVE ARTC-STATUS-CODE      TO STATUS-WS                              
214500     PERFORM IMS-STATUSKONTROLL                                           
214600     .                                                                    
214700     EJECT                                                                
214800 IMS-GNP-ARTC11 SECTION.                                                  
214900     MOVE 'WLARTC11 '           TO SSA1                                   
215000     MOVE '  GE'                TO GODK-STATUSKODER                       
215100     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-ARTC11 SSA1                   
215200     MOVE ARTC-STATUS-CODE      TO STATUS-WS                              
215300     PERFORM IMS-STATUSKONTROLL                                           
215400     .                                                                    
215500     SKIP2                                                                
215600 IMS-GHU-ARTC11 SECTION.                                                  
215700                                                                          
215800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
215900            DELIMITED BY SIZE INTO SSA1                                   
216000     MOVE 'WLARTC11 '           TO SSA2                                   
216100     MOVE '  GE'                TO GODK-STATUSKODER                       
216200     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-ARTC11 SSA1 SSA2              
216300     MOVE ARTC-STATUS-CODE      TO STATUS-WS                              
216400     PERFORM IMS-STATUSKONTROLL                                           
216500     .                                                                    
216600     EJECT                                                                
216700 IMS-REPLACE-ARTC11 SECTION.                                              
216800                                                                          
216900     ADD +1                     TO CHKP-ANT                               
217000     MOVE '  '                  TO GODK-STATUSKODER                       
217100     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-ARTC11                       
217200     MOVE ARTC-STATUS-CODE      TO STATUS-WS                              
217300     PERFORM IMS-STATUSKONTROLL                                           
217400     .                                                                    
217500     EJECT                                                                
217600 IMS-GET-ORDQ01 SECTION.                                                  
217700                                                                          
217800     STRING 'WLORDQ01(WDA5A1KY >' W5-WDA5A1KY-X                           
217900                    '&WDA5A1KY <' W6-WDA5A1KY-X                           
218000                    '&KDSTARAD =' W-KDSTARAD-X ')'                        
218100            DELIMITED BY SIZE INTO SSA1                                   
218200     MOVE '  GE'                TO GODK-STATUSKODER                       
218300     CALL CBLTDLI USING GN ORDQ-PCB DLI-IO-ORDQ01 SSA1                    
218400     MOVE ORDQ-STATUS-CODE      TO STATUS-WS                              
218500     PERFORM IMS-STATUSKONTROLL                                           
218600     .                                                                    
218700     SKIP3                                                                
218800 IMS-GET-ORDQ-STPOS-A1 SECTION.                                           
218900                                                                          
219000     STRING 'WLORDQ01(WDA5A1KY =' W5-WDA5A1KY-X ')'                       
219100            DELIMITED BY SIZE INTO SSA1                                   
219200     MOVE '  GE'                TO GODK-STATUSKODER                       
219300     CALL CBLTDLI USING GU ORDQ-PCB DLI-IO-ORDQ01 SSA1                    
219400     MOVE ORDQ-STATUS-CODE      TO STATUS-WS                              
219500     PERFORM IMS-STATUSKONTROLL                                           
219600     .                                                                    
219700     EJECT                                                                
219800 IMS-GET-ORDP-STPOS-ASEQ SECTION.                                         
219900                                                                          
220000     STRING 'WLORDP01(WDA5ASEQ =' W1-WDA5ASEQ-X ')'                       
220100            DELIMITED BY SIZE INTO SSA1                                   
220200     MOVE '  GE'              TO GODK-STATUSKODER                         
220300     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-ORDP01 SSA1                    
220400     MOVE ORDP-STATUS-CODE      TO STATUS-WS                              
220500     PERFORM IMS-STATUSKONTROLL                                           
220600     .                                                                    
220700     SKIP3                                                                
220800 IMS-GHN-ORDQ01-ORDP01-01 SECTION.                                        
220900                                                                          
221000     STRING 'WLORDP01(WDA5ASEQ =' W1-WDA5ASEQ-X                           
221100                    '&KDSTARAD =' W-KDSTARAD-X ')'                        
221200            DELIMITED BY SIZE INTO SSA1                                   
221300     MOVE '  GE'              TO GODK-STATUSKODER                         
221400     CALL CBLTDLI USING GHN ORDP-PCB DLI-IO-ORDP01 SSA1                   
221500     MOVE ORDP-STATUS-CODE      TO STATUS-WS                              
221600     PERFORM IMS-STATUSKONTROLL                                           
221700     .                                                                    
221800     SKIP3                                                                
221900 IMS-GHN-ORDQ01-ORDP01-02 SECTION.                                        
222000                                                                          
222100     STRING 'WLORDP01(WDA5ASEQ >' W1-WDA5ASEQ-X                           
222200                    '&WDA5ASEQ <' W2-WDA5ASEQ-X                           
222300                    '&KDSTARAD =' W-KDSTARAD-X ')'                        
222400            DELIMITED BY SIZE INTO SSA1                                   
222500     MOVE '  GE'              TO GODK-STATUSKODER                         
222600     CALL CBLTDLI USING GHN ORDP-PCB DLI-IO-ORDP01 SSA1                   
222700     MOVE ORDP-STATUS-CODE      TO STATUS-WS                              
222800     PERFORM IMS-STATUSKONTROLL                                           
222900     .                                                                    
223000     EJECT                                                                
223100 IMS-INSERT-ORDP01 SECTION.                                               
223200                                                                          
223300     ADD +1                     TO CHKP-ANT                               
223400     MOVE 'WLORDP01 '           TO SSA1                                   
223500     MOVE '  II'                TO GODK-STATUSKODER                       
223600     CALL CBLTDLI USING ISRT  ORDP2-PCB DLI-IO-ORDP01 SSA1                
223700     MOVE ORDP2-STATUS-CODE     TO STATUS-WS                              
223800     PERFORM IMS-STATUSKONTROLL                                           
223900     .                                                                    
224000     SKIP3                                                                
224100 IMS-REPLACE-ORDQ01-ORDP01 SECTION.                                       
224200                                                                          
224300     ADD +1                  TO CHKP-ANT                                  
224400     MOVE '  '               TO GODK-STATUSKODER                          
224500     CALL CBLTDLI USING REPL  ORDP-PCB DLI-IO-ORDP01                      
224600     MOVE ORDP-STATUS-CODE   TO STATUS-WS                                 
224700     PERFORM IMS-STATUSKONTROLL                                           
224800     .                                                                    
224900     EJECT                                                                
225000 IMS-GHU-ORDP01-ORDP1 SECTION.                                            
225100                                                                          
225200     STRING 'WLORDP01(WDA501KY =' W3-WDA501KY-X ')'                       
225300            DELIMITED BY SIZE INTO SSA1                                   
225400     MOVE '  '                  TO GODK-STATUSKODER                       
225500     CALL CBLTDLI USING GHU ORDP1-PCB DLI-IO-ORDP01 SSA1                  
225600     MOVE ORDP1-STATUS-CODE     TO STATUS-WS                              
225700     PERFORM IMS-STATUSKONTROLL                                           
225800     .                                                                    
225900     SKIP3                                                                
226000 IMS-DELETE-ORDP01 SECTION.                                               
226100                                                                          
226200     ADD +1                  TO CHKP-ANT                                  
226300     MOVE '  '               TO GODK-STATUSKODER                          
226400     CALL CBLTDLI USING DLET  ORDP1-PCB DLI-IO-ORDP01                     
226500     MOVE ORDP1-STATUS-CODE  TO STATUS-WS                                 
226600     PERFORM IMS-STATUSKONTROLL                                           
226700     .                                                                    
226800     EJECT                                                                
226900 IMS-GHU-ORDP01-ORDP2 SECTION.                                            
227000                                                                          
227100     STRING 'WLORDP01(WDA501KY=>' W3-WDA501KY-X                           
227200                    '&WDA501KY=<' W4-WDA501KY-X                           
227300                    '&KDSTARAD =' W-KDSTARAD-X ')'                        
227400            DELIMITED BY SIZE INTO SSA1                                   
227500     MOVE '  GE'                TO GODK-STATUSKODER                       
227600     CALL CBLTDLI USING GHU ORDP2-PCB DLI-IO-ORDP01-RES SSA1              
227700     MOVE ORDP2-STATUS-CODE     TO STATUS-WS                              
227800     PERFORM IMS-STATUSKONTROLL                                           
227900     .                                                                    
228000     SKIP3                                                                
228100 IMS-GHN-ORDP01-ORDP2 SECTION.                                            
228200                                                                          
228300     STRING 'WLORDP01(WDA501KY=>' W3-WDA501KY-X                           
228400                    '&WDA501KY=<' W4-WDA501KY-X                           
228500                    '&KDSTARAD =' W-KDSTARAD-X ')'                        
228600            DELIMITED BY SIZE INTO SSA1                                   
228700     MOVE '  GE'                TO GODK-STATUSKODER                       
228800     CALL CBLTDLI USING GHN ORDP2-PCB DLI-IO-ORDP01-RES SSA1              
228900     MOVE ORDP2-STATUS-CODE     TO STATUS-WS                              
229000     PERFORM IMS-STATUSKONTROLL                                           
229100     .                                                                    
229200     SKIP3                                                                
229300 IMS-REPLACE-ORDP01-ORDP2 SECTION.                                        
229400                                                                          
229500     ADD +1                  TO CHKP-ANT                                  
229600     MOVE '  '               TO GODK-STATUSKODER                          
229700     CALL CBLTDLI USING REPL  ORDP2-PCB DLI-IO-ORDP01-RES                 
229800     MOVE ORDP2-STATUS-CODE  TO STATUS-WS                                 
229900     PERFORM IMS-STATUSKONTROLL                                           
230000     .                                                                    
230100     EJECT                                                                
230200 IMS-GU-WDB201 SECTION.                                                   
230300                                                                          
230400     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
230500            DELIMITED BY SIZE INTO SSA1                                   
230600                                                                          
230700     MOVE '    ' TO GODK-STATUSKODER                                      
230800     CALL CBLTDLI USING                                                   
230900           GU WDB2-PCB DLI-IO-WDB2 SSA1                                   
231000     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
231100     PERFORM IMS-STATUSKONTROLL                                           
231200     .                                                                    
231300 IMS-GU-WDK701 SECTION.                                                   
231400     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-X ')'                         
231500          DELIMITED BY SIZE INTO SSA1                                     
231600     MOVE '  GE' TO GODK-STATUSKODER                                      
231700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701  SSA1                   
231800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
231900     PERFORM IMS-STATUSKONTROLL                                           
232000     .                                                                    
232100     SKIP3                                                                
232200 IMS-GNP-WDK711 SECTION.                                                  
232300     MOVE 'WDK711   ' TO SSA1                                             
232400     MOVE '  GE' TO GODK-STATUSKODER                                      
232500     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711  SSA1                  
232600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
232700     PERFORM IMS-STATUSKONTROLL                                           
232800     .                                                                    
232900 IMS-RESTART SECTION.                                                     
233000                                                                          
233100     MOVE SPACE TO MSG-IO-AREA-1                                          
233200     MOVE '  '  TO GODK-STATUSKODER                                       
233300     CALL CBLTDLI USING XRST MSG-PCB                                      
233400                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
233500                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
233600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
233700     PERFORM IMS-STATUSKONTROLL                                           
233800     IF IMS-EJ-OK                                                         
233900        CALL FELLOG                                                       
234000     END-IF                                                               
234100     .                                                                    
234200     SKIP1                                                                
234300 IMS-CHECKPOINT SECTION.                                                  
234400                                                                          
234500     MOVE CHKP-ID TO MSG-IO-AREA-1                                        
234600     MOVE '  XD' TO GODK-STATUSKODER                                      
234700     CALL CBLTDLI USING CHKP MSG-PCB                                      
234800                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
234900                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
235000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
235100     PERFORM IMS-STATUSKONTROLL                                           
235200     IF IMS-EJ-OK                                                         
235300        CALL FELLOG                                                       
235400     END-IF                                                               
235500     .                                                                    
235600     SKIP1                                                                
235700 IMS-STATUSKONTROLL SECTION.                                              
235800                                                                          
235900     SET STATUS-IX TO 1                                                   
236000     SEARCH GODK-STATUS                                                   
236100       AT END                                                             
236200         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
236300         DELIMITED BY SIZE INTO FELTEXT                                   
236400         CALL FELLOG                                                      
236500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
236600         CONTINUE                                                         
236700     END-SEARCH                                                           
236800     .                                                                    
236900     -COPY WY2000P9                                                       
