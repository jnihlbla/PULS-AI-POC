000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W4029200.                                                
000500 AUTHOR.         GERRY CARMICHAEL.                                        
000600 DATE-WRITTEN.   90/09/10.                                                
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET HANTERAR ANNULLATION AV HEL ORDER.                    
001200*        PROGRAMMET ÄR ETT BACKGRUNDSPROGRAM SOM STARTAR                  
001300*        OM SIG SJÄLV EFTER 100 LÄSNINGAR/BORTTAG AV SEGMENT              
001400*        GENOM ATT LÄGGA UPP EN NY TRANS PÅ IMS-KÖN.                      
001500*                                                                         
001600*                                                                         
001700*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
001800*        PROGRAMMET UPPDATERAR WLORQI (WDQ2)                              
001900*        PROGRAMMET UPPDATERAR WLORQF (WDQ4)                              
002000*        PROGRAMMET UPPDATERAR WLARTM (WDK9)                              
002100*        PROGRAMMET UPPDATERAR WLORQM (WDQ1)                              
002200*        PROGRAMMET UPPDATERAR WLZZAC (WDG6)                              
002300*        PROGRAMMET UPPDATERAR WLORDP (WDA5)                              
002400*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
002410*        PROGRAMMET UPPDATERAR        (WDM2)                              
002420*                                                                         
002430*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002440*        PROGRAMMET STARTAR    W20109 VIA EN PPSW                         
002450*                                                                         
002460*                                                                         
002470*    INDATA.                                                              
002480*        TRANSAKTION: W4T292X                                             
002490*        MID:         W4I29201                                            
002500*                                                                         
002600*                                                                         
002700*                                                                         
002800*    UTDATA.                                                              
002900*        TRANSAKTION: W4T292X                                             
003000*                     W2T109X                                             
003100*        MID        : W4I29201                                            
003200*                     W2I10902                                            
003300*                                                                         
003400* CHANGE LOG:                                                             
003500*                                                                         
003600* ETRACK 1290414 INTERVALL FOR DISTRICT AND CUSTOMER                      
003700* HÖSTEN 2004 GÖRAN KJELLSON                                              
003800* ETRACKER 887753                                                         
003900* ETRACKER 7450328  2008-HÖST  VOHF                                       
004000* ETRACKER 10254592 2015       DECOMISSION VOHF                           
004100* ETRACKER 10228562 2016 ÄNDRAT FRÅN WLXXKR/XXKT/XXKS TILL WDM2           
004200                                                                          
004300     SKIP3                                                                
004400 ENVIRONMENT DIVISION.                                                    
004500     EJECT                                                                
004600 DATA DIVISION.                                                           
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900*    -- CHECKED BY WY2000                                                 
005000                                                                          
005100 77  IDPGM                       PIC X(08)  VALUE 'W4029200'.             
005200 77  FELTEXT                     PIC X(32)  VALUE SPACE.                  
005300 77  JA                          PIC X      VALUE 'J'.                    
005400 77  NEJ                         PIC X      VALUE 'N'.                    
005500 77  AVSR-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
005600 77  MAX-AVSR-INDX               PIC S9(4)  VALUE +100  COMP SYNC.        
005700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005800 77  DC-IX                       PIC S9(4)  VALUE +0    COMP SYNC.        
005900 77  ANTAL-LAEST                 PIC S9(4)  VALUE +0    COMP SYNC.        
006000 77  MAX-ANTAL-LAEST             PIC S9(4)  VALUE +99   COMP SYNC.        
006100 77  MAX-2109-IX                 PIC S9(4)  VALUE +18   COMP SYNC.        
006200 77  2109-IX                     PIC S9(4)  COMP SYNC.                    
006300 77  IX-CD-OMR                   PIC S9(9)  VALUE ZERO  COMP-3.           
006400 77  WS-TINUDAT                  PIC S9(7)  COMP-3.                       
006500 77  WS-TINUTID                  PIC S9(9)  COMP-3.                       
006600                                                                          
006700*01  -COPY WWDCKONS                                                       
006800                                                                          
006900*01  -COPY WWBYT03                                                        
007000                                                                          
007100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007200 01  WS-TIHHMMSS                 PIC 9(6)    VALUE ZERO.                  
007300 01  FILLER REDEFINES WS-TIHHMMSS.                                        
007400     03 WS-TIHHMM                PIC 9(4).                                
007500     03 FILLER                   PIC 9(2).                                
007600                                                                          
007700 77  WS-9KOMPL                   PIC 9(9)   VALUE 999999999.              
007800 01  WS-9KOMPL-DATUM             PIC 9(8).                                
007900 01  FILLER REDEFINES WS-9KOMPL-DATUM.                                    
008000     03 WS-CENTURY               PIC 9(2).                                
008100     03 WS-AAMMDD                PIC 9(6).                                
008200 77  WS-IDTRANS                  PIC X(4).                                
008300 77  WS-IDDISTR                  PIC 9(4)   VALUE ZERO.                   
008400 77  WS-IDKUNDNR                 PIC 9(6)   VALUE ZERO.                   
008500 77  WS-IDORDER                  PIC 9(7)   VALUE ZERO.                   
008600 77  WS-IDARTNR                  PIC 9(9)   VALUE ZERO.                   
008700 77  WS-IDLOPNR                  PIC 9(3)   VALUE ZERO.                   
008800 77  WS-IDSEKVNR                 PIC 9(3)   VALUE ZERO.                   
008900 77  WS-ADLAGOMR                 PIC 9(3)   VALUE ZERO.                   
009000 77  WS-ADGANG                   PIC 9(3)   VALUE ZERO.                   
009100 77  WS-ADPLATS                  PIC 9(5)   VALUE ZERO.                   
009200 77  WS-TIDISPIN                 PIC 9(7)   VALUE ZERO.                   
009300 77  ANNULLERAT-ANTAL            PIC 9(6)   VALUE ZERO.                   
009400 77  VORDIFF                     PIC S9(7)  VALUE +0    COMP-3.           
009500 77  WS-KVQPACK-1                PIC S9(5)  VALUE +0    COMP-3.           
009600 77  WS-ANTOBKR                  PIC S9(3)  VALUE +0    COMP-3.           
009700                                                                          
009800 01  WS-IDKUNDRF                 PIC X(10)  VALUE SPACE.                  
009900 01  WS-IDORDNR7-FILLER REDEFINES WS-IDKUNDRF.                            
010000     03 WS-IDORDNR7              PIC 9(7).                                
010100     03 FILLER                   PIC X(3).                                
010200                                                                          
010300 01  TABELL.                                                              
010400     03 KDFRAKT-TABELL OCCURS 9.                                          
010500        05 WS-KDFRAKT            PIC 9(3)   VALUE ZERO.                   
010600                                                                          
010700 77  SPAR-KDORDBEK               PIC 9(2)   VALUE ZERO.                   
010800                                                                          
010900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
011000     88  ALLT-OK                             VALUE 'J'.                   
011100                                                                          
011200 77  FORTSAETT-SW                PIC X       VALUE 'J'.                   
011300     88  FORTSAETT                           VALUE 'J'.                   
011400                                                                          
011500 77  AVSR-SW                     PIC X       VALUE 'J'.                   
011600     88  AVSR-OK                             VALUE 'J'.                   
011700                                                                          
011800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011900     88  EGEN-MID                            VALUE '4292'.                
012000     88  GODK-MID                            VALUE '4204' '4213'          
012100                                                   '4223' '4233'          
012200                                                   '4243' '4245'          
012300                                                   '4292'.                
012400 01 DB2-LASNING.                                                          
012500     03 FILLER                   PIC X(16)   VALUE                        
012600                                             'WS-DB2-SEKTION'.            
012700     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
012800                                                                          
012900     EJECT                                                                
013000 01 NYCKLAR-TP4TRAN.                                                      
013100     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
013200                                                                          
013300     EJECT                                                                
013400*    --- ARBETSAREA FÖR BESTÄMNING AV LAGER                               
013500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013600 01  GENERELLA-SUBPROGRAM.                                                
013700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
014100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014200                                                                          
014300 01  GEMENSAMMA-SUBPROGRAM.                                               
014400     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
014500     03  W413AVSO                PIC X(8)    VALUE 'W413AVSO'.            
014600     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
014700*        PRISFRÅGA                                                        
014800                                                                          
014900*    --- PARAMETRAR TILL ABEND                                            
015000 01  RKOD-ABEND                  PIC S9(4)   VALUE +33  COMP SYNC.        
015100     EJECT                                                                
015200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015300*   -COPY WMSGINIT                                                        
015400     EJECT                                                                
015500*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
015600*   -COPY WDATAREA                                                        
015700     EJECT                                                                
015800*    --- AREOR FÖR GEMENSAMMA-SUBPROGRAM                                  
015900*                                                                         
016000 01  FILLER                      PIC X(16)   VALUE 'W413AVSR'.            
016100*01  -COPY W413AVSR                                                       
016200     EJECT                                                                
016300 01  FILLER                      PIC X(16)   VALUE 'W413AVSO'.            
016400*01  -COPY W413AVSO                                                       
016500     EJECT                                                                
016600 01  FILLER                      PIC X(8)    VALUE 'W335PRQU'.            
016700*    -COPY W335PRQU                                                       
016800     EJECT                                                                
016900 01  TEST-IDDISTR              PIC S9(5) COMP-3.                          
017000*01  FILLER -COPY WWDIST18 -RED TEST-IDDISTR.                             
017100     EJECT                                                                
017200*01  FILLER -COPY WWDIST35 -RED TEST-IDDISTR.                             
017300     EJECT                                                                
017400*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
017500     EJECT                                                                
017600 01  FILLER                 PIC X(16)   VALUE 'DIST-DC-TAB'.              
017700     -COPY WWDIST57                                                       
017800     EJECT                                                                
017900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
018000*                                                                         
018100 01  FILLER                 PIC X(16)   VALUE 'MID-AREA'.                 
018200     SKIP3                                                                
018300*01  -COPY W4I29201                                                       
018400     EJECT                                                                
018500 01  FILLER                 PIC X(16)  VALUE 'MSG/MID/MOD-AREA'.          
018600     SKIP3                                                                
018700*01  -COPY WMSGAREA                                                       
018800     EJECT                                                                
018900******************************************************************        
019000*    MID-AREA FÖR W2T109                                         *        
019100******************************************************************        
019200*01  -COPY  W2I10902  -PRE 2109-                                          
019300     EJECT                                                                
019400 01  FILLER                 PIC X(16)   VALUE 'MFS-AREA'.                 
019500     SKIP3                                                                
019600*01  -COPY WMFSAREA                                                       
019700     EJECT                                                                
019800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019900*                                                                         
020000 01  FILLER                       PIC X(16)   VALUE 'IMS-WS'.             
020100     SKIP3                                                                
020200 01  NYCKLAR-TILL-DLI.                                                    
020300*    ---------TILL WDQ101                                                 
020400     03  W-WDQ101KY-MIN-X.                                                
020500         05  W-OBKR-IDORDER-MIN   PIC S9(7)   VALUE ZERO COMP-3.          
020600         05  W-OBKR-IDARTNR-MIN   PIC S9(9)   VALUE ZERO COMP-3.          
020700         05  W-OBKR-IDLOPNR-MIN   PIC S9(3)   VALUE ZERO COMP-3.          
020800         05  W-OBKR-IDSEKVNR-MIN  PIC S9(3)   VALUE ZERO COMP-3.          
020900         05  W-OBKR-IDDC-MIN      PIC  X(2)   VALUE SPACE.                
021000         05  W-OBKR-KDORDBEK-MIN  PIC 9(2)    VALUE ZERO.                 
021100     03  W-WDQ101KY-MAX-X.                                                
021200         05  W-OBKR-IDORDER-MAX   PIC S9(7)   VALUE ZERO COMP-3.          
021300         05  W-OBKR-IDARTNR-MAX   PIC S9(9)   VALUE ZERO COMP-3.          
021400         05  W-OBKR-IDLOPNR-MAX   PIC S9(3)   VALUE ZERO COMP-3.          
021500         05  W-OBKR-IDSEKVNR-MAX  PIC S9(3)   VALUE ZERO COMP-3.          
021600         05  W-OBKR-IDDC-MAX      PIC  X(2)   VALUE SPACE.                
021700         05  W-OBKR-KDORDBEK-MAX  PIC 9(2)    VALUE ZERO.                 
021800*    ---------TILL WDQ201                                                 
021900     03  W-IDORDER-X.                                                     
022000         05  W-OHUV-IDORDER       PIC S9(7)   VALUE ZERO COMP-3.          
022100*    ---------TILL WDQ213                                                 
022200     03  W-IDGMTREF-X.                                                    
022300         05  W-REF-IDDISTR       PIC S9(5)    VALUE ZERO COMP-3.          
022400         05  W-REF-IDKUNDNR      PIC S9(7)    VALUE ZERO COMP-3.          
022500         05  W-REF-IDKUNDRF      PIC X(10)    VALUE SPACE.                
022600*    ---------TILL WDQ401                                                 
022700     03  W-WDQ401KY-MIN-X.                                                
022800         05  W-ORAD-IDORDER-MIN   PIC S9(7)    VALUE ZERO COMP-3.         
022900         05  W-ORAD-IDDC-MIN      PIC  X(2)    VALUE SPACE.               
023000         05  W-ORAD-ADLAGOMR-MIN  PIC S9(3)    VALUE ZERO COMP-3.         
023100         05  W-ORAD-ADGANG-MIN    PIC S9(3)    VALUE ZERO COMP-3.         
023200         05  W-ORAD-ADPLATS-MIN   PIC S9(5)    VALUE ZERO COMP-3.         
023300         05  W-ORAD-IDARTNR-MIN   PIC S9(9)    VALUE ZERO COMP-3.         
023400         05  W-ORAD-IDLOPNR-MIN   PIC S9(3)    VALUE ZERO COMP-3.         
023500     03  W-WDQ401KY-MAX-X.                                                
023600         05  W-ORAD-IDORDER-MAX   PIC S9(7)    VALUE ZERO COMP-3.         
023700         05  W-ORAD-IDDC-MAX      PIC  X(2)    VALUE SPACE.               
023800         05  W-ORAD-ADLAGOMR-MAX  PIC S9(3)    VALUE ZERO COMP-3.         
023900         05  W-ORAD-ADGANG-MAX    PIC S9(3)    VALUE ZERO COMP-3.         
024000         05  W-ORAD-ADPLATS-MAX   PIC S9(5)    VALUE ZERO COMP-3.         
024100         05  W-ORAD-IDARTNR-MAX   PIC S9(9)    VALUE ZERO COMP-3.         
024200         05  W-ORAD-IDLOPNR-MAX   PIC S9(3)    VALUE ZERO COMP-3.         
024300*    ---------TILL WDK901,WDK601,WDK701                                   
024400     03  W-IDARTNR-X.                                                     
024500         05  W-IDARTNR               PIC S9(9)  VALUE ZERO COMP-3.        
024600     03  W-IDDC-X.                                                        
024700         05  W-IDDC                  PIC  X(2)  VALUE SPACE.              
024800*    ---------TILL WDR480                                                 
024900     03  W-IDHTYP-X.                                                      
025000         05  W-IDHTYP                PIC X(4)   VALUE '4541'.             
025100         05  FILLER                  PIC X(26)  VALUE LOW-VALUE.          
025200     03  W-KDVORATG-X.                                                    
025300         05  W-KDVORATG              PIC X      VALUE '2'.                
025400     03  W-WDGXKEY-MIN-X.                                                 
025500         05  W-4542-IDDISTR-MIN      PIC S9(5)  VALUE ZERO COMP-3.        
025600         05  W-4542-IDANSK-MIN       PIC S9(3)  VALUE ZERO COMP-3.        
025700         05  W-4542-IDARTNR-MIN      PIC S9(9)  VALUE ZERO COMP-3.        
025800         05  W-4542-IDLOPNR-MIN      PIC S9(3)  VALUE ZERO COMP-3.        
025900         05  W-4542-IDORDER-MIN      PIC S9(7)  VALUE ZERO COMP-3.        
026000     03  W-WDGXKEY-MAX-X.                                                 
026100         05  W-4542-IDDISTR-MAX      PIC S9(5)  VALUE ZERO COMP-3.        
026200         05  W-4542-IDANSK-MAX       PIC S9(3)  VALUE ZERO COMP-3.        
026300         05  W-4542-IDARTNR-MAX      PIC S9(9)  VALUE ZERO COMP-3.        
026400         05  W-4542-IDLOPNR-MAX      PIC S9(3)  VALUE ZERO COMP-3.        
026500         05  W-4542-IDORDER-MAX      PIC S9(7)  VALUE ZERO COMP-3.        
026600     SKIP2                                                                
026700   03    W-WDA601KY-MIN-X.                                                
026800     05    W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
026900     05    W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
027000     05    W-A601KY-MIN-IDKUNDRF     PIC X(10) VALUE SPACE.               
027100     05    W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
027200     05    W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
027300     05    W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
027400     05    W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
027500     05    W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
027600     SKIP2                                                                
027700   03    W-WDA601KY-MAX-X.                                                
027800     05    W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
027900     05    W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
028000     05    W-A601KY-MAX-IDKUNDRF     PIC X(10) VALUE SPACE.               
028100     05    W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE ZERO COMP-3.         
028200     05    W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
028300     05    W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE ZERO COMP-3.         
028400     05    W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE ZERO COMP-3.         
028500     05    W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE ZERO COMP-3.         
028600*    ---------TILL WDA501                                                 
028700     03  W-WDA501KY-X.                                                    
028800         05  W-RAD-IDDISTR           PIC S9(5)    COMP-3.                 
028900         05  W-RAD-IDKUNDNR          PIC S9(7)    COMP-3.                 
029000         05  W-RAD-IDKUNDRF          PIC X(10).                           
029100         05  W-RAD-IDARTNR           PIC S9(9)    COMP-3.                 
029200         05  W-RAD-IDLOPNR           PIC S9(3)    COMP-3.                 
029300     03    W-RAD-IDDC-X.                                                  
029400         05  W-RAD-IDDC              PIC X(2).                            
029500                                                                          
029600*    ---------TILL WDM2                                                   
029700     03  W-WDM201-X.                                                      
029800         05  W-KAMP-IDKAMPRF     PIC S9(07)   VALUE ZERO COMP-3.          
029810         05  W-KAMP-IDDC         PIC X(02)    VALUE SPACE.                
029820                                                                          
029830     03  W-WDM211-X.                                                      
029840         05  W-KART-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
029850                                                                          
029860     03  W-WDM221-X.                                                      
029870         05  W-KMRK-IDDISTR-FOM   PIC S9(05) VALUE ZERO COMP-3.           
029880         05  W-KMRK-IDDISTR-TOM   PIC S9(05) VALUE ZERO COMP-3.           
029890         05  W-KMRK-IDKUNDNR-FOM  PIC S9(07) VALUE ZERO COMP-3.           
029900         05  W-KMRK-IDKUNDNR-TOM  PIC S9(07) VALUE ZERO COMP-3.           
030000                                                                          
030100*    ---------TILL WDB6                                                   
030200     03  W-IDDC-B6-X.                                                     
030300         05 W-IDDC-B6                  PIC X(2).                          
030400                                                                          
030500*    --- STATUS-KOD FRÅN IMS                                              
030600 01  STATUS-WS                   PIC XX.                                  
030700     88  SEGMENT-FINNS                       VALUE '  '.                  
030800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
030900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
031000     88  BASEN-SLUT                          VALUE 'GB'.                  
031100     SKIP2                                                                
031200 01  GODK-STATUSKODER.                                                    
031300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031400     SKIP3                                                                
031500 01  SSA1                        PIC X(160).                              
031600 01  SSA2                        PIC X(128).                              
031700 01  SSA3                        PIC X(128).                              
031800     EJECT                                                                
031900******************************************************                    
032000*    ARBETSAREA FÖR RYB-TRANS TILL WDG6              *                    
032100******************************************************                    
032200 01  W-RYBPOST.                                                           
032300*    03  -COPY WDGZRYB    -PRE W-                                         
032400     EJECT                                                                
032500******************************************************                    
032600*    ARBETSAREA FÖR RYC-TRANS TILL WDG6              *                    
032700******************************************************                    
032800 01  W-RYCPOST.                                                           
032900*    03  -COPY WDGZRYC   -PRE W-                                          
033000     EJECT                                                                
033100 01  W-RYCSPOST.                                                          
033200*    03  -COPY WDGZRYCS  -PRE W-                                          
033300     EJECT                                                                
033400*                            DB2 FUNKTIONSKODER                           
033500 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
033600       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
033700                                                                          
033800 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
033900 01  DB2-WS.                                                              
034000     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
034100         88  CURSOR-OK                       VALUE 000.                   
034200         88  RADER-FINNS                     VALUE 000.                   
034300         88  RADER-SAKNAS                    VALUE 100.                   
034400         88  ATKOMST-FEL                     VALUE 904.                   
034500     03  GODK-SQLCODEKODER.                                               
034600         05  GODK-SQLCODE OCCURS 5                                        
034700             INDEXED BY SQLCODE-IX PIC 9(3).                              
034800 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
034900     EJECT                                                                
035000*    --- IMS FUNKTIONSKODER                                               
035100*01  -COPY W0003                                                          
035200     EJECT                                                                
035300*    ---  DLI INPUT-OUTPUT AREA                                           
035400     SKIP3                                                                
035500 01  FILLER                      PIC X(16)   VALUE 'IO-AREA-OHUV'.        
035600 01  DLI-IO-AREA-OHUV.                                                    
035700     03  WLORQI01.                                                        
035800*        05  -COPY WDQ201                                                 
035900     EJECT                                                                
036000                                                                          
036100 01  FILLER                      PIC X(16)   VALUE 'IO-AREA-ARB '.        
036200 01  DLI-IO-AREA-ARB.                                                     
036300     03  WLORQI12.                                                        
036400*        05  -COPY WDQ212                                                 
036500     EJECT                                                                
036600                                                                          
036700 01  FILLER                      PIC X(16)   VALUE 'IO-AREA-ORAD'.        
036800 01  DLI-IO-AREA-ORAD.                                                    
036900     03  WLORQF01.                                                        
037000*        05  -COPY WDQ401                                                 
037100     EJECT                                                                
037200                                                                          
037300 01  FILLER                      PIC X(16)   VALUE 'IO-AREA-ARTM'.        
037400 01  DLI-IO-AREA-ARTM.                                                    
037500     03  WLARTM01.                                                        
037600*        05  -COPY WDK901                                                 
037700     EJECT                                                                
037800                                                                          
037900 01  FILLER                      PIC X(16)   VALUE 'IO-AREA-ARTC'.        
038000 01  DLI-IO-AREA-ARTC11.                                                  
038100     03  WLARTC11.                                                        
038200*        05  -COPY WDK611                                                 
038300     EJECT                                                                
038400                                                                          
038500 01  FILLER                      PIC X(16)   VALUE 'IO-AREA-ARTS'.        
038600 01  DLI-IO-AREA-ARTS.                                                    
038700     03  WLARTS11.                                                        
038800*        05  -COPY WDK711                                                 
038900     EJECT                                                                
039000                                                                          
039100 01  FILLER                      PIC X(16)   VALUE 'IO-AREA-ZZAC'.        
039200 01  DLI-IO-AREA-ZZAC.                                                    
039300     03  WLZZAC01.                                                        
039400*        05  -COPY WDGZ01                                                 
039500     EJECT                                                                
039600                                                                          
039700 01  FILLER                      PIC X(16)   VALUE 'IO-AREA-OBKR'.        
039800 01  DLI-IO-AREA-OBKR.                                                    
039900     03  WLORQM01.                                                        
040000*        05  -COPY WDQ101                                                 
040100     EJECT                                                                
040200                                                                          
040300 01  FILLER                      PIC X(16)   VALUE 'IO-AREA-4542'.        
040400 01  DLI-IO-AREA-4542.                                                    
040500     03  WL454111.                                                        
040600*        05  -COPY WDGX4542                                               
040700     EJECT                                                                
040800 01  FILLER                      PIC X(16)   VALUE 'A601-AREA'.           
040900 01  DLI-IO-AREA-WDA6.                                                    
041000*  03    -COPY WDA601                                                     
041100     EJECT                                                                
041200                                                                          
041300 01  FILLER                      PIC X(16)   VALUE 'IO-AREA-ORDP'.        
041400 01  DLI-IO-AREA-ORDP.                                                    
041500     03  WLORDP01.                                                        
041600*        05  -COPY WDA501                                                 
041700     EJECT                                                                
041800                                                                          
041900 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
042000 01  DLI-IO-WDM211.                                                       
042100*    03 -COPY WDM211                                                      
042200                                                                          
042300     EJECT                                                                
042400 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
042500 01  DLI-IO-WDM221.                                                       
042600*    03 -COPY WDM221                                                      
042700     EJECT                                                                
042800                                                                          
042900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
043000 01   DLI-IO-AREA-B601.                                                   
043100*     03  -COPY WDB601                                                    
043200     EJECT                                                                
043300 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
043400                                                                          
043500*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
043600     EJECT                                                                
043700     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
043800     EJECT                                                                
043900 LINKAGE SECTION.                                                         
044000                                                                          
044100*01  -COPY W0009      -PRE MSG-                                           
044200     EJECT                                                                
044300*01  -COPY W0009      -PRE ALT1-                                          
044400     EJECT                                                                
044500*01  -COPY W0009      -PRE 2109-                                          
044600     EJECT                                                                
044700 01  AVSR-ALT-PCB                PIC X.                                   
044800     EJECT                                                                
044900*01  -COPY W0008      -PRE USEA-                                          
045000     05  FILLER                  PIC X.                                   
045100     EJECT                                                                
045200*01  -COPY W0008      -PRE ORQI-                                          
045300     05  FILLER                  PIC X.                                   
045400     EJECT                                                                
045500*01  -COPY W0008      -PRE ORQF-                                          
045600     05  FILLER                  PIC X.                                   
045700     EJECT                                                                
045800*01  -COPY W0008      -PRE ARTC-                                          
045900     05  FILLER                  PIC X.                                   
046000     EJECT                                                                
046100*01  -COPY W0008      -PRE ARTS-                                          
046200     05  FILLER                  PIC X.                                   
046300     EJECT                                                                
046400*01  -COPY W0008      -PRE ARTM-                                          
046500     05  FILLER                  PIC X.                                   
046600     EJECT                                                                
046700*01  -COPY W0008      -PRE ZZAC-                                          
046800     05  FILLER                  PIC X.                                   
046900     EJECT                                                                
047000*01  -COPY W0008      -PRE ORQM-                                          
047100     05  FILLER                  PIC X.                                   
047200     EJECT                                                                
047300*01  -COPY W0008      -PRE ORDP-                                          
047400     05  FILLER                  PIC X.                                   
047500     EJECT                                                                
047600*01  -COPY W0008      -PRE 4541-                                          
047700     05  FILLER                  PIC X.                                   
047800     EJECT                                                                
047900*01  -COPY W0008      -PRE WDM2-                                          
048000     05  FILLER                  PIC X.                                   
048100     EJECT                                                                
048200*01    -COPY W0008     -PRE WDA6A-                                        
048300     05  FILLER                  PIC X.                                   
048400     EJECT                                                                
048500*01    -COPY W0008     -PRE WDA6B-                                        
048600     05  FILLER                  PIC X.                                   
048700     EJECT                                                                
048800*01    -COPY W0008     -PRE WDB6-                                         
048900     05  FILLER                  PIC X.                                   
049000     EJECT                                                                
049100*----> SUBPROGRAM W413AVSR.                                               
049200 01  AVSR-ORQI-PCB               PIC X.                                   
049300 01  AVSR-GMTB-PCB               PIC X.                                   
049400 01  AVSR-GMTC-PCB               PIC X.                                   
049500 01  AVSR-WDB2-PCB               PIC X.                                   
049600 01  AVSR-WDB6-PCB               PIC X.                                   
049700     EJECT                                                                
049800*----> SUBPROGRAM W413AVSO.                                               
049900 01  AVSO-WDE6-PCB               PIC X.                                   
050000 01  AVSO-ORQA-PCB               PIC X.                                   
050100 01  AVSO-WDQ2-PCB               PIC X.                                   
050200 01  AVSO-GMTB-PCB               PIC X.                                   
050300 01  AVSO-XXKA-PCB               PIC X.                                   
050400 01  AVSO-4437-PCB               PIC X.                                   
050500 01  AVSO-XXKE-PCB               PIC X.                                   
050600 01  AVSO-XXKF-PCB               PIC X.                                   
050700 01  AVSO-XXKG-PCB               PIC X.                                   
050800 01  AVSO-XXKH-PCB               PIC X.                                   
050900 01  AVSO-XXKI-PCB               PIC X.                                   
051000 01  AVSO-XXKP-PCB               PIC X.                                   
051100 01  AVSO-WDB2-PCB               PIC X.                                   
051200 01  AVSO-WDB6-PCB               PIC X.                                   
051300 01  AVSO-WDP7-PCB               PIC X.                                   
051400 01  TRAN-XXKB-PCB               PIC X.                                   
051500 01  ORDN-ORQL-PCB               PIC X.                                   
051600 01  ORDN-PROC-PCB               PIC X.                                   
051700 01  ORDN-ORQI-PCB               PIC X.                                   
051800 01  ORDN-WDQ3-PCB               PIC X.                                   
051900                                                                          
052000 01  PRQU-WDG2-PCB               PIC X.                                   
052100 01  PRQU-WDC7-PCB               PIC X.                                   
052200 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
052300     EJECT                                                                
052400 PROCEDURE DIVISION  USING MSG-PCB  ALT1-PCB 2109-PCB AVSR-ALT-PCB        
052500                           USEA-PCB ORQI-PCB ORQF-PCB                     
052600                           ARTC-PCB ARTS-PCB ARTM-PCB                     
052700                           ZZAC-PCB ORQM-PCB                              
052800                           ORDP-PCB 4541-PCB WDM2-PCB                     
052900                           WDA6A-PCB WDA6B-PCB WDB6-PCB                   
053000                           AVSR-ORQI-PCB AVSR-GMTB-PCB                    
053100                           AVSR-GMTC-PCB                                  
053200                           AVSR-WDB2-PCB AVSR-WDB6-PCB                    
053300                           AVSO-WDE6-PCB AVSO-ORQA-PCB                    
053400                           AVSO-WDQ2-PCB                                  
053500                           AVSO-GMTB-PCB AVSO-XXKA-PCB                    
053600                           AVSO-4437-PCB AVSO-XXKE-PCB                    
053700                           AVSO-XXKF-PCB AVSO-XXKG-PCB                    
053800                           AVSO-XXKH-PCB AVSO-XXKI-PCB                    
053900                           AVSO-XXKP-PCB AVSO-WDB2-PCB                    
054000                           AVSO-WDB6-PCB                                  
054100                           AVSO-WDP7-PCB TRAN-XXKB-PCB                    
054200                           ORDN-ORQL-PCB ORDN-PROC-PCB                    
054300                           ORDN-ORQI-PCB ORDN-WDQ3-PCB                    
054400                           PRQU-WDG2-PCB                                  
054500                           PRQU-WDC7-PCB                                  
054600                           PRQU-SJKO-WDK6-PCB.                            
054700     ENTRY 'DLITCBL' USING MSG-PCB  ALT1-PCB 2109-PCB AVSR-ALT-PCB        
054800                           USEA-PCB ORQI-PCB ORQF-PCB                     
054900                           ARTC-PCB ARTS-PCB ARTM-PCB                     
055000                           ZZAC-PCB ORQM-PCB                              
055100                           ORDP-PCB 4541-PCB WDM2-PCB                     
055200                           WDA6A-PCB WDA6B-PCB WDB6-PCB                   
055300                           AVSR-ORQI-PCB AVSR-GMTB-PCB                    
055400                           AVSR-GMTC-PCB                                  
055500                           AVSR-WDB2-PCB AVSR-WDB6-PCB                    
055600                           AVSO-WDE6-PCB AVSO-ORQA-PCB                    
055700                           AVSO-WDQ2-PCB                                  
055800                           AVSO-GMTB-PCB AVSO-XXKA-PCB                    
055900                           AVSO-4437-PCB AVSO-XXKE-PCB                    
056000                           AVSO-XXKF-PCB AVSO-XXKG-PCB                    
056100                           AVSO-XXKH-PCB AVSO-XXKI-PCB                    
056200                           AVSO-XXKP-PCB AVSO-WDB2-PCB                    
056300                           AVSO-WDB6-PCB                                  
056400                           AVSO-WDP7-PCB TRAN-XXKB-PCB                    
056500                           ORDN-ORQL-PCB ORDN-PROC-PCB                    
056600                           ORDN-ORQI-PCB ORDN-WDQ3-PCB                    
056700                           PRQU-WDG2-PCB                                  
056800                           PRQU-WDC7-PCB                                  
056900                           PRQU-SJKO-WDK6-PCB.                            
057000     EJECT                                                                
057100     PERFORM IMS-GET-MSG                                                  
057200     IF SEGMENT-FINNS                                                     
057300       PERFORM A-INIT                                                     
057400       IF GODK-MID                                                        
057500         PERFORM B-KOLLA-INDATA                                           
057600         IF ALLT-OK                                                       
057700           PERFORM IMS-GHU-ORQI-ORQI01                                    
057800           IF SEGMENT-FINNS                                               
057900             IF WS-IDTRANS = '4204' OR '4245'                             
058000               PERFORM C-ANNULLERA-HEL-ORDER                              
058100             ELSE                                                         
058200               PERFORM D-ANNULLERAT-FRAN-SVARSBILDER                      
058300             END-IF                                                       
058400           END-IF                                                         
058500         END-IF                                                           
058600       END-IF                                                             
058700     END-IF                                                               
058800                                                                          
058900     IF 2109-MID2-KVANTART > ZERO                                         
059000       PERFORM S14-PPSW-2109                                              
059100     END-IF                                                               
059200                                                                          
059300     MOVE ZERO TO RETURN-CODE                                             
059400     GOBACK                                                               
059500     .                                                                    
059600     EJECT                                                                
059700 A-INIT SECTION.                                                          
059800                                                                          
059900     IF MSG-DUBBLA-TRANSKODER                                             
060000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I29201                 
060100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
060200                             WS-IDTRANS                                   
060300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
060400     ELSE                                                                 
060500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I29201                  
060600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
060700                             WS-IDTRANS                                   
060800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
060900     END-IF                                                               
061000                                                                          
061100     MOVE MFS-IDTRANS  TO  W-IDTRANS                                      
061200                                                                          
061300     MOVE LOW-VALUE TO MSG-AREA                                           
061400     MOVE SPACE                 TO 2109-MID2-W2I10902                     
061500     MOVE +1                    TO 2109-IX                                
061600     ACCEPT WS-TINUDAT FROM DATE                                          
061700     ACCEPT WS-TINUTID FROM TIME                                          
061800                                                                          
061900     PERFORM AA-NOLLA-WOPS-TABELL                                         
062000     PERFORM AB-FIXA-LOKAL-TID                                            
062100     .                                                                    
062200     EJECT                                                                
062300 AA-NOLLA-WOPS-TABELL SECTION.                                            
062400                                                                          
062500     MOVE +1                   TO AVSR-INDX                               
062600     PERFORM UNTIL AVSR-INDX > MAX-AVSR-INDX                              
062700        MOVE +0                TO AVSR-ADLAGOMR(AVSR-INDX)                
062800        MOVE SPACE             TO AVSR-IDDC(AVSR-INDX)                    
062900        MOVE SPACE             TO AVSR-IDLEVNR(AVSR-INDX)                 
063000        MOVE ZERO              TO AVSR-KDSPEEMB(AVSR-INDX)                
063100        MOVE +0                TO AVSR-KVANNANT(AVSR-INDX)                
063200        MOVE +0                TO AVSR-KVBEART-Q(AVSR-INDX)               
063300        MOVE +0                TO AVSR-PRARTNTO(AVSR-INDX)                
063400        MOVE +0                TO AVSR-PRAVCOST(AVSR-INDX)                
063500        INITIALIZE             AVSR-DEAL-PR-LINE(AVSR-INDX)               
063600        MOVE +0                TO AVSR-VKART(AVSR-INDX)                   
063700        MOVE +0                TO AVSR-VLARTNTO(AVSR-INDX)                
065600        MOVE +0                TO AVSR-KDVSOP(AVSR-INDX)                  
065610                                  AVSR-KDFARLIG(AVSR-INDX)                
065620        ADD +1                 TO AVSR-INDX                               
065630     END-PERFORM                                                          
065640                                                                          
065650     MOVE +1                   TO AVSR-INDX                               
065660     .                                                                    
065670     EJECT                                                                
065680                                                                          
065690 AB-FIXA-LOKAL-TID SECTION.                                               
065700                                                                          
065800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
065900     MOVE '013'             TO MSGI-KDCALL                                
066000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
066100     MOVE '4292'            TO MSGI-IDTRANS                               
066200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
066300                                                                          
066400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
066500     .                                                                    
066600     EJECT                                                                
066700                                                                          
066800 B-KOLLA-INDATA SECTION.                                                  
066900                                                                          
067000       MOVE JA TO ALLT-SW                                                 
067100                  FORTSAETT-SW                                            
067200       MOVE LOW-VALUE TO       W-IDORDER-X                                
067300                               W-IDGMTREF-X                               
067400                               W-WDQ401KY-MIN-X                           
067500                               W-WDQ101KY-MIN-X                           
067600                               W-WDGXKEY-MIN-X                            
067700                               W-WDA501KY-X                               
067800                               W-IDARTNR-X                                
067900       MOVE HIGH-VALUE TO      W-WDQ401KY-MAX-X                           
068000                               W-WDQ101KY-MAX-X                           
068100                               W-WDGXKEY-MAX-X                            
068200     MOVE MID-IDDC TO W-IDDC-B6                                           
068300     PERFORM IMS-GU-WDB601                                                
068400     INSPECT MID-IDORDER REPLACING LEADING SPACE BY ZERO                  
068500     IF MID-IDORDER NOT NUMERIC                                           
068600       MOVE NEJ TO ALLT-SW                                                
068700     ELSE                                                                 
068800       MOVE MID-IDORDER         TO     WS-IDORDER                         
068900       MOVE WS-IDORDER          TO     W-OHUV-IDORDER                     
069000                                       W-ORAD-IDORDER-MIN                 
069100                                       W-ORAD-IDORDER-MAX                 
069200                                       W-OBKR-IDORDER-MIN                 
069300                                       W-OBKR-IDORDER-MAX                 
069400     END-IF                                                               
069500     IF WS-IDTRANS NOT = '4204' AND                                       
069600       WS-IDTRANS NOT = '4245'                                            
069700       INSPECT MID-IDDISTR REPLACING LEADING SPACE BY ZERO                
069800       IF MID-IDDISTR NOT NUMERIC                                         
069900         MOVE NEJ TO ALLT-SW                                              
070000       ELSE                                                               
070100         MOVE MID-IDDISTR       TO     WS-IDDISTR                         
070200         MOVE WS-IDDISTR        TO     W-4542-IDDISTR-MIN                 
070300                                       W-4542-IDDISTR-MAX                 
070400                                       W-RAD-IDDISTR                      
070500                                       W-REF-IDDISTR                      
070600       END-IF                                                             
070700       INSPECT MID-IDKUNDNR REPLACING LEADING SPACE BY ZERO               
070800       IF MID-IDKUNDNR NOT NUMERIC                                        
070900         MOVE NEJ TO ALLT-SW                                              
071000       ELSE                                                               
071100         MOVE MID-IDKUNDNR      TO     WS-IDKUNDNR                        
071200         MOVE WS-IDKUNDNR       TO     W-REF-IDKUNDNR                     
071300                                       W-RAD-IDKUNDNR                     
071400       END-IF                                                             
071500       INSPECT MID-IDKUNDRF REPLACING LEADING SPACE BY ZERO               
071600       IF MID-IDKUNDRF NOT NUMERIC                                        
071700         MOVE NEJ TO ALLT-SW                                              
071800       ELSE                                                               
071900         MOVE MID-IDKUNDRF      TO     WS-IDORDNR7                        
072000         MOVE WS-IDKUNDRF       TO     W-REF-IDKUNDRF                     
072100       END-IF                                                             
072200                                                                          
072300       IF MID-IDDB = 'WLORQM'                                             
072400                                                                          
072500         INSPECT MID-IDARTNR REPLACING LEADING SPACE BY ZERO              
072600         IF MID-IDARTNR NOT NUMERIC                                       
072700           MOVE NEJ TO ALLT-SW                                            
072800         ELSE                                                             
072900           MOVE MID-IDARTNR     TO     WS-IDARTNR                         
073000           MOVE WS-IDARTNR      TO     W-OBKR-IDARTNR-MIN                 
073100         END-IF                                                           
073200                                                                          
073300         INSPECT MID-IDLOPNR REPLACING LEADING SPACE BY ZERO              
073400         IF MID-IDLOPNR NOT NUMERIC                                       
073500           MOVE NEJ TO ALLT-SW                                            
073600         ELSE                                                             
073700           MOVE MID-IDLOPNR     TO     WS-IDLOPNR                         
073800           MOVE WS-IDLOPNR      TO     W-OBKR-IDLOPNR-MIN                 
073900         END-IF                                                           
074000                                                                          
074100         INSPECT MID-IDSEKVNR REPLACING LEADING SPACE BY ZERO             
074200         IF MID-IDSEKVNR NOT NUMERIC                                      
074300           MOVE NEJ TO ALLT-SW                                            
074400         ELSE                                                             
074500           MOVE MID-IDSEKVNR    TO     WS-IDSEKVNR                        
074600           MOVE WS-IDSEKVNR     TO     W-OBKR-IDSEKVNR-MIN                
074700         END-IF                                                           
074800                                                                          
074900         IF DCS-KDDC = SPACE OR DCS-DDC                                   
075000           MOVE NEJ TO ALLT-SW                                            
075100         ELSE                                                             
075200           MOVE MID-IDDC        TO     W-OBKR-IDDC-MIN                    
075300         END-IF                                                           
075400                                                                          
075500         INSPECT MID-KDORDBEK REPLACING LEADING SPACE BY ZERO             
075600         IF MID-KDORDBEK NOT NUMERIC                                      
075700           MOVE NEJ TO ALLT-SW                                            
075800         ELSE                                                             
075900           MOVE MID-KDORDBEK    TO     W-OBKR-KDORDBEK-MIN                
076000         END-IF                                                           
076100       END-IF                                                             
076200                                                                          
076300       IF MID-IDDB = 'WLORQF'                                             
076400                                                                          
076500         IF DCS-KDDC = SPACE OR DCS-DDC                                   
076600           MOVE NEJ TO ALLT-SW                                            
076700         ELSE                                                             
076800           MOVE MID-IDDC        TO     W-ORAD-IDDC-MIN                    
076900         END-IF                                                           
077000                                                                          
077100         INSPECT MID-ADLAGOMR REPLACING LEADING SPACE BY ZERO             
077200         IF MID-ADLAGOMR NOT NUMERIC                                      
077300           MOVE NEJ TO ALLT-SW                                            
077400         ELSE                                                             
077500           MOVE MID-ADLAGOMR  TO     WS-ADLAGOMR                          
077600           MOVE WS-ADLAGOMR   TO     W-ORAD-ADLAGOMR-MIN                  
077700         END-IF                                                           
077800                                                                          
077900         INSPECT MID-ADGANG REPLACING LEADING SPACE BY ZERO               
078000         IF MID-ADGANG NOT NUMERIC                                        
078100           MOVE NEJ TO ALLT-SW                                            
078200         ELSE                                                             
078300           MOVE MID-ADGANG      TO     WS-ADGANG                          
078400           MOVE WS-ADGANG       TO     W-ORAD-ADGANG-MIN                  
078500         END-IF                                                           
078600                                                                          
078700         INSPECT MID-ADPLATS REPLACING LEADING SPACE BY ZERO              
078800         IF MID-ADPLATS NOT NUMERIC                                       
078900           MOVE NEJ TO ALLT-SW                                            
079000         ELSE                                                             
079100           MOVE MID-ADPLATS     TO     WS-ADPLATS                         
079200           MOVE WS-ADPLATS      TO     W-ORAD-ADPLATS-MIN                 
079300         END-IF                                                           
079400                                                                          
079500         INSPECT MID-IDARTNR REPLACING LEADING SPACE BY ZERO              
079600         IF MID-IDARTNR NOT NUMERIC                                       
079700           MOVE NEJ TO ALLT-SW                                            
079800         ELSE                                                             
079900           MOVE MID-IDARTNR     TO     WS-IDARTNR                         
080000           MOVE WS-IDARTNR      TO     W-ORAD-IDARTNR-MIN                 
080100         END-IF                                                           
080200                                                                          
080300         INSPECT MID-IDLOPNR REPLACING LEADING SPACE BY ZERO              
080400         IF MID-IDLOPNR NOT NUMERIC                                       
080500           MOVE NEJ TO ALLT-SW                                            
080600         ELSE                                                             
080700           MOVE MID-IDLOPNR     TO     WS-IDLOPNR                         
080800           MOVE WS-IDLOPNR      TO     W-ORAD-IDLOPNR-MIN                 
080900         END-IF                                                           
081000       END-IF                                                             
081100     END-IF                                                               
081200     .                                                                    
081300     EJECT                                                                
081400 C-ANNULLERA-HEL-ORDER SECTION.                                           
081500                                                                          
081600     MOVE OHUV-IDDISTR  TO  WS-IDDISTR                                    
081700                            TEST-IDDISTR                                  
081800                                                                          
081900     PERFORM CA-LAES-ARBETSTABELLEN                                       
082000                                                                          
082100     IF MID-IDDB NOT = 'WLORQF'                                           
082200                                                                          
082300       IF  OHUV-KDORDKL = 0                                               
082400           PERFORM S05-ANNULL-NYVORKO                                     
082500           PERFORM S06-ANNULL-NYVORKO-BRIST                               
082600       END-IF                                                             
082700                                                                          
082800       PERFORM S01-LAES-VOR-QUEUE                                         
082900       PERFORM S23-DELETE-PRICE-Q-ORDER                                   
083000                                                                          
083100     END-IF                                                               
083200                                                                          
083300     PERFORM CC-LAES-ORDERRADS-REGISTER                                   
083400     .                                                                    
083500     EJECT                                                                
083600 CA-LAES-ARBETSTABELLEN SECTION.                                          
083700                                                                          
083800     MOVE +0 TO WS-KDFRAKT(1)                                             
083900                WS-KDFRAKT(2)                                             
084000                WS-KDFRAKT(3)                                             
084100                WS-KDFRAKT(4)                                             
084200                WS-KDFRAKT(5)                                             
084300                WS-KDFRAKT(6)                                             
084400                WS-KDFRAKT(7)                                             
084500                                                                          
084600     PERFORM IMS-GNP-ORQI-ORQI12                                          
084700     PERFORM UNTIL SEGMENT-SAKNAS                                         
084800       IF ARB-IDDC NOT = W-IDDC-B6                                        
084900          MOVE ARB-IDDC         TO W-IDDC-B6                              
085000          PERFORM IMS-GU-WDB601                                           
085100       END-IF                                                             
085200       IF DCS-CDC OR DCS-CDC-TR                                           
085300          MOVE ARB-KDFRAKT             TO WS-KDFRAKT(1)                   
085400       ELSE                                                               
085500          IF DCS-SDC                                                      
085600            MOVE ARB-KDFRAKT           TO WS-KDFRAKT(2)                   
085700          ELSE                                                            
085800             IF DCS-NDC-NA                                                
085900                IF DCS-CANADA                                             
086000                   MOVE ARB-KDFRAKT    TO WS-KDFRAKT(5)                   
086100                ELSE                                                      
086200                   MOVE ARB-KDFRAKT    TO WS-KDFRAKT(4)                   
086300                END-IF                                                    
086400             ELSE                                                         
086500                IF DCS-NDC-PF                                             
086510                   MOVE ARB-KDFRAKT    TO WS-KDFRAKT(6)                   
086520                ELSE                                                      
086530                   IF DCS-NDC-CN                                          
086540                      MOVE ARB-KDFRAKT TO WS-KDFRAKT(7)                   
086550                   END-IF                                                 
086560                END-IF                                                    
086570             END-IF                                                       
086580          END-IF                                                          
086590       END-IF                                                             
086600       PERFORM IMS-GNP-ORQI-ORQI12                                        
086700     END-PERFORM                                                          
086800     .                                                                    
086900     EJECT                                                                
087000 CC-LAES-ORDERRADS-REGISTER SECTION.                                      
087100                                                                          
087200     MOVE +1 TO AVSR-INDX                                                 
087300     PERFORM IMS-GHU-ORQF-ORQF01                                          
087400     ADD  +1 TO ANTAL-LAEST                                               
087500     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT  OR                       
087600                   ANTAL-LAEST > MAX-ANTAL-LAEST OR                       
087700                   AVSR-INDX > MAX-AVSR-INDX                              
087800       PERFORM S02-SKAPA-AVSR                                             
087900       ADD +1 TO AVSR-INDX                                                
088000       PERFORM S03-AENDRA-BASER                                           
088100                                                                          
088200       PERFORM IMS-GHN-ORQF-ORQF01                                        
088300       ADD +1 TO ANTAL-LAEST                                              
088400     END-PERFORM                                                          
088500     IF AVSR-OK                                                           
088600       CALL W413AVSR USING AVSR-W413AVSR AVSR-ALT-PCB                     
088700                           AVSR-ORQI-PCB AVSR-GMTB-PCB                    
088800                           AVSR-GMTC-PCB AVSR-WDB2-PCB                    
088900                           AVSR-WDB6-PCB TRAN-XXKB-PCB                    
089000     END-IF                                                               
089100                                                                          
089200     IF SEGMENT-FINNS                                                     
089300       PERFORM CCA-SKAPA-IMS-TRANS-HEL-ANNULL                             
089400       MOVE NEJ TO FORTSAETT-SW                                           
089500     ELSE                                                                 
089600       PERFORM IMS-GHU-ORQI-ORQI01                                        
089700       IF SEGMENT-FINNS                                                   
089800         MOVE 'J' TO OHUV-FLKLAR                                          
089900         PERFORM IMS-REPL-ORQI                                            
090000       END-IF                                                             
090100     END-IF                                                               
090200     IF FORTSAETT                                                         
090300       PERFORM S04-ANROPA-AVSO                                            
090400       MOVE NEJ TO FORTSAETT-SW                                           
090500     END-IF                                                               
090600     .                                                                    
090700     EJECT                                                                
090800 CCA-SKAPA-IMS-TRANS-HEL-ANNULL SECTION.                                  
090900     MOVE ORAD-IDORDER       TO    WS-IDORDER                             
091000     MOVE WS-IDORDER         TO   MID-IDORDER                             
091100     MOVE SPACE              TO   MID-IDDISTR                             
091200     MOVE SPACE              TO   MID-IDKUNDNR                            
091300     MOVE SPACE              TO   MID-IDKUNDRF                            
091400     MOVE SPACE              TO   MID-IDARTNR                             
091500     MOVE SPACE              TO   MID-IDLOPNR                             
091600     MOVE SPACE              TO   MID-IDSEKVNR                            
091700     MOVE SPACE              TO   MID-IDDC                                
091800     MOVE SPACE              TO   MID-KDORDBEK                            
091900     MOVE SPACE              TO   MID-ADLAGOMR                            
092000     MOVE SPACE              TO   MID-ADGANG                              
092100     MOVE SPACE              TO   MID-ADPLATS                             
092200     MOVE 'WLORQF'           TO   MID-IDDB                                
092300     MOVE 'W4T292X '         TO   MSG-KDTRANS-1                           
092400     MOVE WS-IDTRANS         TO   MSG-IDTRANS-1                           
092500     MOVE MFS-KDMFSFOR       TO   MSG-KDMFSFOR-1                          
092600     COMPUTE MSG-KVLL = LENGTH OF MID-W4I29201 + 17                       
092700     MOVE MID-W4I29201       TO   MSG-INDATA-MINUS-1-TRANSKOD             
092800     PERFORM IMS-INSERT-ALTMSG-4292                                       
092900     .                                                                    
093000     EJECT                                                                
093100 D-ANNULLERAT-FRAN-SVARSBILDER SECTION.                                   
093200                                                                          
093300     MOVE +1  TO ANTAL-LAEST                                              
093400                                                                          
093500     IF MID-IDDB NOT = 'WLORQF'                                           
093600                                                                          
093700       IF  OHUV-KDORDKL = 0                                               
093800       AND OHUV-FLVORKO = JA                                              
093900           PERFORM S05-ANNULL-NYVORKO                                     
094000       END-IF                                                             
094100                                                                          
094200       PERFORM DA-LAES-ORDERBEKRAEFTELSER                                 
094300                                                                          
094400       IF FORTSAETT                                                       
094500         IF OHUV-FLORDSPE = NEJ                                           
094600           PERFORM S01-LAES-VOR-QUEUE                                     
094700           PERFORM S23-DELETE-PRICE-Q-ORDER                               
094800         END-IF                                                           
094900       END-IF                                                             
095000                                                                          
095100     END-IF                                                               
095200                                                                          
095300     IF FORTSAETT                                                         
095400       PERFORM DB-LAES-ORDERRADER                                         
095500     END-IF                                                               
095600     .                                                                    
095700     EJECT                                                                
095800 DA-LAES-ORDERBEKRAEFTELSER SECTION.                                      
095900     PERFORM IMS-GHU-ORQM-ORQM01                                          
096000     ADD +1 TO ANTAL-LAEST                                                
096100     IF SEGMENT-FINNS                                                     
096200       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
096300       ANTAL-LAEST > MAX-ANTAL-LAEST OR BASEN-SLUT                        
096400         IF OBKR-FLOBOK = NEJ                                             
096500           IF OBKR-IDLEVNR = SPACE AND OHUV-FLORDSPE = NEJ                
096600             PERFORM DAA-BACKA-K9-K7-A5                                   
096700           END-IF                                                         
096800         END-IF                                                           
096900         PERFORM IMS-DLET-ORQM                                            
097000         PERFORM IMS-GHN-ORQM-ORQM01                                      
097100         ADD +2 TO ANTAL-LAEST                                            
097200       END-PERFORM                                                        
097300                                                                          
097400       IF SEGMENT-FINNS                                                   
097500         PERFORM DAB-OMSKEDULERA-ORQM                                     
097600         MOVE NEJ TO FORTSAETT-SW                                         
097700       END-IF                                                             
097800     END-IF                                                               
097900     .                                                                    
098000     EJECT                                                                
098100 DAA-BACKA-K9-K7-A5 SECTION.                                              
098200                                                                          
098300     IF OBKR-IDARTNR-TILLK > +0                                           
098400       MOVE OBKR-IDARTNR-TILLK   TO   W-IDARTNR                           
098500     ELSE                                                                 
098600       MOVE OBKR-IDARTNR         TO   W-IDARTNR                           
098700     END-IF                                                               
098800                                                                          
098900     IF OBKR-IDDC NOT = W-IDDC-B6                                         
099000        MOVE OBKR-IDDC          TO W-IDDC-B6                              
099100        PERFORM IMS-GU-WDB601                                             
099200     END-IF                                                               
099300     IF DCS-CDC                                                           
099400       IF OBKR-KDORDBEK = 10                                              
099500          PERFORM DAAB-BACKA-WDA501                                       
099600       ELSE                                                               
099700          IF OBKR-KVPREAVB > +0 OR OBKR-KVPRERO > +0                      
099800             PERFORM IMS-GHU-ARTM-ARTM01                                  
099900             IF SEGMENT-FINNS                                             
100000                PERFORM DAAA-BACKA-WDK901                                 
100100                PERFORM IMS-REPL-ARTM                                     
100200                ADD +1 TO ANTAL-LAEST                                     
100300             END-IF                                                       
100400          END-IF                                                          
100500       END-IF                                                             
100600     ELSE                                                                 
100700                                                                          
100800       IF DCS-NDC                                                         
100900          IF OBKR-KDORDBEK = 10                                           
101000             PERFORM DAAB-BACKA-WDA501                                    
101100          ELSE                                                            
101200             IF OBKR-KVPREAVB > +0 OR OBKR-KVPRERO > +0                   
101300                MOVE OBKR-IDDC TO W-IDDC                                  
101400                PERFORM IMS-GHU-ARTS-ARTS11                               
101500                IF OBKR-KDORDKL    = 0 OR 1                               
101600                   SUBTRACT OBKR-KVBEART-Q    FROM SLAG-KVOKS-DAG         
101700                ELSE                                                      
101800                   IF OBKR-KDORDKL = 2 OR 3 OR 4                          
101900                      SUBTRACT OBKR-KVBEART-Q FROM SLAG-KVOKS-BULK        
102000                   END-IF                                                 
102100                END-IF                                                    
102200                PERFORM IMS-REPL-ARTS                                     
102300                ADD +1 TO ANTAL-LAEST                                     
102400             END-IF                                                       
102500          END-IF                                                          
102600       ELSE                                                               
102700                                                                          
102800          IF DCS-SDC                                                      
102900             IF OBKR-KVPREAVB > +0 OR OBKR-KVPRERO > +0 OR                
103000                ((DCS-SDC AND DCS-SWEDEN)                                 
103100                  AND OBKR-IDSYSTEM = 'LDC ')                             
103200                MOVE OBKR-IDDC TO W-IDDC                                  
103300                PERFORM IMS-GHU-ARTS-ARTS11                               
103400                IF OBKR-KDORDKL    = 0 OR 1                               
103500                   SUBTRACT OBKR-KVBEART-Q    FROM SLAG-KVOKS-DAG         
103600                ELSE                                                      
103700                   IF OBKR-KDORDKL = 2 OR 3 OR 4                          
103800                      SUBTRACT OBKR-KVBEART-Q FROM SLAG-KVOKS-BULK        
103900                   END-IF                                                 
104000                END-IF                                                    
104100                PERFORM IMS-REPL-ARTS                                     
104200                ADD +1 TO ANTAL-LAEST                                     
104300             END-IF                                                       
104400          END-IF                                                          
104500       END-IF                                                             
104600     END-IF                                                               
104700     MOVE OBKR-IDDISTR     TO TEST-IDDISTR                                
104800     MOVE OBKR-KVBEART-Q   TO ANNULLERAT-ANTAL                            
104900     PERFORM S17A-FIXA-REFILL-TRANSFER                                    
105000     .                                                                    
105100     EJECT                                                                
105200 DAAA-BACKA-WDK901 SECTION.                                               
105300                                                                          
105400     IF OHUV-KDORDKL = ZERO                                               
105500       COMPUTE ART-KVPREAVB-VOR =                                         
105600         ART-KVPREAVB-VOR - OBKR-KVPREAVB                                 
105700       COMPUTE ART-KVOKS-VOR =                                            
105800         ART-KVOKS-VOR - (OBKR-KVPREAVB + OBKR-KVPRERO)                   
105900     END-IF                                                               
106000                                                                          
106100     IF OHUV-KDORDKL = +1                                                 
106200       COMPUTE ART-KVPREAVB-DAG =                                         
106300         ART-KVPREAVB-DAG - OBKR-KVPREAVB                                 
106400       COMPUTE ART-KVOKS-DAG =                                            
106500         ART-KVOKS-DAG - (OBKR-KVPREAVB + OBKR-KVPRERO)                   
106600       COMPUTE ART-KVPRERO-DAG =                                          
106700         ART-KVPRERO-DAG - OBKR-KVPRERO                                   
106800     END-IF                                                               
106900                                                                          
107000     IF OHUV-KDORDKL = +2 OR +3 OR +4                                     
107100       COMPUTE ART-KVPREAVB-BULK =                                        
107200         ART-KVPREAVB-BULK - OBKR-KVPREAVB                                
107300       COMPUTE ART-KVOKS-BULK =                                           
107400         ART-KVOKS-BULK -                                                 
107500        (OBKR-KVPREAVB + OBKR-KVPRERO)                                    
107600       COMPUTE ART-KVPRERO-BULK =                                         
107700         ART-KVPRERO-BULK - OBKR-KVPRERO                                  
107800     END-IF                                                               
107900     .                                                                    
108000     EJECT                                                                
108100 DAAB-BACKA-WDA501 SECTION.                                               
108200                                                                          
108300     MOVE OBKR-IDDISTR           TO   W-RAD-IDDISTR                       
108400     MOVE OBKR-IDKUNDNR          TO   W-RAD-IDKUNDNR                      
108500     MOVE OBKR-IDKUNDRF-RO (3:5) TO   W-RAD-IDKUNDRF                      
108600     MOVE OBKR-IDARTNR           TO   W-RAD-IDARTNR                       
108700     MOVE OBKR-IDLOPNR-RO        TO   W-RAD-IDLOPNR                       
108800     MOVE OBKR-IDDC              TO   W-RAD-IDDC                          
108900                                                                          
109000     PERFORM IMS-GHU-ORDP-ORDP01                                          
109100     ADD +1 TO ANTAL-LAEST                                                
109200     IF SEGMENT-FINNS                                                     
109300       MOVE '00000     '         TO   RAD-IDKUNDRF-LEV                    
109400       MOVE '3'                  TO   RAD-KDSTARAD                        
109500       PERFORM IMS-REPL-ORDP                                              
109600       ADD +1                    TO ANTAL-LAEST                           
109700     END-IF                                                               
109800     .                                                                    
109900     EJECT                                                                
110000 DAB-OMSKEDULERA-ORQM SECTION.                                            
110100                                                                          
110200     MOVE OBKR-IDORDER       TO    WS-IDORDER                             
110300     MOVE WS-IDORDER         TO   MID-IDORDER                             
110400     MOVE OBKR-IDARTNR       TO    WS-IDARTNR                             
110500     MOVE WS-IDARTNR         TO   MID-IDARTNR                             
110600     MOVE OBKR-IDLOPNR       TO    WS-IDLOPNR                             
110700     MOVE WS-IDLOPNR         TO   MID-IDLOPNR                             
110800     MOVE OBKR-IDSEKVNR      TO    WS-IDSEKVNR                            
110900     MOVE WS-IDSEKVNR        TO   MID-IDSEKVNR                            
111000     MOVE OBKR-IDDC          TO   MID-IDDC                                
111100     MOVE OBKR-KDORDBEK      TO   MID-KDORDBEK                            
111200     MOVE WS-IDDISTR         TO   MID-IDDISTR                             
111300     MOVE WS-IDKUNDNR        TO   MID-IDKUNDNR                            
111400     MOVE WS-IDORDNR7        TO   MID-IDKUNDRF                            
111500     MOVE SPACE              TO   MID-ADLAGOMR                            
111600     MOVE SPACE              TO   MID-ADGANG                              
111700     MOVE SPACE              TO   MID-ADPLATS                             
111800     MOVE 'WLORQM'           TO   MID-IDDB                                
111900     MOVE 'W4T292X '         TO   MSG-KDTRANS-1                           
112000     MOVE WS-IDTRANS         TO   MSG-IDTRANS-1                           
112100     MOVE MFS-KDMFSFOR       TO   MSG-KDMFSFOR-1                          
112200     COMPUTE MSG-KVLL = LENGTH OF MID-W4I29201 + 17                       
112300     MOVE MID-W4I29201       TO   MSG-INDATA-MINUS-1-TRANSKOD             
112400     PERFORM IMS-INSERT-ALTMSG-4292                                       
112500     .                                                                    
112600     EJECT                                                                
112700 DB-LAES-ORDERRADER SECTION.                                              
112800                                                                          
112900     PERFORM IMS-GHU-ORQF-ORQF01                                          
113000     ADD +1 TO ANTAL-LAEST                                                
113100     IF SEGMENT-FINNS                                                     
113200       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                      
113300       ANTAL-LAEST > MAX-ANTAL-LAEST                                      
113400         PERFORM S03-AENDRA-BASER                                         
113500         PERFORM IMS-GHN-ORQF-ORQF01                                      
113600         ADD +1 TO ANTAL-LAEST                                            
113700       END-PERFORM                                                        
113800                                                                          
113900       IF SEGMENT-FINNS                                                   
114000         PERFORM DBA-OMSKEDULERA-ORQF                                     
114100         MOVE NEJ TO FORTSAETT-SW                                         
114200       ELSE                                                               
114300         PERFORM IMS-GHU-ORQI-ORQI01                                      
114400         IF SEGMENT-FINNS                                                 
114500           PERFORM IMS-DLET-ORQI                                          
114600         END-IF                                                           
114700         MOVE NEJ TO FORTSAETT-SW                                         
114800       END-IF                                                             
114900     ELSE                                                                 
115000       PERFORM IMS-GHU-ORQI-ORQI01                                        
115100       IF SEGMENT-FINNS                                                   
115200         PERFORM IMS-DLET-ORQI                                            
115300       END-IF                                                             
115400       MOVE NEJ TO FORTSAETT-SW                                           
115500     END-IF                                                               
115600     .                                                                    
115700     EJECT                                                                
115800 DBA-OMSKEDULERA-ORQF SECTION.                                            
115900                                                                          
116000     MOVE ORAD-IDORDER       TO   WS-IDORDER                              
116100     MOVE WS-IDORDER         TO  MID-IDORDER                              
116200     MOVE ORAD-IDDC          TO  MID-IDDC                                 
116300     MOVE ORAD-ADLAGOMR      TO   WS-ADLAGOMR                             
116400     MOVE WS-ADLAGOMR        TO  MID-ADLAGOMR                             
116500     MOVE ORAD-ADGANG        TO   WS-ADGANG                               
116600     MOVE WS-ADGANG          TO  MID-ADGANG                               
116700     MOVE ORAD-ADPLATS       TO   WS-ADPLATS                              
116800     MOVE WS-ADPLATS         TO  MID-ADPLATS                              
116900     MOVE ORAD-IDARTNR       TO   WS-IDARTNR                              
117000     MOVE WS-IDARTNR         TO  MID-IDARTNR                              
117100     MOVE ORAD-IDLOPNR       TO   WS-IDLOPNR                              
117200     MOVE WS-IDLOPNR         TO  MID-IDLOPNR                              
117300     MOVE WS-IDDISTR         TO  MID-IDDISTR                              
117400     MOVE WS-IDKUNDNR        TO  MID-IDKUNDNR                             
117500     MOVE WS-IDORDNR7        TO  MID-IDKUNDRF                             
117600     MOVE SPACE              TO  MID-IDSEKVNR                             
117700     MOVE SPACE              TO  MID-KDORDBEK                             
117800     MOVE 'WLORQF'           TO  MID-IDDB                                 
117900     MOVE 'W4T292X '         TO  MSG-KDTRANS-1                            
118000     MOVE WS-IDTRANS         TO  MSG-IDTRANS-1                            
118100     MOVE MFS-KDMFSFOR       TO  MSG-KDMFSFOR-1                           
118200     MOVE MID-W4I29201       TO  MSG-INDATA-MINUS-1-TRANSKOD              
118300                                                                          
118400     COMPUTE MSG-KVLL = LENGTH OF MID-W4I29201 + 17                       
118500     PERFORM IMS-INSERT-ALTMSG-4292                                       
118600     .                                                                    
118700     EJECT                                                                
118800 S01-LAES-VOR-QUEUE SECTION.                                              
118900     MOVE WS-IDDISTR     TO   W-4542-IDDISTR-MIN                          
119000                              W-4542-IDDISTR-MAX                          
119100     PERFORM IMS-GHU-4541-454111                                          
119200     IF SEGMENT-FINNS                                                     
119300       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                         
119400         IF 4542-IDORDER = WS-IDORDER                                     
119500                                                                          
119600           IF 4542-IDLEVNR = SPACE                                        
119700              IF 4542-IDDC NOT = W-IDDC-B6                                
119800                 MOVE 4542-IDDC TO W-IDDC-B6                              
119900                 PERFORM IMS-GU-WDB601                                    
120000              END-IF                                                      
120100              COMPUTE VORDIFF = 4542-KVBEART-Q - 4542-KVPREAVB            
120200              IF DCS-CDC                                                  
120300                MOVE 4542-IDARTNR   TO   W-IDARTNR                        
120400                PERFORM IMS-GHU-ARTM-ARTM01                               
120500                ADD +1 TO ANTAL-LAEST                                     
120600                IF SEGMENT-FINNS                                          
120700                  COMPUTE ART-KVOKS-VOR =                                 
120800                    ART-KVOKS-VOR - VORDIFF                               
120900                  PERFORM IMS-REPL-ARTM                                   
121000                  ADD +1 TO ANTAL-LAEST                                   
121100                END-IF                                                    
121200              ELSE                                                        
121300                IF 4542-KDORDBEK = 92 OR 93                               
121400                  MOVE 4542-IDARTNR TO W-IDARTNR                          
121500                  MOVE 4542-IDDC    TO W-IDDC                             
121600                  PERFORM IMS-GHU-ARTS-ARTS11                             
121700                  ADD +1 TO ANTAL-LAEST                                   
121800                  IF SEGMENT-FINNS                                        
121900                    COMPUTE SLAG-KVOKS-DAG =                              
122000                      SLAG-KVOKS-DAG - VORDIFF                            
122100                    PERFORM IMS-REPL-ARTS                                 
122200                    ADD +1 TO ANTAL-LAEST                                 
122300                  END-IF                                                  
122400                END-IF                                                    
122500              END-IF                                                      
122600           END-IF                                                         
122700           PERFORM IMS-DLET-4541                                          
122800                                                                          
122900         END-IF                                                           
123000         PERFORM IMS-GHN-4541-454111                                      
123100       END-PERFORM                                                        
123200     END-IF                                                               
123300     .                                                                    
123400     EJECT                                                                
123500 S02-SKAPA-AVSR SECTION.                                                  
123600                                                                          
123700     MOVE JA TO AVSR-SW                                                   
123800     MOVE +2                 TO   AVSR-KDCALL                             
123900     MOVE WS-IDORDER         TO   AVSR-IDORDER                            
124000     MOVE OHUV-KDORDKL       TO   AVSR-KDORDKL                            
124100     MOVE ZERO               TO   AVSR-KDFRAKT                            
124200     MOVE ZERO               TO   AVSR-KDROPACK                           
124300     MOVE MSGI-TILOKDAT      TO   AVSR-TIREGDAT                           
124400     MOVE MSGI-TILOKTID      TO   AVSR-TIHHMM                             
124500                                                                          
124600     MOVE ORAD-ADLAGOMR      TO   AVSR-ADLAGOMR(AVSR-INDX)                
124700     MOVE ORAD-IDDC          TO   AVSR-IDDC(AVSR-INDX)                    
124800     MOVE ORAD-IDLEVNR       TO   AVSR-IDLEVNR(AVSR-INDX)                 
124900     MOVE ORAD-KDSPEEMB      TO   AVSR-KDSPEEMB(AVSR-INDX)                
125000     MOVE ORAD-KVBEART-Q     TO   AVSR-KVANNANT(AVSR-INDX)                
125100     MOVE ORAD-KVBEART-Q     TO   AVSR-KVBEART-Q(AVSR-INDX)               
125200     MOVE ORAD-PRARTNTO      TO   AVSR-PRARTNTO(AVSR-INDX)                
125300     MOVE ORAD-PRAVCOST      TO   AVSR-PRAVCOST(AVSR-INDX)                
125400     MOVE ORAD-DEAL-PR-LINE  TO   AVSR-DEAL-PR-LINE(AVSR-INDX)            
125500     MOVE ORAD-VKART         TO   AVSR-VKART(AVSR-INDX)                   
125600     MOVE ORAD-VLARTNTO      TO   AVSR-VLARTNTO(AVSR-INDX)                
125700     MOVE SPACE              TO   AVSR-KDORDSTA(AVSR-INDX)                
125800     MOVE +0                 TO   AVSR-KDVIA   (AVSR-INDX)                
125900     MOVE +0                 TO   AVSR-KVDAGAR-DIFF(AVSR-INDX)            
126000     MOVE +0                 TO   AVSR-TISKEPPN-DDC(AVSR-INDX)            
126100     .                                                                    
126200     EJECT                                                                
126300 S03-AENDRA-BASER SECTION.                                                
126400                                                                          
126500     MOVE ORAD-IDDC (1:1)        TO   DC-IX                               
126600                                                                          
126700     IF ORAD-IDKUNDRF-RO NOT = '0000000   ' AND                           
126800        ORAD-IDKUNDRF-RO NOT = ORAD-IDKUNDRF                              
126900                                                                          
127000        PERFORM S03A-AENDRA-BIPACKADE-RADER                               
127100                                                                          
127200     ELSE                                                                 
127300*SKROT 991006                                                             
127400       IF OHUV-FLORDSPE = NEJ                                             
127500         OR (OHUV-FLORDSPE = JA AND OHUV-IDSYSTEM = 'W216')               
127600         PERFORM S03B-AENDRA-RADER                                        
127700       END-IF                                                             
127800     END-IF                                                               
127900                                                                          
128000     PERFORM IMS-DLET-ORQF                                                
128100     ADD +1 TO ANTAL-LAEST                                                
128200     .                                                                    
128300     EJECT                                                                
128400 S03A-AENDRA-BIPACKADE-RADER SECTION.                                     
128500     MOVE ORAD-IDDISTR           TO   W-RAD-IDDISTR                       
128600     MOVE ORAD-IDKUNDNR          TO   W-RAD-IDKUNDNR                      
128700     MOVE ORAD-IDKUNDRF-RO (3:5) TO   W-RAD-IDKUNDRF                      
128800     MOVE ORAD-IDARTNR           TO   W-RAD-IDARTNR                       
128900                                      W-IDARTNR                           
129000     MOVE ORAD-IDLOPNR-RO        TO   W-RAD-IDLOPNR                       
129100     MOVE ORAD-IDDC              TO   W-RAD-IDDC                          
129200                                                                          
129300     PERFORM IMS-GHU-ORDP-ORDP01                                          
129400     ADD +1 TO ANTAL-LAEST                                                
129500     IF SEGMENT-FINNS                                                     
129600       IF RAD-KDSTARAD NOT = '3'                                          
129700         MOVE '00000     '       TO   RAD-IDKUNDRF-LEV                    
129800         MOVE '3'                TO   RAD-KDSTARAD                        
129900         PERFORM IMS-REPL-ORDP                                            
130000         ADD +1 TO ANTAL-LAEST                                            
130100       END-IF                                                             
130200     END-IF                                                               
130300                                                                          
130400     IF WS-IDTRANS = '4204' OR '4245'                                     
130500       MOVE 91                   TO   SPAR-KDORDBEK                       
130600       MOVE +0 TO WS-ANTOBKR                                              
130700       PERFORM S03C-SKAPA-ORDERBEKR                                       
130800       PERFORM S11-SKAPA-TRANSAR                                          
130900     END-IF                                                               
131000                                                                          
131100     IF ORAD-KDTPOTYP > ZERO AND                                          
131200       ORAD-TIRODAT NOT > ZERO                                            
131300       MOVE ORAD-IDARTNR         TO   W-IDARTNR                           
131400       IF OHUV-IDSYSTEM = 'W216' AND OHUV-FLORDSPE = JA                   
131500         CONTINUE                                                         
131600       ELSE                                                               
131700         IF ORAD-IDLEVNR = SPACE                                          
131800           PERFORM IMS-GHU-ARTM-ARTM01                                    
131900           ADD +1 TO ANTAL-LAEST                                          
132000           IF SEGMENT-FINNS                                               
132100             PERFORM S03AA-AENDRA-WDK901                                  
132200             PERFORM IMS-REPL-ARTM                                        
132300             ADD +1 TO ANTAL-LAEST                                        
132400           END-IF                                                         
132500         END-IF                                                           
132600       END-IF                                                             
132700     END-IF                                                               
132800     .                                                                    
132900     EJECT                                                                
133000 S03AA-AENDRA-WDK901 SECTION.                                             
133100     IF ORAD-KDORDKL = ZERO                                               
133200       IF ORAD-KVPREAVB > ZERO                                            
133300          COMPUTE ART-KVPREAVB-VOR =                                      
133400          ART-KVPREAVB-VOR - ORAD-KVPREAVB                                
133500       END-IF                                                             
133600     END-IF                                                               
133700                                                                          
133800     IF ORAD-KDORDKL = +1                                                 
133900       IF ORAD-KVPREAVB > ZERO                                            
134000         COMPUTE ART-KVPREAVB-DAG =                                       
134100         ART-KVPREAVB-DAG - ORAD-KVPREAVB                                 
134200       END-IF                                                             
134300       IF ORAD-KVPRERO > ZERO                                             
134400          COMPUTE ART-KVPRERO-DAG =                                       
134500          ART-KVPRERO-DAG - ORAD-KVPRERO                                  
134600       END-IF                                                             
134700     END-IF                                                               
134800                                                                          
134900     IF ORAD-KDORDKL = +2 OR +3 OR +4                                     
135000       IF ORAD-KVPREAVB > ZERO                                            
135100          COMPUTE ART-KVPREAVB-BULK =                                     
135200          ART-KVPREAVB-BULK - ORAD-KVPREAVB                               
135300       END-IF                                                             
135400       IF ORAD-KVPRERO > ZERO                                             
135500          COMPUTE ART-KVPRERO-BULK =                                      
135600          ART-KVPRERO-BULK - ORAD-KVPRERO                                 
135700       END-IF                                                             
135800     END-IF                                                               
135900     .                                                                    
136000     EJECT                                                                
136100 S03B-AENDRA-RADER SECTION.                                               
136200     MOVE ORAD-IDARTNR         TO   W-IDARTNR                             
136300     IF ORAD-IDKAMPRF > ZERO                                              
136400       PERFORM S13-BACKA-KAMPANJ                                          
136500     END-IF                                                               
136600     IF OHUV-IDSYSTEM = 'W216' AND OHUV-FLORDSPE = JA                     
136700       CONTINUE                                                           
136800     ELSE                                                                 
136900       IF ORAD-IDLEVNR = SPACE                                            
137000         IF ORAD-IDDC NOT = W-IDDC-B6                                     
137100            MOVE ORAD-IDDC        TO W-IDDC-B6                            
137200            PERFORM IMS-GU-WDB601                                         
137300         END-IF                                                           
137400         IF DCS-CDC                                                       
137500           PERFORM IMS-GHU-ARTM-ARTM01                                    
137600           ADD +1 TO ANTAL-LAEST                                          
137700           IF SEGMENT-FINNS                                               
137800             PERFORM S03BA-AENDRA-WDK901                                  
137900             IF ORAD-IDKAMPRF NOT > ZERO                                  
138000               PERFORM S03BB-AENDRA-WDK901                                
138100             END-IF                                                       
138200             PERFORM IMS-REPL-ARTM                                        
138300             ADD +1 TO ANTAL-LAEST                                        
138400           END-IF                                                         
138500         ELSE                                                             
138600           IF DCS-NDC OR DCS-SDC                                          
138700              MOVE ORAD-IDDC TO W-IDDC                                    
138800              PERFORM IMS-GHU-ARTS-ARTS11                                 
138900              IF ORAD-KDORDKL  = 0 OR 1                                   
139000                 SUBTRACT ORAD-KVBEART-Q  FROM SLAG-KVOKS-DAG             
139100              ELSE                                                        
139200                 IF ORAD-KDORDKL = 2 OR 3 OR 4                            
139300                    IF ORAD-KVOKS-PREL = ZERO                             
139400                      SUBTRACT ORAD-KVBEART-Q FROM SLAG-KVOKS-BULK        
139500                    END-IF                                                
139600                 END-IF                                                   
139700              END-IF                                                      
139800************                                                              
139900              MOVE ORAD-IDDISTR TO TEST-IDDISTR                           
140000              IF DIST18-SCRAP-NDC                                         
140100              MOVE 'N'       TO SLAG-FLSKROT-BEORD                        
140200              END-IF                                                      
140300**************                                                            
140400              PERFORM IMS-REPL-ARTS                                       
140500              ADD +1 TO ANTAL-LAEST                                       
140600           END-IF                                                         
140700         END-IF                                                           
140800         MOVE ORAD-IDDISTR   TO TEST-IDDISTR                              
140900         MOVE ORAD-KVBEART-Q TO ANNULLERAT-ANTAL                          
141000         PERFORM S17B-FIXA-REFILL-TRANSFER                                
141100       END-IF                                                             
141200     END-IF                                                               
141300                                                                          
141400     IF WS-IDTRANS = '4204' OR '4245'                                     
141500       MOVE 83                   TO   SPAR-KDORDBEK                       
141600       MOVE +0 TO WS-ANTOBKR                                              
141700       PERFORM S03C-SKAPA-ORDERBEKR                                       
141800       PERFORM S11-SKAPA-TRANSAR                                          
141900     END-IF                                                               
142000     .                                                                    
142100     EJECT                                                                
142200 S03BA-AENDRA-WDK901 SECTION.                                             
142300     IF ORAD-KDORDKL = ZERO                                               
142400       COMPUTE ART-KVPREAVB-VOR =                                         
142500         ART-KVPREAVB-VOR - ORAD-KVPREAVB                                 
142600     END-IF                                                               
142700                                                                          
142800     IF ORAD-KDORDKL = +1                                                 
142900       COMPUTE ART-KVPREAVB-DAG =                                         
143000         ART-KVPREAVB-DAG - ORAD-KVPREAVB                                 
143100       COMPUTE ART-KVPRERO-DAG =                                          
143200         ART-KVPRERO-DAG - ORAD-KVPRERO                                   
143300     END-IF                                                               
143400                                                                          
143500     IF ORAD-KDORDKL = +2 OR +3 OR +4                                     
143600       COMPUTE ART-KVPREAVB-BULK =                                        
143700         ART-KVPREAVB-BULK - ORAD-KVPREAVB                                
143800       COMPUTE ART-KVPRERO-BULK =                                         
143900         ART-KVPRERO-BULK - ORAD-KVPRERO                                  
144000     END-IF                                                               
144100     .                                                                    
144200     EJECT                                                                
144300 S03BB-AENDRA-WDK901 SECTION.                                             
144400     IF OHUV-KDORDKL = ZERO                                               
144500       COMPUTE ART-KVOKS-VOR =                                            
144600         ART-KVOKS-VOR - ORAD-KVBEART-Q                                   
144700     END-IF                                                               
144800                                                                          
144900     IF OHUV-KDORDKL = +1                                                 
145000       COMPUTE ART-KVOKS-DAG =                                            
145100         ART-KVOKS-DAG - ORAD-KVBEART-Q                                   
145200     END-IF                                                               
145300                                                                          
145400     IF OHUV-KDORDKL = +2 OR +3 OR +4                                     
145500       COMPUTE ART-KVOKS-BULK =                                           
145600         ART-KVOKS-BULK - ORAD-KVBEART-Q                                  
145700     END-IF                                                               
145800     .                                                                    
145900     EJECT                                                                
146000 S03C-SKAPA-ORDERBEKR SECTION.                                            
146100     PERFORM S03CA-LAES-WDK6                                              
146200     IF WS-ANTOBKR = +0                                                   
146300       MOVE OHUV-IDORDER         TO   W-OBKR-IDORDER-MIN                  
146400                                      W-OBKR-IDORDER-MAX                  
146500       MOVE ORAD-IDARTNR         TO   W-OBKR-IDARTNR-MIN                  
146600                                      W-OBKR-IDARTNR-MAX                  
146700       MOVE +1                   TO   W-OBKR-IDLOPNR-MIN                  
146800                                      W-OBKR-IDLOPNR-MAX                  
146900                                      W-OBKR-IDLOPNR-MAX                  
147000       MOVE +1                   TO   W-OBKR-IDSEKVNR-MIN                 
147100                                      W-OBKR-IDSEKVNR-MAX                 
147200       PERFORM IMS-GU-ORQM-ORQM01                                         
147300       PERFORM UNTIL SEGMENT-SAKNAS                                       
147400         ADD +1 TO W-OBKR-IDLOPNR-MIN                                     
147500                   W-OBKR-IDLOPNR-MAX                                     
147600         PERFORM IMS-GU-ORQM-ORQM01                                       
147700       END-PERFORM                                                        
147800     END-IF                                                               
147900     MOVE WS-IDORDER             TO   OBKR-IDORDER                        
148000     MOVE ORAD-IDARTNR           TO   OBKR-IDARTNR                        
148100     MOVE W-OBKR-IDLOPNR-MIN     TO   OBKR-IDLOPNR                        
148200     MOVE 1                      TO   OBKR-IDSEKVNR                       
148300     MOVE ORAD-IDDC              TO   OBKR-IDDC                           
148400     MOVE ORAD-IDDC-RO           TO   OBKR-IDDC-RO                        
148500     MOVE ORAD-KDOI              TO   OBKR-KDOI                           
148600     MOVE ORAD-CLEARGROUP        TO   OBKR-CLEARGROUP                     
148700     MOVE SPAR-KDORDBEK          TO   OBKR-KDORDBEK                       
148800     MOVE SPACE                  TO   OBKR-BEERS                          
148900     MOVE SPACE                  TO   OBKR-IDBIL                          
149000     MOVE IDPGM                  TO   OBKR-IDPGM                          
149100     MOVE OHUV-BEKUNDRF          TO   OBKR-BEKUNDRF                       
149200     MOVE ORAD-BERADREF          TO   OBKR-BERADREF                       
149300     MOVE ORAD-BEVOLREF          TO   OBKR-BEVOLREF                       
149400     MOVE ORAD-IDKAMPRF          TO   OBKR-IDKAMPRF                       
149500     MOVE ZERO                   TO   OBKR-DIERS-KVOT                     
149600     MOVE ORAD-FLAKPLOC          TO   OBKR-FLAKPLOC                       
149700     MOVE ORAD-FLINVEST          TO   OBKR-FLINVEST                       
149800     MOVE 'J'                    TO   OBKR-FLOBOK                         
149900     MOVE 'J'                    TO   OBKR-FLOBTRAN                       
150000     MOVE 'N'                    TO   OBKR-FLOBPRT                        
150100     MOVE ORAD-FLPRTILL          TO   OBKR-FLPRTILL                       
150200     MOVE ORAD-FLRESTN           TO   OBKR-FLRESTN                        
150300     MOVE JA                     TO   OBKR-FLSLATT                        
150400     MOVE ORAD-FLTILLK           TO   OBKR-FLTILLK                        
150500     MOVE ZERO                   TO   OBKR-IDARTNR-TILLK                  
150600     MOVE ORAD-IDDISTR           TO   OBKR-IDDISTR                        
150700     MOVE ORAD-IDKUNDNR          TO   OBKR-IDKUNDNR                       
150800     MOVE ORAD-IDKUNDRF          TO   OBKR-IDKUNDRF                       
150900     MOVE ORAD-IDKUNDRF-RO       TO   OBKR-IDKUNDRF-RO                    
151000     MOVE ORAD-IDLEVNR           TO   OBKR-IDLEVNR                        
151100     MOVE ORAD-IDLOPNR-RO        TO   OBKR-IDLOPNR-RO                     
151200     MOVE ORAD-IDSYSTEM          TO   OBKR-IDSYSTEM                       
151300     MOVE ORAD-KDDSP             TO   OBKR-KDDSP                          
151400     MOVE ZERO                   TO   OBKR-KDERS                          
151500     MOVE ORAD-KDKVBRYT          TO   OBKR-KDKVBRYT                       
151600     MOVE ORAD-KDPRTYP           TO   OBKR-KDPRTYP                        
151700     MOVE ORAD-KDTPOTYP          TO   OBKR-KDTPOTYP                       
151800     MOVE ORAD-KDVRINFO          TO   OBKR-KDVRINFO                       
151900     IF SPAR-KDORDBEK = 83                                                
152000       MOVE ORAD-KVBEART-Q       TO   OBKR-KVANNANT                       
152100     ELSE                                                                 
152200       MOVE ZERO                 TO   OBKR-KVANNANT                       
152300     END-IF                                                               
152400     IF SPAR-KDORDBEK = 83                                                
152500       MOVE ZERO                 TO   OBKR-KVAVBART                       
152600     ELSE                                                                 
152700       MOVE ORAD-KVBEART         TO   OBKR-KVAVBART                       
152800     END-IF                                                               
152900     MOVE ORAD-KVBEART           TO   OBKR-KVBEART                        
153000     MOVE ORAD-KVBEART-Q         TO   OBKR-KVBEART-Q                      
153100     MOVE ZERO                   TO   OBKR-KVBEART-TILLK                  
153200     MOVE ZERO                   TO   OBKR-KVPREAVB                       
153300     MOVE ZERO                   TO   OBKR-KVPRERO                        
153400     MOVE WS-KVQPACK-1           TO   OBKR-KVQPACK                        
153500     IF SPAR-KDORDBEK = 83                                                
153600       MOVE ZERO                 TO   OBKR-KVRO                           
153700     ELSE                                                                 
153800       MOVE ORAD-KVBEART-Q       TO   OBKR-KVRO                           
153900     END-IF                                                               
154000     MOVE ORAD-KVSLATT           TO   OBKR-KVSLATT                        
154100     MOVE ORAD-PRARTNTO          TO   OBKR-PRARTNTO                       
154200     MOVE ORAD-DEAL-PR-LINE      TO   OBKR-DEAL-PR-LINE                   
154300     MOVE ORAD-PRBPRIS           TO   OBKR-PRBPRIS                        
154400     MOVE ORAD-REKSIFFR          TO   OBKR-REKSIFFR                       
154500     MOVE ZERO                   TO   OBKR-REKSIFFR-TILLK                 
154600     MOVE ORAD-RERF-RAD          TO   OBKR-RERF-RAD                       
154700     IF SPAR-KDORDBEK = 83                                                
154800       MOVE ZERO                 TO   OBKR-TIDISPIN                       
154900     ELSE                                                                 
155000       MOVE WS-TIDISPIN          TO   OBKR-TIDISPIN                       
155100     END-IF                                                               
155200     MOVE OHUV-TIREGDAT          TO   OBKR-TIORDREG                       
155300     MOVE ORAD-TIPRIS            TO   OBKR-TIPRIS                         
155400     MOVE MSGI-TILOKDAT          TO   OBKR-TIREGDAT                       
155500     MOVE MSGI-TILOKTID          TO   WS-TIHHMM                           
155600     MOVE WS-TIHHMMSS            TO   OBKR-TIREGTID                       
155700     IF SPAR-KDORDBEK = 91                                                
155800       MOVE MSGI-TILOKDAT        TO   OBKR-TIRODAT                        
155900     ELSE                                                                 
156000       MOVE ZERO                 TO   OBKR-TIRODAT                        
156100     END-IF                                                               
156200                                                                          
156300     MOVE MSGI-TILOKDAT TO  WS-AAMMDD                                     
156400     IF WS-AAMMDD > 500000                                                
156500       MOVE 19           TO WS-CENTURY                                    
156600     ELSE                                                                 
156700       MOVE 20           TO WS-CENTURY                                    
156800     END-IF                                                               
156900     COMPUTE OBKR-TITIREGD-9KOMPL = WS-9KOMPL - WS-9KOMPL-DATUM           
157000                                                                          
157100     MOVE ORAD-TITPO             TO   OBKR-TITPO                          
157200                                                                          
157300     MOVE OHUV-TIREGDAT   TO  WS-AAMMDD                                   
157400     COMPUTE OBKR-TITIORDD-9KOMPL = WS-9KOMPL - WS-9KOMPL-DATUM           
157500                                                                          
157600     MOVE WS-KDFRAKT(DC-IX)      TO   OBKR-KDFRAKT                        
157700     MOVE OHUV-KDORDKL           TO   OBKR-KDORDKL                        
157800                                                                          
157900     MOVE OHUV-KDORDTYP-LDC      TO OBKR-KDORDTYP-LDC                     
158000     MOVE OHUV-TIREPDAT          TO OBKR-TIREPDAT                         
158100     MOVE ORAD-IDKUNDRF-WIP      TO OBKR-IDKUNDRF-WIP                     
158200     MOVE ZERO                   TO OBKR-TIDLEVDAT                        
158300     MOVE ORAD-PRAVCOST          TO OBKR-PRAVCOST                         
158400     MOVE ORAD-KDVALISO          TO OBKR-KDVALISO                         
158500                                                                          
158600     PERFORM IMS-ISRT-ORQM-ORQM01                                         
158700                                                                          
158800     PERFORM UNTIL SEGMENT-FINNS                                          
158900       ADD 1 TO OBKR-IDSEKVNR                                             
159000       PERFORM IMS-ISRT-ORQM-ORQM01                                       
159100     END-PERFORM                                                          
159200                                                                          
159300     ADD 1 TO WS-ANTOBKR                                                  
159400     ADD +1 TO ANTAL-LAEST                                                
159500     .                                                                    
159600     EJECT                                                                
159700 S03CA-LAES-WDK6 SECTION.                                                 
159800     PERFORM IMS-GU-ARTC-ARTC11                                           
159900     IF SEGMENT-FINNS                                                     
160000       MOVE CLAG-KVQPACK-1 TO WS-KVQPACK-1                                
160100       MOVE CLAG-TIDISPIN  TO WS-TIDISPIN                                 
160200     END-IF                                                               
160300     .                                                                    
160400     EJECT                                                                
160500 S04-ANROPA-AVSO SECTION.                                                 
160600                                                                          
160700     MOVE WS-IDDISTR         TO   AVSO-IDDISTR                            
160800     MOVE WS-IDKUNDNR        TO   AVSO-IDKUNDNR                           
160900     MOVE WS-IDKUNDRF        TO   AVSO-IDKUNDRF                           
161000     MOVE WS-IDORDER         TO   AVSO-IDORDER                            
161100     MOVE SPACE              TO   AVSO-IDDC                               
161200     MOVE ZERO               TO   AVSO-TIRFS                              
161300     MOVE ZERO               TO   AVSO-TIAAMMDD                           
161400     MOVE ZERO               TO   AVSO-TIHHMM                             
161500     MOVE W-IDTRANS          TO   AVSO-IDTRANS                            
161600                                                                          
161700     CALL W413AVSO USING AVSO-W413AVSO                                    
161800          AVSO-WDE6-PCB AVSO-ORQA-PCB                                     
161900          AVSO-WDQ2-PCB AVSO-GMTB-PCB                                     
162000          AVSO-XXKA-PCB AVSO-4437-PCB AVSO-XXKE-PCB                       
162100          AVSO-XXKF-PCB AVSO-XXKG-PCB AVSO-XXKH-PCB                       
162200          AVSO-XXKI-PCB AVSO-XXKP-PCB                                     
162300          AVSO-WDB2-PCB AVSO-WDB6-PCB AVSO-WDP7-PCB                       
162400          TRAN-XXKB-PCB                                                   
162500          ORDN-ORQL-PCB ORDN-PROC-PCB ORDN-ORQI-PCB ORDN-WDQ3-PCB         
162600     .                                                                    
162700     EJECT                                                                
162800 S05-ANNULL-NYVORKO SECTION.                                              
162900                                                                          
163000     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
163100     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
163200     MOVE OHUV-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
163300                                    W-A601KY-MAX-IDDISTR                  
163400     MOVE OHUV-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
163500                                    W-A601KY-MAX-IDKUNDNR                 
163600     MOVE OHUV-IDKUNDRF          TO W-A601KY-MIN-IDKUNDRF                 
163700                                    W-A601KY-MAX-IDKUNDRF                 
163800     MOVE OHUV-TIREGDAT          TO W-A601KY-MIN-TIREGDAT                 
163900                                    W-A601KY-MAX-TIREGDAT                 
164000                                                                          
164100     PERFORM IMS-GHN-WDA6B                                                
164200     PERFORM UNTIL SEGMENT-SAKNAS                                         
164300                OR BASEN-SLUT                                             
164400                                                                          
164500         IF  VOR-KDVORATG > '1'                                           
164600         AND VOR-KDVORATG < '6'                                           
164700             MOVE '8'            TO VOR-KDVORATG                          
164800             MOVE 83             TO VOR-KDORDBEK                          
164900             MOVE 0              TO VOR-KVPREAVB                          
165000             IF VOR-TIKLAR = ZERO                                         
165100                MOVE WS-TINUDAT     TO VOR-TIKLAR                         
165200                COMPUTE VOR-TIKLATID    = WS-TINUTID                      
165300                                        / 100                             
165400                END-COMPUTE                                               
165500             END-IF                                                       
165600             PERFORM IMS-REPL-WDA6B                                       
165700         END-IF                                                           
165800                                                                          
165900         PERFORM IMS-GHN-WDA6B                                            
166000     END-PERFORM                                                          
166100     .                                                                    
166200     EJECT                                                                
166300 S06-ANNULL-NYVORKO-BRIST  SECTION.                                       
166400                                                                          
166500     MOVE LOW-VALUE              TO W-WDA601KY-MIN-X.                     
166600     MOVE HIGH-VALUE             TO W-WDA601KY-MAX-X.                     
166700     MOVE OHUV-IDDISTR           TO W-A601KY-MIN-IDDISTR                  
166800                                    W-A601KY-MAX-IDDISTR                  
166900     MOVE OHUV-IDKUNDNR          TO W-A601KY-MIN-IDKUNDNR                 
167000                                    W-A601KY-MAX-IDKUNDNR                 
167100     MOVE OHUV-IDKUNDRF          TO W-A601KY-MIN-IDKUNDRF                 
167200                                    W-A601KY-MAX-IDKUNDRF                 
167300     MOVE OHUV-TIREGDAT          TO W-A601KY-MIN-TIREGDAT                 
167400                                    W-A601KY-MAX-TIREGDAT                 
167500                                                                          
167600     PERFORM IMS-GHN-WDA6A                                                
167700     PERFORM UNTIL SEGMENT-SAKNAS                                         
167800                OR BASEN-SLUT                                             
167900                                                                          
168000         IF  VOR-KDVORATG < '2'                                           
168100             MOVE '8'            TO VOR-KDVORATG                          
168200             MOVE 83             TO VOR-KDORDBEK                          
168300             IF VOR-TIKLAR = ZERO                                         
168400                MOVE WS-TINUDAT     TO VOR-TIKLAR                         
168500                COMPUTE VOR-TIKLATID    = WS-TINUTID                      
168600                                        / 100                             
168700                END-COMPUTE                                               
168800             END-IF                                                       
168900             MOVE VOR-IDARTNR    TO W-IDARTNR                             
169000             PERFORM IMS-GHU-ARTC-ARTC11                                  
169100             IF SEGMENT-FINNS                                             
169200               COMPUTE CLAG-KVVORKO  = CLAG-KVVORKO                       
169300                                     - VOR-KVBEART-Q                      
169400                                     + VOR-KVPREAVB                       
169500               END-COMPUTE                                                
169600                                                                          
169700               PERFORM IMS-REPL-ARTC                                      
169800             END-IF                                                       
169900             PERFORM IMS-REPL-WDA6A                                       
170000         END-IF                                                           
170100                                                                          
170200         PERFORM IMS-GHN-WDA6A                                            
170300     END-PERFORM                                                          
170400     .                                                                    
170500     EJECT                                                                
170600 S11-SKAPA-TRANSAR SECTION.                                               
170700                                                                          
170800     IF WS-IDTRANS = '4213' OR '4223' OR '4233' OR '4243'                 
170900       CONTINUE                                                           
171000     ELSE                                                                 
171100       IF ORAD-IDKUNDRF-RO = '0000000   ' OR                              
171200         ORAD-IDKUNDRF-RO = ORAD-IDKUNDRF                                 
171300                                                                          
171400         IF ORAD-KDOI NOT = SPACE                                         
171500           PERFORM S11A-SKAPA-2109                                        
171600         END-IF                                                           
171700                                                                          
171800         PERFORM S11B-SKAPA-RYC                                           
171900       ELSE                                                               
172000                                                                          
172100         PERFORM S11C-SKAPA-RYB                                           
172200       END-IF                                                             
172300     END-IF                                                               
172400     .                                                                    
172500     EJECT                                                                
172600 S11A-SKAPA-2109 SECTION.                                                 
172700                                                                          
172800*    FÖR BYYTESARTIKLAR SKALL INGEN ORDERINGÅNG SKAPAS                    
172900     MOVE ORAD-IDARTNR       TO BYT03-IDARTNR                             
173000     IF NOT BYT03-OBJEKT                                                  
173100                                                                          
173200        MOVE 2109-IX            TO 2109-MID2-KVANTART                     
173300        MOVE ORAD-IDARTNR       TO 2109-MID2-IDARTNR (2109-IX)            
173400        MOVE OHUV-IDDC-PRIM     TO 2109-MID2-IDDC (2109-IX)               
173500        MOVE '-'                TO 2109-MID2-KDTECKEN(2109-IX)            
173600        MOVE ORAD-KDOI          TO 2109-MID2-KDOI (2109-IX)               
173700        MOVE ORAD-CLEARGROUP    TO 2109-MID2-CLEARGROUP(2109-IX)          
173800        MOVE ORAD-KVBEART-Q     TO 2109-MID2-KVOI (2109-IX)               
173900        MOVE ORAD-TIREGDAT      TO 2109-MID2-TIUPPDAT (2109-IX)           
174000                                                                          
174100        ADD 1                 TO   2109-IX                                
174200        IF 2109-IX > MAX-2109-IX                                          
174300           PERFORM S14-PPSW-2109                                          
174400           MOVE ZERO          TO   2109-MID2-KVANTART                     
174500        END-IF                                                            
174600     END-IF                                                               
174700     .                                                                    
174800     EJECT                                                                
174900 S11B-SKAPA-RYC SECTION.                                                  
175000     MOVE 'RYC'               TO   W-RYC-IDPTYP                           
175100     MOVE ORAD-BERADREF       TO   W-RYC-BERADREF                         
175200     MOVE ORAD-BEVOLREF       TO   W-RYC-BEVOLREF                         
175300     MOVE ORAD-FLTILLK        TO   W-RYC-FLTILLK                          
175400     MOVE ORAD-IDARTNR        TO   W-RYC-IDARTNR                          
175500     MOVE ORAD-IDDISTR        TO   W-RYC-IDDISTR                          
175600     MOVE ORAD-IDKUNDNR       TO   W-RYC-IDKUNDNR                         
175700     MOVE ORAD-IDKUNDRF       TO   W-RYC-IDKUNDRF                         
175800     MOVE ORAD-IDKUNDRF-RO    TO   W-RYC-IDKUNDRF-RO                      
175900     MOVE ORAD-KDDSP          TO   W-RYC-KDDSP                            
176000     MOVE OHUV-KDFAKTYP       TO   W-RYC-KDFAKTYP                         
176100     MOVE WS-KDFRAKT(DC-IX)   TO   W-RYC-KDFRAKT                          
176200     MOVE 83                  TO   W-RYC-KDORDBEK                         
176300     MOVE OHUV-KDORDKL        TO   W-RYC-KDORDKL                          
176400     MOVE ORAD-KDKVBRYT       TO   W-RYC-KDKVBRYT                         
176500     MOVE ORAD-KDVRINFO       TO   W-RYC-KDVRINFO                         
176600     MOVE 0                   TO   W-RYC-KDVRTPO                          
176700     MOVE ORAD-KVBEART-Q      TO   W-RYC-KVANNANT                         
176800     MOVE ORAD-REKSIFFR       TO   W-RYC-REKSIFFR                         
176900     MOVE OHUV-TIREGDAT       TO   W-RYC-TIORDREG                         
177000     MOVE ORAD-TIRODAT        TO   W-RYC-TIRODAT                          
177100     ACCEPT TIAAMMDD FROM DATE                                            
177200     ACCEPT TIKLOCK FROM TIME                                             
177300     MOVE +1 TO IDLOGLOP                                                  
177400     MOVE 'RYC' TO IDPTYP                                                 
177500     MOVE W-RYCPOST TO LOGGPOST                                           
177600                                                                          
177700     MOVE SPACE               TO W-RYCS-WDGZRYCS                          
177800     MOVE ORAD-IDDC           TO W-RYCS-IDDC                              
177900     MOVE W-RYCSPOST TO SORTPOST                                          
178000                                                                          
178100     PERFORM IMS-ISRT-ZZAC-ZZAC01                                         
178200     ADD +1 TO ANTAL-LAEST                                                
178300     PERFORM UNTIL SEGMENT-FINNS                                          
178400       ADD +1 TO IDLOGLOP                                                 
178500       IF IDLOGLOP = 0                                                    
178600          ADD +1 TO IDLOGLOP                                              
178700                    TIKLOCK                                               
178800       END-IF                                                             
178900       PERFORM IMS-ISRT-ZZAC-ZZAC01                                       
179000       ADD +1 TO ANTAL-LAEST                                              
179100     END-PERFORM                                                          
179200     .                                                                    
179300     EJECT                                                                
179310 S11C-SKAPA-RYB SECTION.                                                  
179320     MOVE 'RYB'               TO   W-RYB-IDPTYP                           
179330     MOVE ORAD-BERADREF       TO   W-RYB-BERADREF                         
179340     MOVE ORAD-BEVOLREF       TO   W-RYB-BEVOLREF                         
179350     MOVE ORAD-FLTILLK        TO   W-RYB-FLTILLK                          
179360     MOVE ORAD-IDARTNR        TO   W-RYB-IDARTNR                          
179370     MOVE ORAD-IDDISTR        TO   W-RYB-IDDISTR                          
179380     MOVE ORAD-IDKUNDNR       TO   W-RYB-IDKUNDNR                         
179390     MOVE ORAD-IDKUNDRF       TO   W-RYB-IDKUNDRF                         
179400     MOVE ORAD-IDKUNDRF-RO    TO   W-RYB-IDKUNDRF-RO                      
179500     MOVE ORAD-IDDC           TO   W-RYB-IDDC                             
179600     MOVE ORAD-KDDSP          TO   W-RYB-KDDSP                            
179700     MOVE OHUV-KDFAKTYP       TO   W-RYB-KDFAKTYP                         
179800     MOVE ARB-KDFRAKT         TO   W-RYB-KDFRAKT                          
179900     MOVE +1                  TO   W-RYB-KDLIDEL                          
180000     MOVE 91                  TO   W-RYB-KDORDBEK                         
180100     MOVE ORAD-KDORDKL        TO   W-RYB-KDORDKL                          
180200     MOVE ORAD-KDKVBRYT       TO   W-RYB-KDKVBRYT                         
180300     MOVE +1                  TO   W-RYB-KDRO                             
180400     MOVE ORAD-KDVRINFO       TO   W-RYB-KDVRINFO                         
180500     MOVE ORAD-KVBEART-Q      TO   W-RYB-KVRO                             
180600     MOVE ORAD-REKSIFFR       TO   W-RYB-REKSIFFR                         
180700     MOVE WS-TIDISPIN         TO   W-RYB-TIDISPIN                         
180800     MOVE OHUV-TIREGDAT       TO   W-RYB-TIORDREG                         
180900     MOVE ORAD-TIRODAT        TO   W-RYB-TIRODAT                          
181000     ACCEPT TIAAMMDD FROM DATE                                            
181100     ACCEPT TIKLOCK FROM TIME                                             
181200     MOVE +1 TO IDLOGLOP                                                  
181300     MOVE 'RYB' TO IDPTYP                                                 
181400     MOVE W-RYBPOST TO LOGGPOST                                           
181500     MOVE SPACE TO SORTPOST                                               
181600     PERFORM IMS-ISRT-ZZAC-ZZAC01                                         
181700     PERFORM UNTIL SEGMENT-FINNS                                          
181800       ADD +1 TO IDLOGLOP                                                 
181900       IF IDLOGLOP = 0                                                    
182000          ADD +1 TO IDLOGLOP                                              
182100                    TIKLOCK                                               
182200       END-IF                                                             
182300       PERFORM IMS-ISRT-ZZAC-ZZAC01                                       
182400     END-PERFORM                                                          
182500     .                                                                    
182600     EJECT                                                                
182700 S13-BACKA-KAMPANJ SECTION.                                               
182800     MOVE ORAD-IDKAMPRF TO W-KAMP-IDKAMPRF                                
182900     MOVE ORAD-IDDC     TO W-KAMP-IDDC                                    
183000     MOVE ORAD-IDARTNR  TO W-KART-IDARTNR                                 
183100                                                                          
183200     PERFORM IMS-GHU-WDM211                                               
183300     IF SEGMENT-FINNS                                                     
183400       COMPUTE KART-KVBEART-KUND = KART-KVBEART-KUND                      
183500                                 - ORAD-KVBEART-Q                         
183600       IF KART-KVBEART-KUND < ZERO                                        
183700         MOVE 'ANTAL SALDO NEGATIV - ABEND' TO FELTEXT                    
183800         CALL ABEND USING RKOD-ABEND                                      
183900       ELSE                                                               
184000         PERFORM IMS-REPL-WDM211                                          
184100       END-IF                                                             
184200     ELSE                                                                 
184300       MOVE 'WDR430 SAKNAS - ABEND' TO FELTEXT                            
184400       CALL ABEND USING RKOD-ABEND                                        
184500     END-IF                                                               
184600                                                                          
184700     MOVE ORAD-IDDISTR  TO W-KMRK-IDDISTR-FOM                             
184800     MOVE ORAD-IDDISTR  TO W-KMRK-IDDISTR-TOM                             
184900     MOVE ORAD-IDKUNDNR TO W-KMRK-IDKUNDNR-FOM                            
185000     MOVE ORAD-IDKUNDNR TO W-KMRK-IDKUNDNR-TOM                            
185100                                                                          
185200     PERFORM S20-FINN-INTERVALL                                           
185300                                                                          
185400     PERFORM IMS-GHU-WDM221                                               
185500     IF SEGMENT-FINNS                                                     
185600       COMPUTE KMRK-KVBEART-KUND = KMRK-KVBEART-KUND                      
185700                                 - ORAD-KVBEART-Q                         
185800       IF KMRK-KVBEART-KUND < ZERO                                        
185900         MOVE 'WDM221 - ANTALSTABELL NEGATIV- ABEND' TO FELTEXT           
186000         CALL ABEND USING RKOD-ABEND                                      
186100       ELSE                                                               
186200         PERFORM IMS-REPL-WDM221                                          
186300       END-IF                                                             
186400     END-IF                                                               
186500                                                                          
186600     .                                                                    
186700     EJECT                                                                
186800 S14-PPSW-2109     SECTION.                                               
186900                                                                          
187000     COMPUTE MSG-KVLL = LENGTH OF 2109-MID2-W2I10902 + 17                 
187100     MOVE 'W2T109X '            TO MSG-KDTRANS-1                          
187200     MOVE '4292'                TO MSG-IDTRANS-1                          
187300     MOVE '1'                   TO MSG-KDMFSFOR-1                         
187400     MOVE 2109-MID2-W2I10902    TO MSG-INDATA-MINUS-1-TRANSKOD            
187500                                                                          
187600     PERFORM IMS-PURG-ALTMSG-2109                                         
187700                                                                          
187800     MOVE SPACE              TO 2109-MID2-W2I10902                        
187900     MOVE +1                 TO 2109-IX                                   
188000     .                                                                    
188100     EJECT                                                                
188200 S17A-FIXA-REFILL-TRANSFER SECTION.                                       
188300                                                                          
188400******************************************************************        
188500*                                                                         
188600*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
188700*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
188800*                                                                         
188900******************************************************************        
189000                                                                          
189100     MOVE OBKR-IDDISTR       TO W-TP4TRAN-IDDISTR                         
189200                                                                          
189300     PERFORM DB2-SELECT-TP4TRAN                                           
189400                                                                          
189500     IF DIST35-REFILL           OR                                        
189600        DIST35-REFILL-INOM-NDC  OR                                        
189700        DIST35-NONVCC-NONVCC-REFILL       OR                              
189800        DIST35-NONVCC-NONVCC-TRANSFER OR                                  
189900        DIST35-NA-TRANSFER      OR                                        
190000        DIST35-NA-NDC-RETURNS   OR                                        
190100        DIST35-PACIFIC-TRANSFER OR                                        
190200        DIST35-REFILL-INOM-JP   OR                                        
190300        DIST35-CN-TRANSFER      OR                                        
190400        DIST35-NONVCC-VCC-REFILL OR                                       
190500        DIST35-NONVCC-VCC-TRANSFER OR                                     
190600        RADER-FINNS                                                       
190700                                                                          
190800       IF RADER-FINNS                                                     
190900         MOVE TP4TRAN-IDDC-REC                                            
191000                             TO W-IDDC                                    
191100       ELSE                                                               
191200                                                                          
191300         SEARCH ALL DIST57-REFILL-DC                                      
191400           AT END                                                         
191500             MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                         
191600                               TO FELTEXT                                 
191700             CALL FELLOG                                                  
191800           WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR              
191900             MOVE DIST57-REFILL-TO-DC(DIST57-IX)                          
192000                               TO W-IDDC                                  
192100         END-SEARCH                                                       
192200       END-IF                                                             
192300                                                                          
192400       PERFORM IMS-GHU-ARTS-ARTS11                                        
192500       COMPUTE SLAG-KVBEART = SLAG-KVBEART - ANNULLERAT-ANTAL             
192600       PERFORM IMS-REPL-ARTS                                              
192700                                                                          
192800       MOVE ORAD-IDARTNR   TO W-IDARTNR                                   
192900       PERFORM IMS-GHU-ARTC-ARTC11                                        
193000       MOVE 1                TO IX-CD-OMR                                 
193100       PERFORM UNTIL IX-CD-OMR > 4                                        
193200       OR ORAD-ADLAGOMR = CLAG-ADLAGOMR-CD (IX-CD-OMR)                    
193300         ADD 1               TO IX-CD-OMR                                 
193400       END-PERFORM                                                        
193500       IF IX-CD-OMR <= 4                                                  
193600*                                                                         
193700*BOKA UPP CROSS DOCKING SALDO                                             
193800*                                                                         
193900          ADD ANNULLERAT-ANTAL                                            
194000                           TO CLAG-KVLS-CD (IX-CD-OMR)                    
194100       END-IF                                                             
194200       PERFORM IMS-REPL-ARTC                                              
194300     ELSE                                                                 
194400        IF DIST35-NONVCC-CDC-REFILL                                       
194500                                                                          
194600           SEARCH ALL DIST57-REFILL-DC                                    
194610             AT END                                                       
194620              MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                        
194630                               TO FELTEXT                                 
194640              CALL FELLOG                                                 
194650             WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR            
194660              MOVE DIST57-REFILL-TO-DC(DIST57-IX)                         
194670                               TO W-IDDC                                  
194680           END-SEARCH                                                     
194690                                                                          
194691           PERFORM IMS-GHU-ARTC-ARTC11                                    
194692           COMPUTE CLAG-KVBEART = CLAG-KVBEART - ANNULLERAT-ANTAL         
194693           PERFORM IMS-REPL-ARTC                                          
194694        END-IF                                                            
194695     END-IF                                                               
194696     .                                                                    
194697     EJECT                                                                
194698 S17B-FIXA-REFILL-TRANSFER SECTION.                                       
194699                                                                          
194700******************************************************************        
194800*                                                                         
194900*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
195000*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
195100*                                                                         
195200******************************************************************        
195300                                                                          
195400     MOVE ORAD-IDDISTR       TO W-TP4TRAN-IDDISTR                         
195500                                                                          
195600     PERFORM DB2-SELECT-TP4TRAN                                           
195700                                                                          
195800     IF DIST35-REFILL           OR                                        
195900        DIST35-REFILL-INOM-NDC  OR                                        
196000        DIST35-NONVCC-NONVCC-REFILL       OR                              
196100        DIST35-NONVCC-NONVCC-TRANSFER OR                                  
196200        DIST35-NA-TRANSFER      OR                                        
196300        DIST35-NA-NDC-RETURNS   OR                                        
196400        DIST35-PACIFIC-TRANSFER OR                                        
196500        DIST35-REFILL-INOM-JP   OR                                        
196600        DIST35-CN-TRANSFER      OR                                        
196601        DIST35-NONVCC-VCC-REFILL OR                                       
196602        DIST35-NONVCC-VCC-TRANSFER OR                                     
196603        RADER-FINNS                                                       
196604                                                                          
196605       IF RADER-FINNS                                                     
196606         MOVE TP4TRAN-IDDC-REC                                            
196607                             TO W-IDDC                                    
196608       ELSE                                                               
196609                                                                          
196610         SEARCH ALL DIST57-REFILL-DC                                      
196620           AT END                                                         
196630             MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                         
196640                               TO FELTEXT                                 
196650             CALL FELLOG                                                  
196660           WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR              
196670             MOVE DIST57-REFILL-TO-DC(DIST57-IX)                          
196680                               TO W-IDDC                                  
196690         END-SEARCH                                                       
196700       END-IF                                                             
196800                                                                          
196900       PERFORM IMS-GHU-ARTS-ARTS11                                        
197000       COMPUTE SLAG-KVBEART = SLAG-KVBEART - ANNULLERAT-ANTAL             
197100       PERFORM IMS-REPL-ARTS                                              
197200                                                                          
197300       MOVE ORAD-IDARTNR   TO W-IDARTNR                                   
197400       PERFORM IMS-GHU-ARTC-ARTC11                                        
197500       MOVE 1                TO IX-CD-OMR                                 
197600       PERFORM UNTIL IX-CD-OMR > 4                                        
197700       OR ORAD-ADLAGOMR = CLAG-ADLAGOMR-CD (IX-CD-OMR)                    
197800         ADD 1               TO IX-CD-OMR                                 
197900       END-PERFORM                                                        
198000       IF IX-CD-OMR <= 4                                                  
198100*                                                                         
198200*BOKA UPP CROSS DOCKING SALDO                                             
198300*                                                                         
198400          ADD ANNULLERAT-ANTAL                                            
198500                           TO CLAG-KVLS-CD (IX-CD-OMR)                    
198600       END-IF                                                             
198700       PERFORM IMS-REPL-ARTC                                              
198800     ELSE                                                                 
198900        IF DIST35-NONVCC-CDC-REFILL                                       
199000                                                                          
199100           SEARCH ALL DIST57-REFILL-DC                                    
199200            AT END                                                        
199300             MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                         
199400                               TO FELTEXT                                 
199500             CALL FELLOG                                                  
199600            WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR             
199700             MOVE DIST57-REFILL-TO-DC(DIST57-IX)                          
199800                               TO W-IDDC                                  
199900           END-SEARCH                                                     
200000                                                                          
200100           PERFORM IMS-GHU-ARTC-ARTC11                                    
200200           COMPUTE CLAG-KVBEART = CLAG-KVBEART - ANNULLERAT-ANTAL         
200300           PERFORM IMS-REPL-ARTC                                          
200310        END-IF                                                            
200320     END-IF                                                               
200330     .                                                                    
200340     EJECT                                                                
200350 S20-FINN-INTERVALL SECTION.                                              
200360                                                                          
200370     PERFORM IMS-GU-WDM211                                                
200380     IF SEGMENT-FINNS                                                     
200390       PERFORM IMS-GNP-WDM221                                             
200400       PERFORM UNTIL SEGMENT-SAKNAS                                       
200500         IF  ORAD-IDDISTR > KMRK-IDDISTR-TOM                              
200600         OR  ORAD-IDDISTR < KMRK-IDDISTR-FOM                              
200700           CONTINUE                                                       
200800         ELSE                                                             
200900           IF  ORAD-IDDISTR  = KMRK-IDDISTR-TOM                           
201000           AND ORAD-IDKUNDNR > KMRK-IDKUNDNR-TOM                          
201100             CONTINUE                                                     
201200           ELSE                                                           
201300             IF  ORAD-IDDISTR  = KMRK-IDDISTR-FOM                         
201400             AND ORAD-IDKUNDNR < KMRK-IDKUNDNR-FOM                        
201500               CONTINUE                                                   
201600             ELSE                                                         
201700               MOVE KMRK-IDDISTR-FOM  TO W-KMRK-IDDISTR-FOM               
201800               MOVE KMRK-IDDISTR-TOM  TO W-KMRK-IDDISTR-TOM               
201900               MOVE KMRK-IDKUNDNR-FOM TO W-KMRK-IDKUNDNR-FOM              
202000               MOVE KMRK-IDKUNDNR-TOM TO W-KMRK-IDKUNDNR-TOM              
202100             END-IF                                                       
202200           END-IF                                                         
202300         END-IF                                                           
202400         PERFORM IMS-GNP-WDM221                                           
202500       END-PERFORM                                                        
202600     END-IF                                                               
202700     .                                                                    
202800     EJECT                                                                
202900                                                                          
203000 S23-DELETE-PRICE-Q-ORDER SECTION.                                        
203100                                                                          
203200*TL:MÖJLIGT PROBLEM!, VI VET INTE OM DET FINNS EN PRISFRÅGA PÅ            
203300*   ORDERN EFTERSOM VI INTE HAR LÄST Q4 ÄN. DÅ GÖR MAN                    
203400*   CALL PÅ W335PRQU MED 0 I ALLA NYCKLAR.                                
203500                                                                          
203600     IF DIST79-DEALER-PRICE                                               
203700         INITIALIZE PRQU-W335PRQU                                         
203800         MOVE OHUV-IDDISTR       TO PRQU-IDDISTR                          
203900         MOVE OHUV-IDKUNDNR      TO PRQU-IDKUNDNR                         
204000         MOVE OHUV-IDKUNDRF      TO PRQU-IDKUNDRF                         
204100         MOVE 3                  TO PRQU-KDCALL                           
204200         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
204300                                            PRQU-WDC7-PCB                 
204400                                            PRQU-SJKO-WDK6-PCB            
204500     END-IF                                                               
204600     .                                                                    
204700     EJECT                                                                
204800* --- IMS SEKTIONER ---                                                   
204900     SKIP3                                                                
205000 IMS-GET-MSG SECTION.                                                     
205100                                                                          
205200     MOVE '  QC' TO GODK-STATUSKODER                                      
205300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
205400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
205500     PERFORM IMS-STATUSKONTROLL                                           
205600     .                                                                    
205700     SKIP3                                                                
205800                                                                          
205900 IMS-INSERT-ALTMSG-4292 SECTION.                                          
206000                                                                          
206100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
206200     MOVE SPACE TO GODK-STATUSKODER                                       
206300     CALL CBLTDLI USING ISRT ALT1-PCB MSG-IO-AREA                         
206400     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
206500     PERFORM IMS-STATUSKONTROLL                                           
206600     .                                                                    
206700     SKIP2                                                                
206800                                                                          
206900 IMS-PURG-ALTMSG-2109 SECTION.                                            
207000                                                                          
207100     MOVE    '  '             TO   GODK-STATUSKODER                       
207200     CALL    CBLTDLI         USING PURG 2109-PCB MSG-IO-AREA              
207300     MOVE    2109-STATUS-CODE TO   STATUS-WS                              
207400     PERFORM IMS-STATUSKONTROLL                                           
207500     .                                                                    
207600     EJECT                                                                
207700                                                                          
207800 IMS-GU-ARTC-ARTC11 SECTION.                                              
207900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
208000          DELIMITED BY SIZE INTO SSA1                                     
208100     MOVE 'WLARTC11 ' TO SSA2                                             
208200     MOVE '  GE' TO GODK-STATUSKODER                                      
208300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-ARTC11 SSA1 SSA2          
208400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
208500     PERFORM IMS-STATUSKONTROLL                                           
208600     .                                                                    
208700     EJECT                                                                
208800 IMS-GHU-ARTC-ARTC11 SECTION.                                             
208900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
209000          DELIMITED BY SIZE INTO SSA1                                     
209100     MOVE 'WLARTC11 ' TO SSA2                                             
209200     MOVE '  GE' TO GODK-STATUSKODER                                      
209300     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-ARTC11 SSA1 SSA2         
209400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
209500     PERFORM IMS-STATUSKONTROLL                                           
209600     .                                                                    
209700     EJECT                                                                
209800 IMS-REPL-ARTC       SECTION.                                             
209900     MOVE '  '   TO GODK-STATUSKODER                                      
210000     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-ARTC11                  
210100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
210200     PERFORM IMS-STATUSKONTROLL                                           
210300     .                                                                    
210400     SKIP2                                                                
210500                                                                          
210600 IMS-GHU-ARTM-ARTM01 SECTION.                                             
210700                                                                          
210800     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
210900          DELIMITED BY SIZE INTO SSA1                                     
211000     MOVE '  GE' TO GODK-STATUSKODER                                      
211100     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA-ARTM SSA1                
211200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
211300     PERFORM IMS-STATUSKONTROLL                                           
211400     .                                                                    
211500                                                                          
211600 IMS-REPL-ARTM SECTION.                                                   
211700                                                                          
211800     MOVE '  ' TO GODK-STATUSKODER                                        
211900     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA-ARTM                    
212000     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
212100     PERFORM IMS-STATUSKONTROLL                                           
212200     .                                                                    
212300     EJECT                                                                
212400 IMS-GHU-ARTS-ARTS11 SECTION.                                             
212500                                                                          
212600     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
212700          DELIMITED BY SIZE INTO SSA1                                     
212800     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
212900          DELIMITED BY SIZE INTO SSA2                                     
213000     MOVE '    ' TO GODK-STATUSKODER                                      
213100     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-AREA-ARTS SSA1 SSA2           
213200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
213300     PERFORM IMS-STATUSKONTROLL                                           
213400     .                                                                    
213500                                                                          
213600 IMS-REPL-ARTS SECTION.                                                   
213700                                                                          
213800     MOVE '  ' TO GODK-STATUSKODER                                        
213900     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-AREA-ARTS                    
214000     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
214100     PERFORM IMS-STATUSKONTROLL                                           
214200     .                                                                    
214300     EJECT                                                                
214400 IMS-GHU-ORDP-ORDP01 SECTION.                                             
214500     STRING 'WLORDP01(WDA501KY =' W-WDA501KY-X                            
214600                    '&IDDC     =' W-RAD-IDDC-X ')'                        
214700          DELIMITED BY SIZE INTO SSA1                                     
214800     MOVE '  GE' TO GODK-STATUSKODER                                      
214900     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-AREA-ORDP SSA1                
215000     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
215100     PERFORM IMS-STATUSKONTROLL                                           
215200     .                                                                    
215300     EJECT                                                                
215400 IMS-REPL-ORDP SECTION.                                                   
215500     MOVE '  ' TO GODK-STATUSKODER                                        
215600     CALL CBLTDLI USING REPL ORDP-PCB DLI-IO-AREA-ORDP                    
215700     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
215800     PERFORM IMS-STATUSKONTROLL                                           
215900     .                                                                    
216000     EJECT                                                                
216100 IMS-GHU-ORQF-ORQF01 SECTION.                                             
216200     STRING 'WLORQF01(WDQ401KY>=' W-WDQ401KY-MIN-X                        
216300                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
216400          DELIMITED BY SIZE INTO SSA1                                     
216500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
216600     CALL CBLTDLI USING GHU ORQF-PCB DLI-IO-AREA-ORAD SSA1                
216700     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
216800     PERFORM IMS-STATUSKONTROLL                                           
216900     .                                                                    
217000     EJECT                                                                
217100 IMS-GHN-ORQF-ORQF01 SECTION.                                             
217200     STRING 'WLORQF01(WDQ401KY >' W-WDQ401KY-MIN-X                        
217300                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
217400          DELIMITED BY SIZE INTO SSA1                                     
217500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
217600     CALL CBLTDLI USING GHN ORQF-PCB DLI-IO-AREA-ORAD SSA1                
217700     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
217800     PERFORM IMS-STATUSKONTROLL                                           
217900     .                                                                    
218000     EJECT                                                                
218100 IMS-DLET-ORQF SECTION.                                                   
218200                                                                          
218300     MOVE '  ' TO GODK-STATUSKODER                                        
218400     CALL CBLTDLI USING DLET ORQF-PCB DLI-IO-AREA-ORAD                    
218500     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
218600     PERFORM IMS-STATUSKONTROLL                                           
218700     .                                                                    
218800     EJECT                                                                
218900 IMS-GHU-ORQI-ORQI01 SECTION.                                             
219000     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
219100          DELIMITED BY SIZE INTO SSA1                                     
219200     MOVE '  GE' TO GODK-STATUSKODER                                      
219300     CALL CBLTDLI USING GHU ORQI-PCB DLI-IO-AREA-OHUV SSA1                
219400     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
219500     PERFORM IMS-STATUSKONTROLL                                           
219600     .                                                                    
219700     EJECT                                                                
219800                                                                          
219900 IMS-GNP-ORQI-ORQI12 SECTION.                                             
220000                                                                          
220100     MOVE 'WLORQI12 ' TO SSA1                                             
220200     MOVE '  GE' TO GODK-STATUSKODER                                      
220300     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-AREA-ARB SSA1                 
220400     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
220500     PERFORM IMS-STATUSKONTROLL                                           
220600     .                                                                    
220700     EJECT                                                                
220800                                                                          
220900 IMS-REPL-ORQI SECTION.                                                   
221000                                                                          
221100     MOVE '  ' TO GODK-STATUSKODER                                        
221200     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-AREA-OHUV                    
221300     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
221400     PERFORM IMS-STATUSKONTROLL                                           
221500     .                                                                    
221600     EJECT                                                                
221700 IMS-DLET-ORQI SECTION.                                                   
221800                                                                          
221900     MOVE '  ' TO GODK-STATUSKODER                                        
222000     CALL CBLTDLI USING DLET ORQI-PCB DLI-IO-AREA-OHUV                    
222100     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
222200     PERFORM IMS-STATUSKONTROLL                                           
222300     .                                                                    
222400     EJECT                                                                
222500 IMS-GU-ORQM-ORQM01 SECTION.                                              
222600     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
222700                    '&WDQ101KY<=' W-WDQ101KY-MAX-X ')'                    
222800          DELIMITED BY SIZE INTO SSA1                                     
222900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
223000     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-AREA-OBKR SSA1                 
223100     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
223200     PERFORM IMS-STATUSKONTROLL                                           
223300     .                                                                    
223400     EJECT                                                                
223500 IMS-GHU-ORQM-ORQM01 SECTION.                                             
223600     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-MIN-X                        
223700                    '&WDQ101KY<=' W-WDQ101KY-MAX-X ')'                    
223800          DELIMITED BY SIZE INTO SSA1                                     
223900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
224000     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-OBKR SSA1                
224100     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
224200     PERFORM IMS-STATUSKONTROLL                                           
224300     .                                                                    
224400     EJECT                                                                
224500 IMS-GHN-ORQM-ORQM01 SECTION.                                             
224600     STRING 'WLORQM01(WDQ101KY >' W-WDQ101KY-MIN-X                        
224700                    '&WDQ101KY<=' W-WDQ101KY-MAX-X ')'                    
224800          DELIMITED BY SIZE INTO SSA1                                     
224900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
225000     CALL CBLTDLI USING GHN ORQM-PCB DLI-IO-AREA-OBKR SSA1                
225100     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
225200     PERFORM IMS-STATUSKONTROLL                                           
225300     .                                                                    
225400     EJECT                                                                
225500 IMS-ISRT-ORQM-ORQM01 SECTION.                                            
225600                                                                          
225700     MOVE 'WLORQM01 ' TO SSA1                                             
225800     MOVE '  II' TO GODK-STATUSKODER                                      
225900     CALL CBLTDLI USING ISRT ORQM-PCB DLI-IO-AREA-OBKR SSA1               
226000     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
226100     PERFORM IMS-STATUSKONTROLL                                           
226200     .                                                                    
226300                                                                          
226400 IMS-DLET-ORQM SECTION.                                                   
226500                                                                          
226600     MOVE '  ' TO GODK-STATUSKODER                                        
226700     CALL CBLTDLI USING DLET ORQM-PCB DLI-IO-AREA-OBKR                    
226800     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
226900     PERFORM IMS-STATUSKONTROLL                                           
227000     .                                                                    
227100     EJECT                                                                
227200 IMS-GHU-4541-454111 SECTION.                                             
227300     STRING 'WDR401  (WDGXKEY  =' W-IDHTYP-X ')'                          
227400          DELIMITED BY SIZE INTO SSA1                                     
227500     STRING 'WDGX4542(KY4542  >=' W-WDGXKEY-MIN-X                         
227600                    '&KY4542  <=' W-WDGXKEY-MAX-X                         
227700                    '&KDVORATG <' W-KDVORATG-X ')'                        
227800          DELIMITED BY SIZE INTO SSA2                                     
227900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
228000     CALL CBLTDLI USING GHU 4541-PCB DLI-IO-AREA-4542 SSA1 SSA2           
228100     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
228200     PERFORM IMS-STATUSKONTROLL                                           
228300     .                                                                    
228400     EJECT                                                                
228500 IMS-GHN-4541-454111 SECTION.                                             
228600     STRING 'WDR401  (WDGXKEY  =' W-IDHTYP-X ')'                          
228700          DELIMITED BY SIZE INTO SSA1                                     
228800     STRING 'WDGX4542(KY4542   >' W-WDGXKEY-MIN-X                         
228900                    '&KY4542  <=' W-WDGXKEY-MAX-X                         
229000                    '&KDVORATG <' W-KDVORATG-X ')'                        
229100          DELIMITED BY SIZE INTO SSA2                                     
229200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
229300     CALL CBLTDLI USING GHN 4541-PCB DLI-IO-AREA-4542 SSA1 SSA2           
229400     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
229500     PERFORM IMS-STATUSKONTROLL                                           
229600     .                                                                    
229700     EJECT                                                                
229800 IMS-DLET-4541 SECTION.                                                   
229900                                                                          
230000     MOVE '  ' TO GODK-STATUSKODER                                        
230100     CALL CBLTDLI USING DLET 4541-PCB DLI-IO-AREA-4542                    
230200     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
230300     PERFORM IMS-STATUSKONTROLL                                           
230400     .                                                                    
230500     EJECT                                                                
230600 IMS-ISRT-ZZAC-ZZAC01 SECTION.                                            
230700                                                                          
230800     MOVE 'WLZZAC01 ' TO SSA1                                             
230900     MOVE '  II' TO GODK-STATUSKODER                                      
231000     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA-ZZAC SSA1               
231100     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
231200     PERFORM IMS-STATUSKONTROLL                                           
231300     .                                                                    
231400                                                                          
231500 IMS-GU-WDM211 SECTION.                                                   
231600                                                                          
231700     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
231800          DELIMITED BY SIZE INTO SSA1                                     
231900     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
232000          DELIMITED BY SIZE INTO SSA2                                     
232100     MOVE '  GE'              TO GODK-STATUSKODER                         
232200     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2               
232210     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
232211     PERFORM IMS-STATUSKONTROLL                                           
232212     .                                                                    
232213                                                                          
232214 IMS-GNP-WDM221 SECTION.                                                  
232215                                                                          
232216     MOVE 'WDM221 '           TO SSA1                                     
232217     MOVE '    GE'            TO GODK-STATUSKODER                         
232218     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221 SSA1                   
232219     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
232220     PERFORM IMS-STATUSKONTROLL                                           
232221     .                                                                    
232222                                                                          
232223 IMS-GHU-WDM211 SECTION.                                                  
232224                                                                          
232225     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
232226          DELIMITED BY SIZE INTO SSA1                                     
232227     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
232228          DELIMITED BY SIZE INTO SSA2                                     
232229     MOVE '  GE'              TO GODK-STATUSKODER                         
232230     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2              
232240     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
232250     PERFORM IMS-STATUSKONTROLL                                           
232260     .                                                                    
232270                                                                          
232280 IMS-REPL-WDM211 SECTION.                                                 
232290                                                                          
232291     MOVE '  '             TO GODK-STATUSKODER                            
232292     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM211                       
232293     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
232294     PERFORM IMS-STATUSKONTROLL                                           
232295     .                                                                    
232296     EJECT                                                                
232297 IMS-GHU-WDM221 SECTION.                                                  
232298                                                                          
232299     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
232300          DELIMITED BY SIZE INTO SSA1                                     
232310     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
232311          DELIMITED BY SIZE INTO SSA2                                     
232312     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
232313          DELIMITED BY SIZE INTO SSA3                                     
232314     MOVE '  GE' TO GODK-STATUSKODER                                      
232315     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3         
232316     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
232317     PERFORM IMS-STATUSKONTROLL                                           
232318     .                                                                    
232319                                                                          
232320 IMS-REPL-WDM221 SECTION.                                                 
232321                                                                          
232322     MOVE '  '             TO GODK-STATUSKODER                            
232323     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM221                       
232324     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
232325     PERFORM IMS-STATUSKONTROLL                                           
232326     .                                                                    
232327     EJECT                                                                
232328 IMS-GHN-WDA6A SECTION.                                                   
232329     STRING 'WDA601  (WDA6ASEQ>=' W-WDA601KY-MIN-X                        
232330                    '&WDA6ASEQ<=' W-WDA601KY-MAX-X ')'                    
232340            DELIMITED BY SIZE INTO SSA1                                   
232350     MOVE '  GEGB'               TO GODK-STATUSKODER                      
232360     CALL  CBLTDLI  USING GHN   WDA6A-PCB DLI-IO-AREA-WDA6 SSA1           
232370     MOVE WDA6A-STATUS-CODE TO STATUS-WS                                  
232380     PERFORM IMS-STATUSKONTROLL                                           
232390     .                                                                    
232400 IMS-REPL-WDA6A SECTION.                                                  
232500     MOVE 'WDA601  '           TO SSA1                                    
232600     MOVE '    '               TO GODK-STATUSKODER                        
232700     CALL  CBLTDLI  USING REPL WDA6A-PCB DLI-IO-AREA-WDA6 SSA1            
232800     MOVE WDA6A-STATUS-CODE TO STATUS-WS                                  
232900     PERFORM IMS-STATUSKONTROLL                                           
233000     .                                                                    
233100 IMS-GHN-WDA6B SECTION.                                                   
233200     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
233300                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
233400            DELIMITED BY SIZE INTO SSA1                                   
233500     MOVE '  GEGB'               TO GODK-STATUSKODER                      
233600     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-AREA-WDA6 SSA1           
233700     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
233800     PERFORM IMS-STATUSKONTROLL                                           
233900     .                                                                    
234000 IMS-REPL-WDA6B SECTION.                                                  
234100     MOVE 'WDA601  '           TO SSA1                                    
234200     MOVE '    '               TO GODK-STATUSKODER                        
234300     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-AREA-WDA6 SSA1            
234400     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
234500     PERFORM IMS-STATUSKONTROLL                                           
234600     .                                                                    
234700                                                                          
234800 IMS-GU-WDB601    SECTION.                                                
234900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
235000          DELIMITED BY SIZE INTO SSA1                                     
235100     MOVE '  GE' TO GODK-STATUSKODER                                      
235200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
235300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
235400     PERFORM IMS-STATUSKONTROLL                                           
235500     .                                                                    
235600     EJECT                                                                
235700 DB2-SELECT-TP4TRAN     SECTION.                                          
235800                                                                          
235900     MOVE 000100 TO GODK-SQLCODEKODER                                     
236000                                                                          
236100     EXEC SQL                                                             
236200           SELECT  DISTINCT                                               
236300                   IDDC_REC                                               
236400                                                                          
236500           INTO   :TP4TRAN-IDDC-REC                                       
236600                                                                          
236700           FROM    TP4TRAN                                                
236800                                                                          
236900           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
237000     END-EXEC                                                             
237100                                                                          
237200     MOVE SQLCODE TO SQLCODE-WS                                           
237300     PERFORM DB2-STATUSKONTROLL                                           
237400     .                                                                    
237500     EJECT                                                                
237600 IMS-STATUSKONTROLL SECTION.                                              
237700                                                                          
237800     SET STATUS-IX TO 1                                                   
237900     SEARCH GODK-STATUS                                                   
238000       AT END                                                             
238100         STRING 'STATUSKOD FROM IMS ' STATUS-WS                           
238200           DELIMITED BY SIZE INTO FELTEXT                                 
238300         CALL FELLOG                                                      
238400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
238500     END-SEARCH                                                           
238600     .                                                                    
238700 DB2-STATUSKONTROLL  SECTION.                                             
238800                                                                          
238900     SET SQLCODE-IX TO 1                                                  
239000     SEARCH GODK-SQLCODE                                                  
239100       AT END                                                             
239200          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
239300          DELIMITED BY SIZE INTO FELTEXT                                  
239400          CALL ABEND USING RKOD-ABEND-DB2                                 
239500       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
239600     END-SEARCH                                                           
239700     .                                                                    
