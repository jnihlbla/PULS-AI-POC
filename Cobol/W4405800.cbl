001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W4405800.                                                
001300 AUTHOR.         GÖRAN KJELLSON.                                          
001400 DATE-WRITTEN.   20/01/07.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001800*        KOMPLETTERA VOR-UPPFÖLJNINGSFIL                                  
001900*                                                                         
002010*        PROGRAMMET LÄSER      WDE2                                       
002020*                   LÄSER      WDE6                                       
002040*                   LÄSER      WDQ3                                       
002100*                                                                         
002200*    ABENDKODER:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700                                                                          
002800 ENVIRONMENT DIVISION.                                                    
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301                                                                          
003302*          --- WDA6 EXTRAKT                                               
003303     SELECT W4405A                     ASSIGN TO W44058D1.                
003304                                                                          
003305*          --- KOMPLETTERAD VOR-EXTRAKT                                   
003310     SELECT W44062                     ASSIGN TO W44058D2.                
003500                                                                          
003600 DATA DIVISION.                                                           
003800 FILE SECTION.                                                            
003901                                                                          
003902 FD  W4405A                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003906*01  -COPY WDA601      -L.                                                
003907                                                                          
003908                                                                          
003909 FD  W44062                                                               
003910     RECORDING       F                                                    
003911     BLOCK CONTAINS  0.                                                   
003912                                                                          
003920*01  POST -COPY W440058 -PRE  UT-  -L.                                    
004000                                                                          
004010                                                                          
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W4405A00'.            
004310 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004320 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004701                                                                          
004702 77  W4405A-EOF-SW               PIC X       VALUE 'N'.                   
004710     88  END-OF-W4405A                       VALUE 'J'.                   
004800                                                                          
004900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES DAGENS-DATUM.                                       
005100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005310                                                                          
005311 01  WS-DATUM-HELP               PIC 9(6)    VALUE ZERO.                  
005312 01  FILLER REDEFINES WS-DATUM-HELP.                                      
005313     03  WS-AAR-HELP             PIC 9(2).                                
005314     03  WS-MAANAD-HELP          PIC 9(2).                                
005315     03  WS-DAG-HELP             PIC 9(2).                                
005316                                                                          
005320 01  WS-DATUM                    PIC X(10)   VALUE '2000-00-00'.          
005330 01  FILLER REDEFINES WS-DATUM.                                           
005340     03  FILLER                  PIC 9(2).                                
005341     03  WS-AAR                  PIC 9(2).                                
005342     03  FILLER                  PIC X(1).                                
005350     03  WS-MAANAD               PIC 9(2).                                
005351     03  FILLER                  PIC X(1).                                
005360     03  WS-DAG                  PIC 9(2).                                
005400                                                                          
005401 01  WS-TID-HELP-8               PIC 9(8)    VALUE ZERO.                  
005402 01  FILLER REDEFINES WS-TID-HELP-8.                                      
005403     03  WS-HH-8                 PIC 9(2).                                
005404     03  WS-MM-8                 PIC 9(2).                                
005405     03  WS-SS-8                 PIC 9(2).                                
005406     03  WS-TH-8                 PIC 9(2).                                
005407                                                                          
005408 01  WS-TID-HELP-6               PIC 9(6)    VALUE ZERO.                  
005409 01  FILLER REDEFINES WS-TID-HELP-6.                                      
005410     03  WS-HH-6                 PIC 9(2).                                
005411     03  WS-MM-6                 PIC 9(2).                                
005412     03  WS-SS-6                 PIC 9(2).                                
005413                                                                          
005414 01  WS-TID-HELP-4               PIC 9(4)    VALUE ZERO.                  
005415 01  FILLER REDEFINES WS-TID-HELP-4.                                      
005416     03  WS-HH-4                 PIC 9(2).                                
005417     03  WS-MM-4                 PIC 9(2).                                
005419                                                                          
005420 01  WS-TID                      PIC X(8)    VALUE '00:00:00'.            
005430 01  FILLER REDEFINES WS-TID.                                             
005440     03  WS-HH                   PIC 9(2).                                
005450     03  FILLER                  PIC X(1).                                
005460     03  WS-MM                   PIC 9(2).                                
005470     03  FILLER                  PIC X(1).                                
005480     03  WS-SS                   PIC 9(2).                                
005490                                                                          
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100                                                                          
006200*    --- PARAMETRAR TILL ABEND                                            
006300                                                                          
006400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006700                                                                          
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007101                                                                          
007102                                                                          
007103*    --- PARAMETRAR TILL POSTSUM                                          
007104*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301                                                                          
007302 01  W-DATE-TIME                 PIC 9(10).                               
007303 01  FILLER REDEFINES W-DATE-TIME.                                        
007304     03  W-DATE                  PIC 9(6).                                
007305     03  W-TIME                  PIC 9(4).                                
007306                                                                          
007307 01  IN-AREA-START               PIC X(24)   VALUE                        
007308                                 'IN-AREA-START  '.                       
007309                                                                          
007310*01  AREA -COPY WDA601     -PRE IN-                                       
007311                                                                          
007312 01  UT-AREA-START               PIC X(24)   VALUE                        
007313                                 'UT-AREA-START  '.                       
007314                                                                          
007320*01  AREA -COPY W440058    -PRE UT-                                       
007400                                                                          
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900                                                                          
008000 01  NYCKLAR-TILL-DLI.                                                    
008108*    WDE6                                                                 
008109     03  W-IDPRODNR-X.                                                    
008110       05  W-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.              
008111*    WDQ3                                                                 
008112     03  W-WDQ3GSEQ-X.                                                    
008114       05  W-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.              
008116       05  W-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.              
008117       05  W-IDDC            PIC  X(2)   VALUE SPACE.                     
008118       05  W-IDKUNDRF.                                                    
008119         07  W-IDORDNR7        PIC  9(7)   VALUE ZERO.                    
008120         07  FILLER            PIC  X(3)   VALUE SPACE.                   
008123                                                                          
008124*    WDE2                                                                 
008130     03  W-IDSHIPM-X.                                                     
008140       05 W-IDSHIPM          PIC 9(7)    VALUE ZERO.                      
008200                                                                          
008201*    WDE6                                                                 
008210     03  W-IDKOLLI-X.                                                     
008220         05  W-IDKOLLI           PIC S9(5)   COMP-3.                      
008230                                                                          
008240                                                                          
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008800                                                                          
008900 01  GODK-STATUSKODER.                                                    
009000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100                                                                          
009110 01  ALL-SSA.                                                             
009200     03 SSA1                     PIC X(64).                               
009300     03 SSA2                     PIC X(64).                               
009400                                                                          
009410                                                                          
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700                                                                          
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE201'.                      
010002 01  DLI-IO-WDE201.                                                       
010010*    03  -COPY WDE201                                                     
010300                                                                          
010310                                                                          
010320 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
010330 01  DLI-IO-WDE601.                                                       
010340*    03  -COPY WDE601                                                     
010350                                                                          
010351 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE611'.                      
010352 01  DLI-IO-WDE611.                                                       
010353*    03  -COPY WDE611                                                     
010359                                                                          
010360                                                                          
010366 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ301'.                      
010367 01  DLI-IO-WDQ301.                                                       
010368*    03  -COPY WDQ301                                                     
010369                                                                          
010370                                                                          
010400 LINKAGE SECTION.                                                         
010500                                                                          
010601                                                                          
010602*01  -COPY W0008  -PRE WDE2-                                              
010610     05  FILLER                  PIC X.                                   
010700                                                                          
010710*01  -COPY W0008  -PRE WDE6-                                              
010720     05  FILLER                  PIC X.                                   
010721                                                                          
010725*01  -COPY W0008  -PRE WDQ3-                                              
010726     05  FILLER                  PIC X.                                   
010730                                                                          
010800                                                                          
010801 PROCEDURE DIVISION  USING WDE2-PCB WDE6-PCB WDQ3-PCB.                    
010802 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING WDE2-PCB WDE6-PCB WDQ3-PCB.                    
010900                                                                          
011100                                                                          
011200     PERFORM A-INIT                                                       
011300                                                                          
011410     PERFORM S01-LAES-W4405A                                              
011420     PERFORM B-GET-WDA6-INFO                                              
011421                                                                          
011430     PERFORM S01-LAES-W4405A                                              
011500     PERFORM UNTIL END-OF-W4405A                                          
011600                                                                          
011701        IF IN-VOR-IDDISTR  = UT-IDDISTR       AND                         
011702           IN-VOR-IDKUNDNR = UT-IDKUNDNR      AND                         
011703           IN-VOR-IDORDNR7 = UT-IDORDNR7-URSP AND                         
011708           IN-VOR-IDARTNR  = UT-IDARTNR                                   
011712           PERFORM B-GET-WDA6-INFO                                        
011713        ELSE                                                              
011720           PERFORM C-KOMPLETTERA-VOR                                      
012110           PERFORM B-GET-WDA6-INFO                                        
012200        END-IF                                                            
012210        PERFORM S01-LAES-W4405A                                           
012300     END-PERFORM                                                          
012400                                                                          
012410     PERFORM C-KOMPLETTERA-VOR                                            
012500                                                                          
012600     PERFORM Z-FINIT                                                      
012700                                                                          
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100                                                                          
013110                                                                          
013200 A-INIT SECTION.                                                          
013300     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
013301                                                                          
013310     OPEN INPUT  W4405A                                                   
013401                                                                          
013410     OPEN OUTPUT W44062                                                   
013500                                                                          
013600     ACCEPT DAGENS-DATUM  FROM DATE                                       
013710     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013900     .                                                                    
014000                                                                          
014010                                                                          
014100 B-GET-WDA6-INFO SECTION.                                                 
014200     MOVE 'B-GET-WDA6-INFO ' TO CURRENT-SECTION                           
014201                                                                          
014212     MOVE IN-VOR-IDDISTR       TO UT-IDDISTR                              
014213     MOVE IN-VOR-IDKUNDNR      TO UT-IDKUNDNR                             
014214     MOVE IN-VOR-IDORDNR7      TO UT-IDORDNR7-URSP                        
014215                                                                          
014217     MOVE IN-VOR-TIREGDAT-URSP TO WS-DATUM-HELP                           
014218     MOVE WS-AAR-HELP          TO WS-AAR                                  
014219     MOVE WS-MAANAD-HELP       TO WS-MAANAD                               
014220     MOVE WS-DAG-HELP          TO WS-DAG                                  
014221     MOVE WS-DATUM             TO UT-TIREGDAT-URSP                        
014226                                                                          
014227     MOVE IN-VOR-TIREGTID-URSP TO WS-TID-HELP-8                           
014228     MOVE WS-HH-8              TO WS-HH                                   
014229     MOVE WS-MM-8              TO WS-MM                                   
014230     MOVE WS-SS-8              TO WS-SS                                   
014231     MOVE WS-TID               TO UT-TIREGTID-URSP                        
014232                                                                          
014233     IF IN-VOR-TIREGDAT-AVV > ZERO                                        
014234      MOVE IN-VOR-TIREGDAT-AVV TO WS-DATUM-HELP                           
014235      MOVE WS-AAR-HELP         TO WS-AAR                                  
014236      MOVE WS-MAANAD-HELP      TO WS-MAANAD                               
014237      MOVE WS-DAG-HELP         TO WS-DAG                                  
014238      MOVE WS-DATUM            TO UT-TIREGDAT-AVV                         
014239     ELSE                                                                 
014240      MOVE '0000-00-00'        TO UT-TIREGDAT-AVV                         
014241     END-IF                                                               
014242                                                                          
014243     MOVE IN-VOR-TIREGTID-AVV  TO WS-TID-HELP-8                           
014244     MOVE WS-HH-8              TO WS-HH                                   
014245     MOVE WS-MM-8              TO WS-MM                                   
014246     MOVE WS-SS-8              TO WS-SS                                   
014247     MOVE WS-TID               TO UT-TIREGTID-AVV                         
014248                                                                          
014249     MOVE IN-VOR-IDARTNR       TO UT-IDARTNR                              
014250     MOVE IN-VOR-IDORDNR7-LEV  TO UT-IDORDNR7-LEV                         
014251                                                                          
014252     IF IN-VOR-TIREGDAT-LEV > ZERO                                        
014253      MOVE IN-VOR-TIREGDAT-LEV TO WS-DATUM-HELP                           
014254      MOVE WS-AAR-HELP         TO WS-AAR                                  
014255      MOVE WS-MAANAD-HELP      TO WS-MAANAD                               
014256      MOVE WS-DAG-HELP         TO WS-DAG                                  
014257      MOVE WS-DATUM            TO UT-TIREGDAT-LEV                         
014258     ELSE                                                                 
014259      MOVE '0000-00-00'        TO UT-TIREGDAT-LEV                         
014260     END-IF                                                               
014264                                                                          
014265     MOVE IN-VOR-TIREGTID-LEV  TO WS-TID-HELP-8                           
014266     MOVE WS-HH-8              TO WS-HH                                   
014267     MOVE WS-MM-8              TO WS-MM                                   
014268     MOVE WS-SS-8              TO WS-SS                                   
014269     MOVE WS-TID               TO UT-TIREGTID-LEV                         
014270                                                                          
014271     MOVE IN-VOR-IDROLL        TO UT-IDROLL                               
014272     MOVE IN-VOR-IDANSK        TO UT-IDANSK                               
014273     MOVE IN-VOR-IDLEVNR       TO UT-IDLEVNR                              
014274     MOVE IN-VOR-KVBEART       TO UT-KVBEART                              
014275     MOVE IN-VOR-KVBEART-Q     TO UT-KVBEART-Q                            
014276     MOVE IN-VOR-IDDC          TO UT-IDDC                                 
014277     MOVE IN-VOR-KDORDBEK      TO UT-KDORDBEK                             
014278                                                                          
014290     IF IN-VOR-TIKLAR > ZERO                                              
014291        MOVE IN-VOR-TIKLAR     TO WS-DATUM-HELP                           
014292        MOVE WS-AAR-HELP       TO WS-AAR                                  
014293        MOVE WS-MAANAD-HELP TO WS-MAANAD                                  
014294        MOVE WS-DAG-HELP       TO WS-DAG                                  
014295        MOVE WS-DATUM          TO UT-TIKLAR                               
014296     ELSE                                                                 
014297        MOVE '0000-00-00'      TO UT-TIKLAR                               
014298     END-IF                                                               
014299                                                                          
014300     MOVE IN-VOR-TIKLATID      TO WS-TID-HELP-6                           
014301     MOVE WS-HH-6              TO WS-HH                                   
014302     MOVE WS-MM-6              TO WS-MM                                   
014303     MOVE WS-SS-6              TO WS-SS                                   
014304     MOVE WS-TID               TO UT-TIKLATID                             
014305                                                                          
014306*  WDQ3-GRP.                                                              
014307     MOVE '0000-00-00'         TO UT-TIRFS-DATE-URSP                      
014308     MOVE '00:00:00'           TO UT-TIRFS-TIME-URSP                      
014309     MOVE '0000-00-00'         TO UT-TIRFS-DATE-LEV                       
014310     MOVE '00:00:00'           TO UT-TIRFS-TIME-LEV                       
014311     MOVE '0000-00-00'         TO UT-DATRPAVD-URSP                        
014312     MOVE '00:00:00'           TO UT-TIHHMM-URSP                          
014313     MOVE '0000-00-00'         TO UT-DATRPAVD-LEV                         
014314     MOVE '00:00:00'           TO UT-TIHHMM-LEV                           
014315                                                                          
014316*  WDE2-GRP.                                                              
014317     MOVE '0000-00-00'         TO UT-TISKEPPN-LEV                         
014318     MOVE '00:00:00'           TO UT-TISKPTID-LEV                         
014319                                                                          
014320*  WDE6-GRP.                                                              
014321     MOVE '0000-00-00'         TO UT-TIUTSKR-LEV                          
014322     MOVE '00:00:00'           TO UT-TIUTSTID-LEV                         
014323     MOVE '0000-00-00'         TO UT-TIPACKN-LEV                          
014324     MOVE '00:00:00'           TO UT-TIPACTID-LEV                         
014325     MOVE '0000-00-00'         TO UT-TIFAKT-LEV                           
014326     MOVE '00:00:00'           TO UT-TIFAKTID-LEV                         
014327     MOVE ZERO                 TO UT-IDFAKT                               
014328     MOVE ZERO                 TO UT-IDPLOCK                              
014329     .                                                                    
014330                                                                          
014331                                                                          
014332 C-KOMPLETTERA-VOR SECTION.                                               
014333     MOVE 'C-KOMPLETTERA-VOR' TO CURRENT-SECTION                          
014334                                                                          
014335     PERFORM CA-GET-WDQ3-INFO                                             
014336     IF SEGMENT-FINNS                                                     
014337                                                                          
014338        IF ODEL-KDODELSTA NOT = 'R'                                       
014339           PERFORM CB-GET-WDE6-INFO                                       
014340           IF SEGMENT-FINNS                                               
014341              PERFORM CC-GET-WDE2-INFO                                    
014342           END-IF                                                         
014343        END-IF                                                            
014344                                                                          
014345        PERFORM S11-SKRIV-W44062                                          
014346     END-IF                                                               
014347     .                                                                    
014348                                                                          
014349                                                                          
014350 CA-GET-WDQ3-INFO SECTION.                                                
014351     MOVE 'CA-GET-WDQ3-INFO ' TO CURRENT-SECTION                          
014352                                                                          
014353     MOVE UT-IDDISTR  TO W-IDDISTR                                        
014354     MOVE UT-IDKUNDNR      TO W-IDKUNDNR                                  
014355     MOVE UT-IDORDNR7-URSP TO W-IDORDNR7                                  
014356     MOVE UT-IDDC          TO W-IDDC                                      
014357                                                                          
014358     PERFORM IMS-GU-WDQ301                                                
014359     IF SEGMENT-FINNS                                                     
014360           MOVE ODEL-DARFS TO W-DATE-TIME                                 
014361           MOVE W-DATE               TO WS-DATUM-HELP                     
014362           MOVE WS-AAR-HELP          TO WS-AAR                            
014363           MOVE WS-MAANAD-HELP       TO WS-MAANAD                         
014364           MOVE WS-DAG-HELP          TO WS-DAG                            
014365           MOVE WS-DATUM             TO UT-TIRFS-DATE-URSP                
014366                                                                          
014367           MOVE W-TIME               TO WS-TID-HELP-4                     
014368           MOVE WS-HH-4              TO WS-HH                             
014369           MOVE WS-MM-4              TO WS-MM                             
014370           MOVE ZERO                 TO WS-SS                             
014371           MOVE WS-TID               TO UT-TIRFS-TIME-URSP                
014372                                                                          
014373           MOVE ODEL-DATRPAVD        TO WS-DATUM-HELP                     
014374           MOVE WS-AAR-HELP          TO WS-AAR                            
014375           MOVE WS-MAANAD-HELP       TO WS-MAANAD                         
014376           MOVE WS-DAG-HELP          TO WS-DAG                            
014380           MOVE WS-DATUM             TO UT-DATRPAVD-URSP                  
014430                                                                          
014431           MOVE ODEL-TIHHMM          TO WS-TID-HELP-4                     
014432           MOVE WS-HH-4              TO WS-HH                             
014433           MOVE WS-MM-4              TO WS-MM                             
014434           MOVE ZERO                 TO WS-SS                             
014435           MOVE WS-TID               TO UT-TIHHMM-URSP                    
014436                                                                          
014437     END-IF                                                               
014438                                                                          
014439     IF UT-IDORDNR7-LEV NOT = ZERO                                        
014440     MOVE UT-IDORDNR7-LEV  TO W-IDORDNR7                                  
014441                                                                          
014442     PERFORM IMS-GU-WDQ301                                                
014443     IF SEGMENT-FINNS                                                     
014445           MOVE ODEL-DARFS TO W-DATE-TIME                                 
014446           MOVE W-DATE               TO WS-DATUM-HELP                     
014447           MOVE WS-AAR-HELP          TO WS-AAR                            
014448           MOVE WS-MAANAD-HELP       TO WS-MAANAD                         
014449           MOVE WS-DAG-HELP          TO WS-DAG                            
014450           MOVE WS-DATUM             TO UT-TIRFS-DATE-LEV                 
014451                                                                          
014452           MOVE W-TIME               TO WS-TID-HELP-4                     
014453           MOVE WS-HH-4              TO WS-HH                             
014454           MOVE WS-MM-4              TO WS-MM                             
014455           MOVE ZERO                 TO WS-SS                             
014456           MOVE WS-TID               TO UT-TIRFS-TIME-LEV                 
014457                                                                          
014461           MOVE ODEL-DATRPAVD        TO WS-DATUM-HELP                     
014462           MOVE WS-AAR-HELP          TO WS-AAR                            
014463           MOVE WS-MAANAD-HELP       TO WS-MAANAD                         
014464           MOVE WS-DAG-HELP          TO WS-DAG                            
014465           MOVE WS-DATUM             TO UT-DATRPAVD-LEV                   
014466                                                                          
014467           MOVE ODEL-TIHHMM          TO WS-TID-HELP-4                     
014468           MOVE WS-HH-4              TO WS-HH                             
014469           MOVE WS-MM-4              TO WS-MM                             
014470           MOVE ZERO                 TO WS-SS                             
014471           MOVE WS-TID               TO UT-TIHHMM-LEV                     
014472                                                                          
014473                                                                          
014474     END-IF                                                               
014475     END-IF                                                               
014476     .                                                                    
014477                                                                          
014478                                                                          
014479 CB-GET-WDE6-INFO SECTION.                                                
014480     MOVE 'CB-GET-WDE6-INFO' TO CURRENT-SECTION                           
014481                                                                          
014482     MOVE ODEL-IDPRODNR TO W-IDPRODNR                                     
014483                                                                          
014484     PERFORM IMS-GU-WDE601                                                
014485     IF SEGMENT-FINNS                                                     
014486        MOVE VORD-TIUTSKR      TO WS-DATUM-HELP                           
014487        MOVE WS-AAR-HELP       TO WS-AAR                                  
014488        MOVE WS-MAANAD-HELP    TO WS-MAANAD                               
014489        MOVE WS-DAG-HELP       TO WS-DAG                                  
014490        MOVE WS-DATUM          TO UT-TIUTSKR-LEV                          
014491                                                                          
014492        MOVE VORD-TIUTSTID     TO WS-TID-HELP-6                           
014493        MOVE WS-HH-6        TO WS-HH                                      
014494        MOVE WS-MM-6        TO WS-MM                                      
014495        MOVE WS-SS-6        TO WS-SS                                      
014496        MOVE WS-TID            TO UT-TIUTSTID-LEV                         
014497                                                                          
014498        PERFORM IMS-GNP-WDE611                                            
014499        IF SEGMENT-FINNS                                                  
014500           MOVE KOLLI-TIPACKN  TO WS-DATUM-HELP                           
014501           MOVE WS-AAR-HELP    TO WS-AAR                                  
014502           MOVE WS-MAANAD-HELP TO WS-MAANAD                               
014503           MOVE WS-DAG-HELP    TO WS-DAG                                  
014504           MOVE WS-DATUM       TO UT-TIPACKN-LEV                          
014505                                                                          
014506           MOVE KOLLI-TIPACTID TO WS-TID-HELP-6                           
014507           MOVE WS-HH-6     TO WS-HH                                      
014508           MOVE WS-MM-6     TO WS-MM                                      
014509           MOVE WS-SS-6     TO WS-SS                                      
014510           MOVE WS-TID         TO UT-TIPACTID-LEV                         
014511                                                                          
014512           IF KOLLI-TIFAKT > ZERO                                         
014513            MOVE KOLLI-TIFAKT   TO WS-DATUM-HELP                          
014514            MOVE WS-AAR-HELP    TO WS-AAR                                 
014515            MOVE WS-MAANAD-HELP TO WS-MAANAD                              
014516            MOVE WS-DAG-HELP    TO WS-DAG                                 
014517            MOVE WS-DATUM       TO UT-TIFAKT-LEV                          
014518           ELSE                                                           
014519            MOVE '0000-00-00'   TO UT-TIFAKT-LEV                          
014520           END-IF                                                         
014521                                                                          
014522           MOVE KOLLI-TIFAKTID TO WS-TID-HELP-6                           
014523           MOVE WS-HH-6     TO WS-HH                                      
014524           MOVE WS-MM-6     TO WS-MM                                      
014525           MOVE WS-SS-6     TO WS-SS                                      
014526           MOVE WS-TID         TO UT-TIFAKTID-LEV                         
014527                                                                          
014528           MOVE KOLLI-IDFAKT   TO UT-IDFAKT                               
014529           MOVE KOLLI-IDPLOCK  TO UT-IDPLOCK                              
014530        END-IF                                                            
014531     END-IF                                                               
014532     .                                                                    
014533                                                                          
014534                                                                          
014535 CC-GET-WDE2-INFO SECTION.                                                
014536     MOVE 'CC-GET-WDE2-INFO' TO CURRENT-SECTION                           
014537                                                                          
014538     MOVE KOLLI-IDSHIPM TO W-IDSHIPM                                      
014539                                                                          
014540     PERFORM IMS-GU-WDE201                                                
014541     IF SEGMENT-FINNS                                                     
014542        MOVE BILL-TISKEPPN  TO WS-DATUM-HELP                              
014543        MOVE WS-AAR-HELP    TO WS-AAR                                     
014544        MOVE WS-MAANAD-HELP TO WS-MAANAD                                  
014545        MOVE WS-DAG-HELP    TO WS-DAG                                     
014546        MOVE WS-DATUM       TO UT-TISKEPPN-LEV                            
014547                                                                          
014548        MOVE BILL-TISKPTID  TO WS-TID-HELP-6                              
014549        MOVE WS-HH-6        TO WS-HH                                      
014550        MOVE WS-MM-6        TO WS-MM                                      
014551        MOVE WS-SS-6        TO WS-SS                                      
014552        MOVE WS-TID         TO UT-TISKPTID-LEV                            
014553     ELSE                                                                 
014554        MOVE '0000-00-00'   TO UT-TIKLAR                                  
014555        MOVE '00:00:00'     TO UT-TIKLATID                                
014556     END-IF                                                               
014557                                                                          
014558*    IF BILL-TISKEPPN = ZERO                                              
014563*       MOVE '0000-00-00'   TO UT-TIKLAR                                  
014570*       MOVE '00:00:00'     TO UT-TIKLATID                                
014571*    END-IF                                                               
014572     .                                                                    
014573                                                                          
014574                                                                          
014575 Z-FINIT SECTION.                                                         
014576     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
014577                                                                          
014578     CLOSE W4405A                                                         
014579           W44062                                                         
014580                                                                          
014581     MOVE 'S' TO POSTSUM-OPKOD                                            
014582     CALL POSTSUM USING POSTSUM-PARM                                      
014583     .                                                                    
014584                                                                          
014585                                                                          
014586 S01-LAES-W4405A  SECTION.                                                
014587                                                                          
014588     READ W4405A INTO IN-AREA                                             
014589     AT END                                                               
014590        MOVE HIGH-VALUE   TO IN-AREA                                      
014591        SET END-OF-W4405A TO TRUE                                         
014592                                                                          
014593     NOT AT END                                                           
014594        MOVE 'W4405A'     TO POSTSUM-FDNAMN                               
014595        MOVE 'W4405AD1'   TO POSTSUM-DDNAMN2                              
014596        MOVE SPACE        TO POSTSUM-TRANSTYP                             
014597        CALL POSTSUM USING POSTSUM-PARM                                   
014598     END-READ                                                             
014599     .                                                                    
014600                                                                          
014601                                                                          
014602 S11-SKRIV-W44062 SECTION.                                                
014603                                                                          
014604     WRITE UT-POST FROM UT-AREA                                           
014605                                                                          
014606     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
014607     MOVE 'W44062'   TO POSTSUM-FDNAMN                                    
014608     MOVE 'W4405AD2' TO POSTSUM-DDNAMN2                                   
014609     CALL POSTSUM USING POSTSUM-PARM                                      
014610     .                                                                    
014620                                                                          
014630                                                                          
014900 S99-ABEND SECTION.                                                       
015101                                                                          
015102     MOVE 'S' TO POSTSUM-OPKOD                                            
015110     CALL POSTSUM USING POSTSUM-PARM                                      
015200     CALL ABEND   USING RKOD-ABEND                                        
015300     .                                                                    
015400                                                                          
015410                                                                          
015500* --- IMS SEKTIONER ---                                                   
015600                                                                          
015702 IMS-GU-WDE201 SECTION.                                                   
015703     MOVE 'IMS-GU-WDE201   ' TO CURRENT-IMS-SECTION                       
015704                                                                          
015705     MOVE SPACE TO ALL-SSA                                                
015706                                                                          
015707     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
015708          DELIMITED BY SIZE INTO SSA1                                     
015709     MOVE '  GE'              TO GODK-STATUSKODER                         
015710     CALL CBLTDLI USING GU WDE2-PCB DLI-IO-WDE201 SSA1                    
015711     MOVE WDE2-STATUS-CODE    TO STATUS-WS                                
015712     PERFORM IMS-STATUSKONTROLL                                           
015720     .                                                                    
015800                                                                          
015810                                                                          
015930 IMS-GU-WDQ301 SECTION.                                                   
015931     MOVE 'IMS-GU-WDQ301'   TO CURRENT-IMS-SECTION                        
015932                                                                          
015933     MOVE SPACE TO ALL-SSA                                                
015934                                                                          
015936     STRING 'WDQ301  (WDQ3GSEQ =' W-WDQ3GSEQ-X ')'                        
015937          DELIMITED BY SIZE INTO SSA1                                     
015938     MOVE '  GE' TO GODK-STATUSKODER                                      
015939     CALL CBLTDLI USING GU WDQ3-PCB DLI-IO-WDQ301 SSA1                    
015940     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
015941     PERFORM IMS-STATUSKONTROLL                                           
015942     .                                                                    
015944                                                                          
015953                                                                          
015954 IMS-GU-WDE601          SECTION.                                          
015955     MOVE 'IMS-GU-WDE601'   TO CURRENT-IMS-SECTION                        
015956                                                                          
015957     MOVE SPACE TO ALL-SSA                                                
015958                                                                          
015959     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
015960            DELIMITED BY SIZE INTO SSA1                                   
015961     MOVE '  GE' TO GODK-STATUSKODER                                      
015962     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
015963     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
015964     PERFORM IMS-STATUSKONTROLL                                           
015965     .                                                                    
015966                                                                          
015967                                                                          
015968 IMS-GNP-WDE611 SECTION.                                                  
015969     MOVE 'IMS-GNP-WDE611'  TO CURRENT-IMS-SECTION                        
015970                                                                          
015971     MOVE SPACE TO ALL-SSA                                                
015972                                                                          
015975     MOVE 'WDE611 '        TO SSA1                                        
015977     MOVE '  GE'           TO GODK-STATUSKODER                            
015978     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-WDE611 SSA1                   
015979     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
015980     PERFORM IMS-STATUSKONTROLL                                           
015981     .                                                                    
015982                                                                          
015983                                                                          
015990 IMS-STATUSKONTROLL SECTION.                                              
016000                                                                          
016100     SET STATUS-IX TO 1                                                   
016200     SEARCH GODK-STATUS                                                   
016300       AT END                                                             
016400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016500           DELIMITED BY SIZE INTO FELTEXT                                 
016600         DISPLAY FELTEXT                                                  
016700         CALL FELLOG                                                      
016800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016900         CONTINUE                                                         
017000     END-SEARCH                                                           
017100     .                                                                    
