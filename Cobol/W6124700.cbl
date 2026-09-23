000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6124700.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   MAJ -00                                                  
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        NEDLÄSNING HTR WDR301                                            
001000*        LDC REFILL  AVVIKELSE VID INLEVERANS                             
001100*                                                                         
001200*        PROGRAMMET LÄSER WDR301                                          
001300*                         WDK6                                            
001400*        SKAPAR FIL W61247 LISTFIL AVVIKELSE                              
001500*                   W6121F ALLA DC                                        
001600*                   W61248 FÖR BORTTAG HTR                                
001700*                   W6124S SKROTSAVVIKELSER                               
001800*                                                                         
001900* 2013-0408 ETRACKER 8200058 SCRAPPING FOLLOW UP                          
002000*                                                                         
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300                                                                          
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700                                                                          
002800*          --- LISTFIL AVVIKELSE FLINLREP = JA                            
002900     SELECT W61247                     ASSIGN TO W61247D1.                
003000*          --- BORTTAG HTR                                                
003100     SELECT W61248                     ASSIGN TO W61247D2.                
003200*          --- LISTFIL AVVIKELSE ALLA DC                                  
003300     SELECT W6121F                     ASSIGN TO W61247D3.                
003400*          --- FIL SKROTAVVIKELSER (FRÅN WL0109)                          
003500     SELECT W6124S                     ASSIGN TO W61247D4.                
003600                                                                          
003610                                                                          
003700 DATA DIVISION.                                                           
003800                                                                          
003900 FILE SECTION.                                                            
004000                                                                          
004100 FD  W61247                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400*01  POST -COPY W6124A  -PRE  AVVIK-  -L.                                 
004500                                                                          
004510                                                                          
004600 FD  W61248                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900*01  POST -COPY WDR301  -PRE  BORT-   -L.                                 
005000                                                                          
005010                                                                          
005100 FD  W6121F                                                               
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400*01  POST -COPY W61247  -PRE  UT2-    -L.                                 
005500                                                                          
005510                                                                          
005600 FD  W6124S                                                               
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900*01  POST -COPY W41403S -PRE  UT3-    -L.                                 
006000                                                                          
006010                                                                          
006020                                                                          
006100 WORKING-STORAGE SECTION.                                                 
006200*    -- CHECKED BY WY2000                                                 
006300                                                                          
006400 77  IDPGM                       PIC X(8)    VALUE 'W6124700'.            
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  NEJ                         PIC X       VALUE 'N'.                   
006610 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
006620 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
006700                                                                          
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007100                                                                          
007200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007300 01  FILLER REDEFINES DAGENS-DATUM.                                       
007400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007700                                                                          
007701                                                                          
007702 01  WS-IDDC-SEND-REC.                                                    
007703     03  WS-IDDC-SEND            PIC X(2).                                
007704     03  WS-IDDC-REC             PIC X(2).                                
007705*01  FILLER  -COPY WWDC03                                                 
007710                                                                          
007800 01  DYNAMISKA-SUBPROGRAM.                                                
007900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008200     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
008300                                                                          
008400*    --- PARAMETRAR TILL POSTSUM                                          
008500*01  -COPY W0005   -PRE  POSTSUM-                                         
008600                                                                          
008700*    --- PARAMETRAR TILL WL10WBDC                                         
008800*01  -COPY WL10WBDC                                                       
008900                                                                          
009000*01  -COPY WWDC99                                                         
009100                                                                          
009110                                                                          
009200 01  AVVIK-AREA-START            PIC X(16)   VALUE                        
009300                                             'AVVIK-AREA-START'.          
009400*01  AREA -COPY W6124A      -PRE AVVIK-                                   
009500                                                                          
009501                                                                          
009510 01  UT-AREA-START               PIC X(16)   VALUE                        
009520                                             'UT-AREA-START'.             
009530*01  AREA -COPY W61247      -PRE UT-                                      
009540                                                                          
009550                                                                          
009600 01  BORT-AREA-START             PIC X(16)   VALUE                        
009700                                             'BORT-AREA-START'.           
009800*01  AREA -COPY WDR301      -PRE BORT-                                    
009900                                                                          
009910                                                                          
010000 01  UT-AREA3-START              PIC X(16)   VALUE                        
010100                                             'UT-AREA3-START'.            
010200*01  AREA -COPY W41403S     -PRE UT3-                                     
010300                                                                          
010310                                                                          
010400 01  NYCKLAR-TILL-DLI.                                                    
010500     03  W-WDR301KY-X.                                                    
010600         05  W-WDR301KY          PIC X(27)    VALUE SPACE.                
010700     03  W-IDCPYTXT-X.                                                    
010800         05  W-IDCPYTXT          PIC X(08)    VALUE 'W61247  '.           
010900     03  W-IDARTNR-X.                                                     
011000         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
011100     03  W-KDSEGKEY-X.                                                    
011200         05  W-KDSEGKEY          PIC X(1)     VALUE '1'.                  
011210     03  W-WDE4C1KY-MIN-X.                                                
011220         05  W-E4C1KY-MIN-IDARTNR  PIC S9(9)  VALUE ZERO  COMP-3.         
011230         05  W-E4C1KY-MIN-IDPRODNR PIC S9(7)  VALUE ZERO  COMP-3.         
011240         05  W-E4C1KY-MIN-IDPURAD  PIC S9(5)  VALUE ZERO  COMP-3.         
011260     03  W-WDE4C1KY-MAX-X.                                                
011270         05 W-E4C1KY-MAX-IDARTNR   PIC S9(9)  VALUE ZERO  COMP-3.         
011280         05  W-E4C1KY-MAX-IDPRODNR PIC S9(7)  VALUE ZERO  COMP-3.         
011290         05  W-E4C1KY-MAX-IDPURAD  PIC S9(5)  VALUE ZERO  COMP-3.         
011291     03  W-WDE4KEY-X.                                                     
011292         05  W-E401KY-IDGMTREF.                                           
011293             07  W-E401KY-IDDISTR  PIC S9(5)  VALUE ZERO  COMP-3.         
011294             07  W-E401KY-IDKUNDNR PIC S9(7)  VALUE ZERO  COMP-3.         
011295             07  W-E401KY-IDKUNDRF.                                       
011296                 09 W-E401KY-IDORDNR5 PIC 9(05) VALUE ZERO.               
011297                 09 FILLER            PIC X(05) VALUE SPACE.              
011298         05  W-E401KY-IDPRODNR     PIC S9(7)  VALUE ZERO  COMP-3.         
011299         05  W-E401KY-IDPLKLST     PIC S9(3)  VALUE ZERO  COMP-3.         
011300     03  W-WDE4ASEQ-X.                                                    
011301         05  W-SEQA-IDDISTR       PIC S9(5)   VALUE ZERO COMP-3.          
011302         05  W-SEQA-IDKUNDNR      PIC S9(7)   VALUE ZERO COMP-3.          
011303         05  W-SEQA-IDKUNDRF      PIC X(10)   VALUE SPACE.                
011305     03  W-IDKOLLI-X.                                                     
011306         05  W-IDKOLLI           PIC S9(5)    VALUE ZERO COMP-3.          
011310                                                                          
011400*    --- STATUS-KOD FRÅN IMS                                              
011500 01  STATUS-WS                   PIC XX.                                  
011600     88  SEGMENT-FINNS                       VALUE '  '.                  
011700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011900     88  IMS-EJ-OK                           VALUE 'XD'.                  
012000                                                                          
012100 01  GODK-STATUSKODER.                                                    
012200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012300                                                                          
012400 01  ALL-SSA.                                                             
012410     03 SSA1                     PIC X(96).                               
012500     03 SSA2                     PIC X(64).                               
012600                                                                          
012700*    --- IMS FUNKTIONSKODER                                               
012800*01  -COPY W0003                                                          
012900                                                                          
013000*    ---  DLI INPUT-OUTPUT AREA                                           
013100                                                                          
013200 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDR301'.                    
013300 01  DLI-IO-WDR301.                                                       
013400*  03  -COPY WDR301                                                       
013500                                                                          
013600                                                                          
013700 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDK611'.                    
013800 01  DLI-IO-WDK611.                                                       
013900*  03  -COPY WDK611                                                       
014000                                                                          
014010                                                                          
014011 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDE4C1'.                    
014012 01  DLI-IO-WDE4C1.                                                       
014013*  03  -COPY WDE4C1                                                       
014014                                                                          
014015 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDE401'.                    
014016 01  DLI-IO-WDE401.                                                       
014017*  03  -COPY WDE401                                                       
014018                                                                          
014019 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDE421'.                    
014020 01  DLI-IO-WDE421.                                                       
014021*  03  -COPY WDE421                                                       
014022                                                                          
014023                                                                          
014030                                                                          
014100 LINKAGE SECTION.                                                         
014200*01  -COPY W0008  -PRE WDR3-                                              
014300     05  FILLER                  PIC X.                                   
014400                                                                          
014500*01  -COPY W0008  -PRE WDK6-                                              
014600     05  FILLER                  PIC X.                                   
014610                                                                          
014620*01  -COPY W0008  -PRE WDE4C-                                             
014630     05  FILLER                  PIC X.                                   
014700                                                                          
014701*01  -COPY W0008  -PRE WDE4-                                              
014702     05  FILLER                  PIC X.                                   
014703                                                                          
014704*01  -COPY W0008  -PRE WDE4A-                                             
014705     05  FILLER                  PIC X.                                   
014706                                                                          
014710                                                                          
014800 PROCEDURE DIVISION  USING WDR3-PCB WDK6-PCB                              
014810                           WDE4C-PCB WDE4-PCB WDE4A-PCB.                  
014900 MAIN SECTION.                                                            
015000     ENTRY 'DLITCBL' USING WDR3-PCB WDK6-PCB                              
015010                           WDE4C-PCB WDE4-PCB WDE4A-PCB.                  
015100                                                                          
015200     PERFORM A-INIT                                                       
015300                                                                          
015400     PERFORM IMS-GET-FILC-ROT                                             
015500     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
015600                                                                          
015700        MOVE FIL-WDR301-DATA TO UT-AREA                                   
015800        PERFORM B-SKAPA-SKRIV-UTPOSTER                                    
015900        PERFORM C-SKAPA-SKRIV-BORTPOST                                    
016000        PERFORM S10-NOLLSTALL                                             
016100                                                                          
016200        PERFORM IMS-GET-FILC-ROT                                          
016300     END-PERFORM                                                          
016400                                                                          
016500     PERFORM Z-FINIT                                                      
016600     MOVE ZERO TO RETURN-CODE                                             
016700     GOBACK                                                               
016800     .                                                                    
016900                                                                          
016910                                                                          
017000 A-INIT SECTION.                                                          
017010     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
017100                                                                          
017200     OPEN OUTPUT W61247                                                   
017300                 W61248                                                   
017400                 W6121F                                                   
017500                 W6124S                                                   
017600                                                                          
017700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017800     PERFORM S10-NOLLSTALL                                                
017900     .                                                                    
018000                                                                          
018010                                                                          
018100 B-SKAPA-SKRIV-UTPOSTER SECTION.                                          
018110     MOVE 'B-SKAPA-UTPOSTER' TO CURRENT-SECTION                           
018200                                                                          
018300     MOVE UT-IDARTNR TO W-IDARTNR                                         
018400                                                                          
018500     PERFORM IMS-GET-WDK611                                               
018600     IF SEGMENT-FINNS                                                     
018700        MOVE CLAG-PRARTSTD TO UT-PRARTSTD                                 
018710                              AVVIK-PRARTSTD                              
018800        MOVE CLAG-ADART    TO AVVIK-ADART                                 
018900     ELSE                                                                 
019000        MOVE ZERO          TO UT-PRARTSTD                                 
019001                              AVVIK-PRARTSTD                              
019010        MOVE SPACE         TO AVVIK-ADART                                 
019100     END-IF                                                               
019101                                                                          
019110     MOVE UT-DAFAKT        TO AVVIK-DAFAKT                                
019130     MOVE UT-IDARTNR       TO AVVIK-IDARTNR                               
019150     MOVE UT-IDDC-REC      TO AVVIK-IDDC-REC                              
019170     MOVE UT-IDDC-SEND     TO AVVIK-IDDC-SEND                             
019190     MOVE UT-IDFAKT        TO AVVIK-IDFAKT                                
019192     MOVE UT-IDKUNDNR      TO AVVIK-IDKUNDNR                              
019194     MOVE UT-IDKUNDRF      TO AVVIK-IDKUNDRF                              
019196     MOVE UT-IDKOLLI       TO AVVIK-IDKOLLI                               
019198     MOVE UT-KVAVIS        TO AVVIK-KVAVIS                                
019200     MOVE UT-KVANTAL       TO AVVIK-KVANTAL                               
019202     MOVE UT-AVVIKELSETYP  TO AVVIK-AVVIKELSETYP                          
019205     MOVE UT-KDSORT1       TO AVVIK-KDSORT1                               
019207     MOVE UT-FLINLREP      TO AVVIK-FLINLREP                              
019209     MOVE UT-DAREGDAT      TO AVVIK-DAREGDAT                              
019211     MOVE UT-IDUSER        TO AVVIK-IDUSER-BIN                            
019212     MOVE SPACE            TO AVVIK-IDPRC                                 
019213                              AVVIK-KDPRCGRP                              
019214                                                                          
019215     PERFORM BA-GET-IDDISTR                                               
019216     PERFORM BB-GET-WDE4-INFO                                             
019217                                                                          
019220     MOVE UT-IDDC-SEND TO WS-IDDC                                         
019300     IF CDC-SE AND UT-FLINLREP = 'J'                                      
019400        PERFORM S01-SKRIV-W61247                                          
019500     END-IF                                                               
019510                                                                          
019600     IF (FIL-IDPGM = 'WL010900') AND                                      
019700        UT-FLINLREP     = 'J'    AND                                      
019800        UT-AVVIKELSETYP = 'DAM'                                           
019900       PERFORM BC-SKAPA-FIL-AVVIKELSER                                    
020000     END-IF                                                               
020100     MOVE UT-IDDC-SEND TO WS-IDDC                                         
020200     IF GOOD-DDC                                                          
020300        MOVE '11' TO UT-IDDC-SEND                                         
020400     END-IF                                                               
020500     PERFORM S03-SKRIV-W6121F                                             
020600     .                                                                    
020700                                                                          
020710                                                                          
020800 BA-GET-IDDISTR          SECTION.                                         
020810     MOVE 'BA-GET-IDDISTR  ' TO CURRENT-SECTION                           
020900                                                                          
020901     MOVE AVVIK-IDDC-REC     TO WS-IDDC-REC                               
020902     MOVE AVVIK-IDDC-SEND    TO WS-IDDC-SEND                              
020903                                                                          
020904     SEARCH ALL WWDC03-IDDISTR                                            
020905       AT END                                                             
020906       MOVE ZERO     TO AVVIK-IDDISTR                                     
020907         MOVE 'EJ TRÄFF I TABELL WWDC03' TO FELTEXT-STR                   
020908         DISPLAY FELTEXT                                                  
020909*        CALL FELLOG                                                      
020910       WHEN WWDC03-IDDC-SEND-REC(WWDC03-IX) = WS-IDDC-SEND-REC            
020911         MOVE WWDC03-SOK-IDDISTR(WWDC03-IX) TO AVVIK-IDDISTR              
020912     END-SEARCH                                                           
020913     .                                                                    
020914                                                                          
020915                                                                          
020916 BB-GET-WDE4-INFO        SECTION.                                         
020917     MOVE 'BB-GET-WDE4     ' TO CURRENT-SECTION                           
020918                                                                          
020919     MOVE SPACE                  TO AVVIK-IDUSER-PIC                      
020920     MOVE ZERO                   TO AVVIK-IDPRODNR                        
020921                                    AVVIK-KDFRAKT                         
020922                                    AVVIK-KDORDKL                         
020923                                    AVVIK-IDORDER                         
020924                                    AVVIK-IDPLKLST                        
020925                                                                          
020926     MOVE LOW-VALUE              TO W-WDE4C1KY-MIN-X                      
020927     MOVE HIGH-VALUE             TO W-WDE4C1KY-MAX-X                      
020928     MOVE AVVIK-IDARTNR          TO W-E4C1KY-MIN-IDARTNR                  
020929                                    W-E4C1KY-MAX-IDARTNR                  
020930     MOVE AVVIK-IDDISTR          TO W-E401KY-IDDISTR                      
020931     MOVE AVVIK-IDKUNDNR         TO W-E401KY-IDKUNDNR                     
020932     MOVE AVVIK-IDKUNDRF         TO W-E401KY-IDKUNDRF                     
020933                                                                          
020934     PERFORM IMS-GU-WDE4C1                                                
020935     IF SEGMENT-FINNS                                                     
020936        MOVE SEQC-IDPRODNR       TO W-E401KY-IDPRODNR                     
020937                                    AVVIK-IDPRODNR                        
020938        MOVE SEQC-IDPLKLST       TO W-E401KY-IDPLKLST                     
020939                                    AVVIK-IDPLKLST                        
020940        PERFORM IMS-GU-WDE401                                             
020942        MOVE KORD-IDUSER         TO AVVIK-IDUSER-PIC                      
020943        MOVE KORD-KDFRAKT        TO AVVIK-KDFRAKT                         
020944        MOVE KORD-KDORDKL        TO AVVIK-KDORDKL                         
020945        MOVE KORD-IDORDER        TO AVVIK-IDORDER                         
020947     ELSE                                                                 
020948        PERFORM BBA-LETA-ORDERINFO                                        
020949     END-IF                                                               
020950     .                                                                    
020951                                                                          
020952                                                                          
020953 BBA-LETA-ORDERINFO      SECTION.                                         
020954     MOVE 'BBA-LETA-ORDER  ' TO CURRENT-SECTION                           
020960                                                                          
020961*--  DET HÄNDER ATT DET FINNS RAPPORTERADE ARTIKLAR I KOLLIT              
020962*--  SOM INTE FINNS PÅ ORDERN                                             
020963*--  I DESSA FALL FÖRSÖKER VI ÄNDÅ LETA RÄTT PÅ ORDERN GENOM              
020964*--  ATT HITTA EN ANNAN ARTIKEL I SAMMA KOLLI                             
020965                                                                          
020966     MOVE AVVIK-IDDISTR  TO W-SEQA-IDDISTR                                
020967     MOVE AVVIK-IDKUNDNR TO W-SEQA-IDKUNDNR                               
020968     MOVE AVVIK-IDKUNDRF TO W-SEQA-IDKUNDRF                               
020969                                                                          
020972     MOVE AVVIK-IDKOLLI  TO W-IDKOLLI                                     
020973                                                                          
020974     PERFORM IMS-GU-WDE401-ASEQ                                           
020975     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
020976          OR AVVIK-IDORDER NOT = ZERO                                     
020977                                                                          
020978        PERFORM IMS-GNP-WDE421                                            
020979        IF SEGMENT-FINNS                                                  
020980           MOVE KORD-IDPRODNR TO AVVIK-IDPRODNR                           
020981           MOVE KORD-IDPLKLST TO AVVIK-IDPLKLST                           
020982           MOVE KORD-IDUSER      TO AVVIK-IDUSER-PIC                      
020983           MOVE KORD-KDFRAKT     TO AVVIK-KDFRAKT                         
020984           MOVE KORD-KDORDKL     TO AVVIK-KDORDKL                         
020985           MOVE KORD-IDORDER     TO AVVIK-IDORDER                         
020986        ELSE                                                              
020987           PERFORM IMS-GN-WDE401-ASEQ                                     
020988        END-IF                                                            
020989     END-PERFORM                                                          
020990     .                                                                    
020991                                                                          
020992                                                                          
020993 BC-SKAPA-FIL-AVVIKELSER SECTION.                                         
020994     MOVE 'BC-SKAPA-FIL-AVV' TO CURRENT-SECTION                           
020995                                                                          
021000     MOVE 'SCR'                  TO UT3-IDPTYP                            
021100     MOVE 1                      TO UT3-KDSORT1                           
021200     MOVE UT-IDDC-REC            TO WBDC-IDDC                             
021300     CALL WL10WBDC USING WBDC-AREA                                        
021400     IF WBDC-FLWEBDC = JA                                                 
021500       MOVE WBDC-KDMFUP          TO UT3-KDMFUP                            
021600     ELSE                                                                 
021700       MOVE SPACE                TO UT3-KDMFUP                            
021800     END-IF                                                               
021900                                                                          
022000     MOVE UT-IDDC-REC            TO UT3-IDDC                              
022100     MOVE ZERO                   TO UT3-IDDISTR                           
022200     MOVE UT-IDKUNDNR            TO UT3-IDKUNDNR                          
022300     MOVE UT-IDKUNDRF            TO UT3-IDKUNDRF                          
022400     MOVE UT-IDARTNR             TO UT3-IDARTNR                           
022500     MOVE UT-KVANTAL             TO UT3-KVBEART                           
022600     MOVE UT-KVANTAL             TO UT3-KVBEART-Q                         
022700     MOVE UT-PRARTSTD            TO UT3-PRARTNTO                          
022800                                                                          
022900     COMPUTE UT3-SUARTNTO = UT-PRARTSTD *                                 
023000                            UT-KVANTAL                                    
023100     MOVE 'REFSCRAP'             TO UT3-BERADREF                          
023200                                                                          
023300     PERFORM S04-SKRIV-W6124S                                             
023400     .                                                                    
023500                                                                          
023510                                                                          
023600 C-SKAPA-SKRIV-BORTPOST SECTION.                                          
023610     MOVE 'S-SKAPA-BORTPOST' TO CURRENT-SECTION                           
023700                                                                          
023800     MOVE FIL-WDR301 TO BORT-FIL-WDR301                                   
023900     PERFORM S02-SKRIV-W61248                                             
024000     .                                                                    
024100                                                                          
024110                                                                          
024200 Z-FINIT SECTION.                                                         
024210     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
024300                                                                          
024400     CLOSE W61247                                                         
024500           W61248                                                         
024600           W6121F                                                         
024700           W6124S                                                         
024800                                                                          
024900     MOVE 'S' TO POSTSUM-OPKOD                                            
025000     CALL POSTSUM USING POSTSUM-PARM                                      
025100     .                                                                    
025200                                                                          
025210                                                                          
025300 S01-SKRIV-W61247 SECTION.                                                
025400                                                                          
025500     WRITE AVVIK-POST FROM AVVIK-AREA                                     
025600                                                                          
025700     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
025800     MOVE 'W61247 '  TO POSTSUM-FDNAMN                                    
025900     MOVE 'W61247D1' TO POSTSUM-DDNAMN2                                   
026000     CALL POSTSUM USING POSTSUM-PARM                                      
026100     .                                                                    
026200                                                                          
026210                                                                          
026300 S02-SKRIV-W61248 SECTION.                                                
026400                                                                          
026500     WRITE BORT-POST FROM BORT-AREA                                       
026600                                                                          
026700     MOVE SPACE       TO POSTSUM-TRANSTYP                                 
026800     MOVE 'W61248 '   TO POSTSUM-FDNAMN                                   
026900     MOVE 'W61247D2'  TO POSTSUM-DDNAMN2                                  
027000     CALL POSTSUM USING POSTSUM-PARM                                      
027100     .                                                                    
027200                                                                          
027210                                                                          
027300 S03-SKRIV-W6121F SECTION.                                                
027400                                                                          
027500     WRITE UT2-POST FROM UT-AREA                                          
027600                                                                          
027700     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
027800     MOVE 'W6121F '  TO POSTSUM-FDNAMN                                    
027900     MOVE 'W61247D3' TO POSTSUM-DDNAMN2                                   
028000     CALL POSTSUM USING POSTSUM-PARM                                      
028100     .                                                                    
028200                                                                          
028210                                                                          
028300 S04-SKRIV-W6124S SECTION.                                                
028400                                                                          
028500     WRITE UT3-POST FROM UT3-AREA                                         
028600                                                                          
028700     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
028800     MOVE 'W6124S '  TO POSTSUM-FDNAMN                                    
028900     MOVE 'W61247D4' TO POSTSUM-DDNAMN2                                   
029000     CALL POSTSUM USING POSTSUM-PARM                                      
029100     .                                                                    
029200                                                                          
029210                                                                          
029300 S10-NOLLSTALL SECTION.                                                   
029400                                                                          
029500     MOVE SPACE TO UT-IDDC-REC                                            
029600                   UT-IDDC-SEND                                           
029700                   UT-IDKUNDRF                                            
029800                   UT-AVVIKELSETYP                                        
029900                   UT-FLINLREP                                            
030000     MOVE ZERO  TO UT-IDFAKT                                              
030100                   UT-IDKOLLI                                             
030200                   UT-DAFAKT                                              
030300                   UT-IDARTNR                                             
030400                   UT-KVANTAL                                             
030500                   UT-KVAVIS                                              
030600                   UT-PRARTSTD                                            
030700     .                                                                    
030800                                                                          
030810                                                                          
030900* --- IMS SEKTIONER ---                                                   
031000                                                                          
031100 IMS-GET-FILC-ROT SECTION.                                                
031110     MOVE 'GET-FILC-ROT    ' TO CURRENT-IMS-SECTION                       
031120                                                                          
031130     MOVE SPACE            TO ALL-SSA                                     
031200     STRING 'WDR301  (IDCPYTXT =' W-IDCPYTXT-X ')'                        
031300       DELIMITED BY SIZE INTO SSA1                                        
031400     MOVE '  GEGB'         TO GODK-STATUSKODER                            
031500     CALL CBLTDLI USING GN WDR3-PCB DLI-IO-WDR301 SSA1                    
031600     MOVE WDR3-STATUS-CODE TO STATUS-WS                                   
031700     PERFORM IMS-STATUSKONTROLL                                           
031800     .                                                                    
031900                                                                          
032000 IMS-GET-WDK611 SECTION.                                                  
032010     MOVE 'GET-WDK611      ' TO CURRENT-IMS-SECTION                       
032020                                                                          
032030     MOVE SPACE            TO ALL-SSA                                     
032100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
032200       DELIMITED BY SIZE INTO SSA1                                        
032300     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
032400       DELIMITED BY SIZE INTO SSA2                                        
032500     MOVE '  GE'           TO GODK-STATUSKODER                            
032600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
032700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
032800     PERFORM IMS-STATUSKONTROLL                                           
032900     .                                                                    
033000                                                                          
033010 IMS-GU-WDE4C1         SECTION.                                           
033011     MOVE 'GU-WDE4C1       ' TO CURRENT-IMS-SECTION                       
033012                                                                          
033013     MOVE SPACE             TO ALL-SSA                                    
033030     STRING 'WDE4C1  (WDE4C1KY>=' W-WDE4C1KY-MIN-X                        
033040                    '&WDE4C1KY<=' W-WDE4C1KY-MAX-X                        
033050                    '&IDGMTREF =' W-E401KY-IDGMTREF ')'                   
033090        DELIMITED BY SIZE INTO SSA1                                       
033091     MOVE '  GE'            TO GODK-STATUSKODER                           
033092     CALL CBLTDLI USING GU WDE4C-PCB DLI-IO-WDE4C1 SSA1                   
033093     MOVE WDE4C-STATUS-CODE TO STATUS-WS                                  
033094     PERFORM IMS-STATUSKONTROLL                                           
033095     .                                                                    
033096                                                                          
033097 IMS-GU-WDE401 SECTION.                                                   
033098     MOVE 'GU-WDE401       ' TO CURRENT-IMS-SECTION                       
033099                                                                          
033100     MOVE SPACE             TO ALL-SSA                                    
033101     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
033102        DELIMITED BY SIZE INTO SSA1                                       
033103     MOVE '  '              TO GODK-STATUSKODER                           
033104     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE401 SSA1                    
033105     MOVE WDE4-STATUS-CODE  TO STATUS-WS                                  
033106     PERFORM IMS-STATUSKONTROLL                                           
033107     .                                                                    
033108                                                                          
033121 IMS-GU-WDE401-ASEQ SECTION.                                              
033122     MOVE 'GU-WDE401-ASEQ  ' TO CURRENT-IMS-SECTION                       
033123                                                                          
033124     MOVE SPACE             TO ALL-SSA                                    
033125     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
033126          DELIMITED BY SIZE INTO SSA1                                     
033127     MOVE '  GE' TO GODK-STATUSKODER                                      
033128     CALL CBLTDLI USING GU WDE4A-PCB DLI-IO-WDE401 SSA1                   
033129     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
033130     PERFORM IMS-STATUSKONTROLL                                           
033131     .                                                                    
033132                                                                          
033133 IMS-GN-WDE401-ASEQ SECTION.                                              
033134     MOVE 'GN-WDE401-ASEQ  ' TO CURRENT-IMS-SECTION                       
033135                                                                          
033136     MOVE SPACE             TO ALL-SSA                                    
033137     STRING 'WDE401  (WDE4ASEQ =' W-WDE4ASEQ-X ')'                        
033138          DELIMITED BY SIZE INTO SSA1                                     
033139     MOVE '  GEGB' TO GODK-STATUSKODER                                    
033140     CALL CBLTDLI USING GN WDE4A-PCB DLI-IO-WDE401 SSA1                   
033141     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
033142     PERFORM IMS-STATUSKONTROLL                                           
033143     .                                                                    
033150                                                                          
033151 IMS-GNP-WDE421      SECTION.                                             
033152     MOVE 'GNP-WDE421      ' TO CURRENT-IMS-SECTION                       
033153                                                                          
033154     MOVE SPACE             TO ALL-SSA                                    
033155     STRING 'WDE421  (IDKOLLI  =' W-IDKOLLI-X ')'                         
033156        DELIMITED BY SIZE INTO SSA1                                       
033157     MOVE '  GE'            TO GODK-STATUSKODER                           
033158     CALL CBLTDLI USING GNP WDE4A-PCB DLI-IO-WDE421 SSA1                  
033159     MOVE WDE4A-STATUS-CODE  TO STATUS-WS                                 
033160     PERFORM IMS-STATUSKONTROLL                                           
033161     .                                                                    
033162                                                                          
033170 IMS-STATUSKONTROLL SECTION.                                              
033200     SET STATUS-IX TO 1                                                   
033300     SEARCH GODK-STATUS                                                   
033400       AT END                                                             
033500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033600           DELIMITED BY SIZE INTO FELTEXT-STR                             
033700         DISPLAY FELTEXT                                                  
033800         CALL FELLOG                                                      
033900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034000         CONTINUE                                                         
034100     END-SEARCH                                                           
034200     .                                                                    
