001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W3717600.                                                
001200 AUTHOR.         GAVIN SMITH.                                             
001300 DATE-WRITTEN.   99/12/28.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        PROGRAM THAT CREATES WEEKS BALANCE FOR EACH DISTRICT AND         
001800*        ACCOUNT UPDATED ON THE WDGX3158 REGISTER.                        
001900*        FIRST GET INFO FROM WDGX3158 REGISTER, ADD WEEKS SUM             
002000*        TRANS AND CALCULATE UTGÅENDE BALANS.                             
002100*                                                                         
002210*        PROGRAMMET LÄSER      WDR1                                       
002300*                                                                         
002400*    ABENDKODER:                                                          
002500*        U0016 -  . . . .                                                 
002600*        U1000 -  . . . .                                                 
002700*                                                                         
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003501     SKIP2                                                                
003502*          --- SUMMARYFILE OF WEEKS TRANSACTIONS                          
003503     SELECT W37142                     ASSIGN TO W37176D1.                
003504     SKIP2                                                                
003505*          --- CUSTOMER/ACCOUNT WEEKLY UPDATE FILE                        
003510     SELECT W37131                     ASSIGN TO W37176D2.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP2                                                                
004000 FILE SECTION.                                                            
004101     SKIP3                                                                
004102 FD  W37142                                                               
004103     RECORDING       F                                                    
004104     BLOCK CONTAINS  0.                                                   
004105                                                                          
004106*01  -COPY W37109      -L.                                                
004107     SKIP3                                                                
004108 FD  W37131                                                               
004109     RECORDING       F                                                    
004110     BLOCK CONTAINS  0.                                                   
004111                                                                          
004120*01  POST -COPY W37107 -PRE  KUND-  -L.                                   
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W3717600'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004901                                                                          
004902 77  W37142-EOF-SW               PIC X       VALUE 'N'.                   
004910     88  END-OF-W37142                       VALUE 'J'.                   
005000     EJECT                                                                
005100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005200 01  FILLER REDEFINES DAGENS-DATUM.                                       
005300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005600     EJECT                                                                
005610 01  W-DAAAVV                    PIC 9(6)    VALUE ZERO.                  
005620 01  FILLER REDEFINES W-DAAAVV.                                           
005630     03 W-SEKEL                  PIC 9(2).                                
005640     03 W-AAR                    PIC 9(2).                                
005650     03 W-VECKA                  PIC 9(2).                                
005660     EJECT                                                                
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800*                                                                         
005900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006210     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006220     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006230     SKIP2                                                                
006240*    --- PARAMETRAR TILL DATKORT                                          
006250*                                                                         
006260 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
006270     SKIP2                                                                
006280*01  -COPY WDATKORT                                                       
006290     EJECT                                                                
006300     SKIP2                                                                
006400*    --- PARAMETRAR TILL ABEND                                            
006500                                                                          
006600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006900     SKIP2                                                                
007000 01  FELTEXT.                                                             
007100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007301     EJECT                                                                
007302*    --- PARAMETRAR TILL POSTSUM                                          
007303*                                                                         
007310*01  -COPY W0005   -PRE  POSTSUM-                                         
007501     EJECT                                                                
007502 01  SUM-AREA-START              PIC X(24)   VALUE                        
007503                                 'SUM-AREA-START  '.                      
007504     SKIP2                                                                
007505                                                                          
007506*01  AREA -COPY W37109     -PRE SUM-                                      
007507     EJECT                                                                
007508 01  KUND-AREA-START             PIC X(24)   VALUE                        
007509                                 'KUND-AREA-START  '.                     
007510     SKIP2                                                                
007511                                                                          
007520*01  AREA -COPY W37107     -PRE KUND-                                     
007600     EJECT                                                                
007700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007800*                                                                         
007900     EJECT                                                                
008000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008100     SKIP3                                                                
008200 01  NYCKLAR-TILL-DLI.                                                    
008301     03  W-WDGXKEY-X.                                                     
008302         05  W-3157-IDHTYP       PIC X(4)    VALUE '3157'.                
008303         05  W-3157-LOW-VALUE    PIC X(26)   VALUE LOW-VALUE.             
008304     03  W-KEY3158-X.                                                     
008310         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
008320         05  W-KDEXCHA           PIC S9(3)   VALUE ZERO COMP-3.           
008321     03  W-BAS3158-BAS.                                                   
008322         05  W-IDDISTR-BAS       PIC S9(5)   VALUE ZERO COMP-3.           
008323         05  W-KDEXCHA-BAS       PIC S9(3)   VALUE ZERO COMP-3.           
008330     03  W-KEYSUM-X.                                                      
008340         05  W-IDDISTR-SUM       PIC S9(5)   VALUE ZERO COMP-3.           
008350         05  W-KDEXCHA-SUM       PIC S9(3)   VALUE ZERO COMP-3.           
008400     SKIP2                                                                
008500*    --- STATUS-KOD FRÅN IMS                                              
008600 01  STATUS-WS                   PIC XX.                                  
008700     88  SEGMENT-FINNS                       VALUE '  '.                  
008800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008910     88  SEGMENT-UP                          VALUE 'GA'.                  
008920     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009000     SKIP2                                                                
009100 01  GODK-STATUSKODER.                                                    
009200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009300     SKIP3                                                                
009400 01  SSA1                        PIC X(64).                               
009500 01  SSA2                        PIC X(64).                               
009600     EJECT                                                                
009700*    --- IMS FUNKTIONSKODER                                               
009800*01  -COPY W0003                                                          
009900     EJECT                                                                
010100*    ---  DLI INPUT-OUTPUT AREA                                           
010205 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3158'.                    
010206 01  DLI-IO-WDGX3158.                                                     
010210*    03  -COPY WDGX3158                                                   
010500     EJECT                                                                
010600 LINKAGE SECTION.                                                         
010700                                                                          
010801                                                                          
010802*01  -COPY W0008  -PRE 3158-                                              
010810     05  FILLER                  PIC X.                                   
010900     EJECT                                                                
011001 PROCEDURE DIVISION  USING 3158-PCB.                                      
011002 MAIN SECTION.                                                            
011010     ENTRY 'DLITCBL' USING 3158-PCB.                                      
011100                                                                          
011300                                                                          
011400     PERFORM A-INIT                                                       
011500     PERFORM IMS-GU-WDR101                                                
011600     PERFORM IMS-GNP-WDGX3158                                             
011602     MOVE 3158-IDDISTR TO W-IDDISTR-BAS                                   
011603     MOVE 3158-KDEXCHA TO W-KDEXCHA-BAS                                   
011610     PERFORM S01-LAES-W37142                                              
011700     PERFORM UNTIL SEGMENT-UP OR SEGMENT-SLUT OR SEGMENT-SAKNAS           
011701       IF W-BAS3158-BAS =  W-KEYSUM-X                                     
011702         IF NOT END-OF-W37142                                             
011703           PERFORM B-FYLL-UTAREA                                          
011705         END-IF                                                           
011720         PERFORM UNTIL SUM-IDDISTR NOT = 3158-IDDISTR OR                  
011730                       SUM-KDEXCHA NOT = 3158-KDEXCHA OR                  
011740                       END-OF-W37142                                      
011800*          IF SUM-IDPTYP = 'FAK' OR 'KRE'                                 
011900*            COMPUTE KUND-SUPOINT-FAK =                                   
011910*                    KUND-SUPOINT-FAK + SUM-SUPOINT                       
011920*          END-IF                                                         
011921           IF SUM-IDPTYP = 'FAK'                                          
011922             COMPUTE KUND-SUPOINT-FAK =                                   
011923                     KUND-SUPOINT-FAK + SUM-SUPOINT                       
011924           END-IF                                                         
011925           IF SUM-IDPTYP = 'KRE'                                          
011926             COMPUTE KUND-SUPOINT-FAK =                                   
011927                     KUND-SUPOINT-FAK - SUM-SUPOINT                       
011928           END-IF                                                         
011930           IF SUM-IDPTYP = 'SUT'                                          
011940             MOVE SUM-SUPOINT           TO KUND-SUPOINT-RIT               
011950           END-IF                                                         
011960           IF SUM-IDPTYP = 'SUP'                                          
011970             MOVE SUM-SUPOINT           TO KUND-SUPOINT-PP                
011980           END-IF                                                         
011990           IF SUM-IDPTYP = 'RET'                                          
011991             MOVE SUM-SUPOINT           TO KUND-SUPOINT-RET               
011992           END-IF                                                         
011993           IF SUM-IDPTYP = 'ADJ'                                          
011994             MOVE SUM-SUPOINT           TO KUND-SUPOINT-ADJ               
011995           END-IF                                                         
011997           PERFORM S01-LAES-W37142                                        
011999         END-PERFORM                                                      
012001         COMPUTE KUND-SUPOINT-UTG = KUND-SUPOINT-ING -                    
012002                                    KUND-SUPOINT-FAK +                    
012003                                    KUND-SUPOINT-RET +                    
012004                                    KUND-SUPOINT-ADJ                      
012006         PERFORM IMS-GNP-WDGX3158                                         
012009         MOVE 3158-IDDISTR TO W-IDDISTR-BAS                               
012010         MOVE 3158-KDEXCHA TO W-KDEXCHA-BAS                               
012020       ELSE                                                               
012100         IF W-BAS3158-BAS > W-KEYSUM-X                                    
012200           PERFORM C-FYLL-UTAREA-FILINFO                                  
012202           PERFORM UNTIL SUM-IDDISTR NOT = KUND-IDDISTR OR                
012203                         SUM-KDEXCHA NOT = KUND-KDEXCHA OR                
012204                         END-OF-W37142                                    
012205*            IF SUM-IDPTYP = 'FAK' OR 'KRE'                               
012206*              COMPUTE KUND-SUPOINT-FAK =                                 
012207*                      KUND-SUPOINT-FAK + SUM-SUPOINT                     
012208*            END-IF                                                       
012209             IF SUM-IDPTYP = 'FAK'                                        
012210               COMPUTE KUND-SUPOINT-FAK =                                 
012211                       KUND-SUPOINT-FAK + SUM-SUPOINT                     
012212             END-IF                                                       
012213             IF SUM-IDPTYP = 'KRE'                                        
012214               COMPUTE KUND-SUPOINT-FAK =                                 
012215                       KUND-SUPOINT-FAK - SUM-SUPOINT                     
012216             END-IF                                                       
012217             IF SUM-IDPTYP = 'SUT'                                        
012218               MOVE SUM-SUPOINT           TO KUND-SUPOINT-RIT             
012219             END-IF                                                       
012220             IF SUM-IDPTYP = 'SUP'                                        
012221               MOVE SUM-SUPOINT           TO KUND-SUPOINT-PP              
012222             END-IF                                                       
012223             IF SUM-IDPTYP = 'RET'                                        
012224               MOVE SUM-SUPOINT           TO KUND-SUPOINT-RET             
012225             END-IF                                                       
012226             IF SUM-IDPTYP = 'ADJ'                                        
012227               MOVE SUM-SUPOINT           TO KUND-SUPOINT-ADJ             
012228             END-IF                                                       
012229             PERFORM S01-LAES-W37142                                      
012230           END-PERFORM                                                    
012231           COMPUTE KUND-SUPOINT-UTG = KUND-SUPOINT-ING -                  
012232                                      KUND-SUPOINT-FAK +                  
012233                                      KUND-SUPOINT-RET +                  
012234                                      KUND-SUPOINT-ADJ                    
012235         ELSE                                                             
012236           IF W-BAS3158-BAS < W-KEYSUM-X                                  
012237             PERFORM D-FYLL-UTAREA-BASINFO                                
012238             PERFORM IMS-GNP-WDGX3158                                     
012239             IF SEGMENT-FINNS                                             
012240               MOVE 3158-IDDISTR TO W-IDDISTR-BAS                         
012241               MOVE 3158-KDEXCHA TO W-KDEXCHA-BAS                         
012242             END-IF                                                       
012243           END-IF                                                         
012244         END-IF                                                           
012245       END-IF                                                             
012246                                                                          
012250       PERFORM S11-SKRIV-W37131                                           
012300       PERFORM E-NOLLSTALL-UTAREA                                         
012500     END-PERFORM                                                          
012600                                                                          
012700                                                                          
012800     PERFORM Z-FINIT                                                      
012900                                                                          
013000     MOVE ZERO TO RETURN-CODE                                             
013100     GOBACK                                                               
013200     .                                                                    
013300     EJECT                                                                
013400 A-INIT SECTION.                                                          
013501                                                                          
013510     OPEN INPUT  W37142                                                   
013601                                                                          
013610     OPEN OUTPUT W37131                                                   
013700                                                                          
013800     ACCEPT DAGENS-DATUM  FROM DATE                                       
013900     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
013901     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
013902                        W-AAR                                             
013903*    MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
013904*    MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
013905     MOVE D-VECKA   TO W-VECKA                                            
013906     IF DAGENS-DATUM-AAR > 50                                             
013907       MOVE 19      TO W-SEKEL                                            
013908     ELSE                                                                 
013909       MOVE 20      TO W-SEKEL                                            
013910     END-IF                                                               
013911     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013920     PERFORM E-NOLLSTALL-UTAREA                                           
014100     .                                                                    
014200     EJECT                                                                
014210 B-FYLL-UTAREA    SECTION.                                                
014211     MOVE W-DAAAVV           TO KUND-DAAAVV                               
014220     MOVE 3158-IDDISTR       TO KUND-IDDISTR                              
014221     MOVE 3158-KDEXCHA       TO KUND-KDEXCHA                              
014222     MOVE 3158-IDDISTR-BET   TO KUND-IDDISTR-BET                          
014223     MOVE 3158-SUPOINT-BAL   TO KUND-SUPOINT-ING                          
014224     MOVE 3158-FLEXCREP      TO KUND-FLEXCREP                             
014225     MOVE 3158-FLEXCDET      TO KUND-FLEXCDET                             
014226     MOVE 3158-IDMAIL        TO KUND-IDMAIL                               
014234     .                                                                    
014240     EJECT                                                                
014250 C-FYLL-UTAREA-FILINFO SECTION.                                           
014251     MOVE W-DAAAVV           TO KUND-DAAAVV                               
014260     MOVE SUM-IDDISTR        TO KUND-IDDISTR                              
014261     MOVE SUM-KDEXCHA        TO KUND-KDEXCHA                              
014262     MOVE SUM-IDDISTR-BET    TO KUND-IDDISTR-BET                          
014263     MOVE SUM-SUPOINT        TO KUND-SUPOINT-UTG                          
014264*    MOVE SUM-FLEXCREP       TO KUND-FLEXCREP                             
014265*    MOVE SUM-FLEXCDET       TO KUND-FLEXCDET                             
014266*    MOVE SUM-IDMAIL         TO KUND-IDMAIL                               
014270     .                                                                    
014280     EJECT                                                                
014290 D-FYLL-UTAREA-BASINFO SECTION.                                           
014291                                                                          
014292     MOVE W-DAAAVV           TO KUND-DAAAVV                               
014293     MOVE 3158-IDDISTR       TO KUND-IDDISTR                              
014294     MOVE 3158-KDEXCHA       TO KUND-KDEXCHA                              
014295     MOVE 3158-IDDISTR-BET   TO KUND-IDDISTR-BET                          
014296     MOVE 3158-SUPOINT-RIT   TO KUND-SUPOINT-RIT                          
014297*CHANGED LINE                                                             
014298     MOVE 0                  TO KUND-SUPOINT-RIT                          
014299     MOVE 3158-SUPOINT-PP    TO KUND-SUPOINT-PP                           
014300*CHANGED LINE                                                             
014301     MOVE 0                  TO KUND-SUPOINT-PP                           
014302     MOVE ZERO               TO KUND-SUPOINT-ING                          
014303*CHANGED LINE                                                             
014304     MOVE 3158-SUPOINT-BAL   TO KUND-SUPOINT-ING                          
014305     MOVE 3158-SUPOINT-BAL   TO KUND-SUPOINT-UTG                          
014306     MOVE ZERO               TO KUND-SUPOINT-FAK                          
014307     MOVE ZERO               TO KUND-SUPOINT-RET                          
014308     MOVE ZERO               TO KUND-SUPOINT-ADJ                          
014309     MOVE 3158-FLEXCREP      TO KUND-FLEXCREP                             
014310     MOVE 3158-FLEXCDET      TO KUND-FLEXCDET                             
014311     MOVE 3158-IDMAIL        TO KUND-IDMAIL                               
014312                                                                          
014313     .                                                                    
014314     EJECT                                                                
014320 E-NOLLSTALL-UTAREA SECTION.                                              
014330     MOVE ZERO               TO KUND-DAAAVV                               
014331                                KUND-IDDISTR                              
014332                                KUND-KDEXCHA                              
014333                                KUND-IDDISTR-BET                          
014334                                KUND-SUPOINT-RIT                          
014335                                KUND-SUPOINT-PP                           
014336                                KUND-SUPOINT-ING                          
014337                                KUND-SUPOINT-UTG                          
014338                                KUND-SUPOINT-FAK                          
014339                                KUND-SUPOINT-RET                          
014340                                KUND-SUPOINT-ADJ                          
014341                                                                          
014342     MOVE SPACE              TO KUND-FLEXCREP                             
014343                                KUND-FLEXCDET                             
014344                                KUND-IDMAIL                               
014345                                                                          
014346     .                                                                    
014350     EJECT                                                                
014400 Z-FINIT SECTION.                                                         
014401     CLOSE W37142                                                         
014410           W37131                                                         
014501     SKIP2                                                                
014502     MOVE 'S' TO POSTSUM-OPKOD                                            
014510     CALL POSTSUM USING POSTSUM-PARM                                      
014600     .                                                                    
014701     EJECT                                                                
014702 S01-LAES-W37142  SECTION.                                                
014703     READ W37142 INTO SUM-AREA                                            
014706                                                                          
014707     AT END                                                               
014708        MOVE HIGH-VALUE TO SUM-AREA                                       
014709                                                                          
014710        SET END-OF-W37142 TO TRUE                                         
014711                                                                          
014712     NOT AT END                                                           
014713        MOVE 'W37142' TO POSTSUM-FDNAMN                                   
014714        MOVE 'W37176D1' TO POSTSUM-DDNAMN2                                
014715*       -- ÄNDRA TILL MOVE SPACE OM FILEN SAKNAR POSTTYP                  
014716*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
014717        MOVE SUM-IDPTYP TO POSTSUM-TRANSTYP                               
014718        CALL POSTSUM USING POSTSUM-PARM                                   
014719     END-READ                                                             
014720     IF END-OF-W37142                                                     
014721       MOVE 99999              TO W-IDDISTR-SUM                           
014722       MOVE 99999              TO SUM-IDDISTR                             
014723       MOVE 999                TO W-KDEXCHA-SUM                           
014724       MOVE 999                TO SUM-KDEXCHA                             
014726                                                                          
014727     ELSE                                                                 
014728       MOVE SUM-IDDISTR        TO W-IDDISTR-SUM                           
014729       MOVE SUM-KDEXCHA        TO W-KDEXCHA-SUM                           
014731     END-IF                                                               
014740     .                                                                    
014801     EJECT                                                                
014802 S11-SKRIV-W37131 SECTION.                                                
014803                                                                          
014804     WRITE KUND-POST FROM KUND-AREA                                       
014805                                                                          
014806     MOVE 'UT-'       TO POSTSUM-TRANSTYP                                 
014807     MOVE 'W37131'    TO POSTSUM-FDNAMN                                   
014808     MOVE 'W37176D2'  TO POSTSUM-DDNAMN2                                  
014809     CALL POSTSUM USING POSTSUM-PARM                                      
014810     .                                                                    
015000     EJECT                                                                
015100 S99-ABEND SECTION.                                                       
015200                                                                          
015301     SKIP2                                                                
015302     MOVE 'S' TO POSTSUM-OPKOD                                            
015310     CALL POSTSUM USING POSTSUM-PARM                                      
015400     CALL ABEND USING RKOD-ABEND                                          
015500     .                                                                    
015600     EJECT                                                                
015700* --- IMS SEKTIONER ---                                                   
015800                                                                          
015901     EJECT                                                                
015902 IMS-GU-WDR101 SECTION.                                                   
015903                                                                          
015904     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-X ')'                         
015905          DELIMITED BY SIZE INTO SSA1                                     
015906*    MOVE 'WDGX3158  '      TO   SSA2                                     
015907     MOVE '  ' TO GODK-STATUSKODER                                        
015908     CALL CBLTDLI USING GU 3158-PCB DLI-IO-WDGX3158 SSA1                  
015909     MOVE 3158-STATUS-CODE TO STATUS-WS                                   
015910     PERFORM IMS-STATUSKONTROLL                                           
015911     .                                                                    
015912     EJECT                                                                
016001 IMS-GNP-WDGX3158 SECTION.                                                
016002     MOVE 'WDGX3158  '      TO   SSA1                                     
016003     MOVE '  GE'            TO   GODK-STATUSKODER                         
016004     CALL CBLTDLI USING GNP 3158-PCB DLI-IO-WDGX3158 SSA1                 
016005     MOVE 3158-STATUS-CODE  TO   STATUS-WS                                
016006     PERFORM IMS-STATUSKONTROLL                                           
016007     .                                                                    
016008     EJECT                                                                
016009 IMS-GN-WDGX3158 SECTION.                                                 
016010                                                                          
016020     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-X ')'                         
016030          DELIMITED BY SIZE INTO SSA1                                     
016040     STRING 'WDGX3158(KEY3158 >=' W-KEY3158-X ')'                         
016050          DELIMITED BY SIZE INTO SSA2                                     
016060     MOVE 'GAGBGE  ' TO GODK-STATUSKODER                                  
016070     CALL CBLTDLI USING GN  3158-PCB DLI-IO-WDGX3158 SSA1 SSA2            
016080     MOVE 3158-STATUS-CODE TO STATUS-WS                                   
016090     PERFORM IMS-STATUSKONTROLL                                           
016091     .                                                                    
016092     EJECT                                                                
016100 IMS-STATUSKONTROLL SECTION.                                              
016200                                                                          
016300     SET STATUS-IX TO 1                                                   
016400     SEARCH GODK-STATUS                                                   
016500       AT END                                                             
016600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016700           DELIMITED BY SIZE INTO FELTEXT                                 
016800         DISPLAY FELTEXT                                                  
016900         CALL FELLOG                                                      
017000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017100         CONTINUE                                                         
017200     END-SEARCH                                                           
017300     .                                                                    
