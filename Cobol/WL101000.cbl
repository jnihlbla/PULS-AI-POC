000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WL101000.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   01/02/08.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001110*        LÄSER FIL W61410/11 MED URVAL FRÅN BILD L136                     
001120*        SÄNDER DOKUMENTDATA TILL DISTRIBUTION & PRINT                    
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
001910     SKIP2                                                                
001920 CONFIGURATION SECTION.                                                   
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002401     SKIP2                                                                
002402*          --- PARMFIL                                                    
002403     SELECT WL10PP                     ASSIGN TO WL1010D1.                
002404     SKIP2                                                                
002405*          --- ARTINFO                                                    
002406     SELECT W61410                     ASSIGN TO WL1010D2.                
002407     SKIP2                                                                
002600*          --- SORTERINGSFIL                                              
002700     SELECT SORTFIL                    ASSIGN TO WL1010DS.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003301     SKIP3                                                                
003302 FD  WL10PP                                                               
003303     RECORDING       F                                                    
003304     BLOCK CONTAINS  0.                                                   
003306 01  PARM                  PIC X(80).                                     
003307     SKIP3                                                                
003308 FD  W61410                                                               
003309     RECORDING       F                                                    
003310     BLOCK CONTAINS  0.                                                   
003312*01  -COPY W61410      -L.                                                
003313     SKIP3                                                                
003500 SD  SORTFIL.                                                             
003610*01  POST -COPY W61410      -PRE SORT-                                    
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003810     SKIP2                                                                
003900*    -COPY WY2000W1                                                       
003901     EJECT                                                                
003910*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(8)    VALUE 'WL101000'.            
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300 77  WS-URVAL                    PIC X       VALUE 'N'.                   
004301 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004302 77  WS-ADRESS                   PIC X(50)                                
004303         VALUE 'CARPARTS.DAP.DISTRDOC'.                                   
004304 77  KDRC-DISPLAY                PIC Z(5).                                
004305 77  SW-HEAD-KLAR                PIC X       VALUE 'N'.                   
004306                                                                          
004309 77  WS-CP-UTF8                  PIC X(4)    VALUE 'UTF8'.                
004310 77  WS-CP-EBCDIC                PIC X(3)    VALUE '278'.                 
004320                                                                          
004400 01  WS-IDFKNGRP                 PIC 9(4)    VALUE ZERO.                  
004404 01  FILLER REDEFINES WS-IDFKNGRP.                                        
004405     03 WS-GRP-00                PIC 9(2).                                
004406     03 WS-RESTEN-00             PIC 9(2).                                
004407 01  FILLER REDEFINES WS-IDFKNGRP.                                        
004408     03 WS-GRP-000               PIC 9(1).                                
004409     03 WS-RESTEN-000            PIC 9(3).                                
004410                                                                          
004411 01  WS-PARM-IDFKNGRP            PIC 9(4)    VALUE ZERO.                  
004418 01  FILLER REDEFINES WS-PARM-IDFKNGRP.                                   
004419     03 WS-PARM-GRP-00           PIC 9(2).                                
004420     03 WS-PARM-RESTEN-00        PIC 9(2).                                
004421 01  FILLER REDEFINES WS-PARM-IDFKNGRP.                                   
004422     03 WS-PARM-GRP-000          PIC 9(1).                                
004423     03 WS-PARM-RESTEN-000       PIC 9(3).                                
004424                                                                          
004425 77  WL10PP-EOF-SW               PIC X       VALUE 'N'.                   
004426     88  END-OF-WL10PP                       VALUE 'J'.                   
004427                                                                          
004428 77  W61410-EOF-SW               PIC X       VALUE 'N'.                   
004430     88  END-OF-W61410                       VALUE 'J'.                   
004500                                                                          
004600 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
004700     88  END-OF-SORTFIL                      VALUE 'J'.                   
004800                                                                          
004810 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
004820 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
004830 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
004840                                                                          
004910 01  DAGENS-DATUM                PIC 9(6).                                
004911 01  DAGENS-KLOCKA.                                                       
004912     03  DAGENS-KLOCKA-1-4       PIC 9(4).                                
004913     03  FILLER                  PIC 9(4).                                
004914 01  DAGENS-DATUM-TOT.                                                    
004920     03  DAGENS-DATUM-KLOCKA.                                             
004921         05 DAGENS-DATUM-AAMMDD  PIC 9(6).                                
004930         05 DAGENS-KLOCKA-TTMM   PIC 9(4).                                
005400                                                                          
006810     EJECT                                                                
       01  STATUS-WS                   PIC XX.                                  
            88  SEGMENT-FINNS                       VALUE '  '.                 
            88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                 
            88  SEGMENT-SAKNAS                      VALUE 'GE'.                 
            88  BASEN-SLUT                          VALUE 'GB'.                 
            SKIP2                                                               
       01  GODK-STATUSKODER.                                                    
            03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.               
            SKIP3                                                               
       01  W-IDARTNR-X.                                                         
           03  W-IDARTNR           PIC S9(9)              COMP-3.               
       01  W-IDSKYLT-X.                                                         
           03  W-IDSKYLT           PIC X(3)    VALUE SPACE.                     
       01  W-IDDC-B6-X.                                                         
           03  W-IDDC-B6           PIC X(2)    VALUE SPACE.                     
       01  SSA1                        PIC X(224).                              
       01  SSA2                        PIC X(32).                               
           EJECT                                                                
      *    --- IMS FUNKTIONSKODER                                               
      *01  -COPY W0003                                                          
           EJECT                                                                
                                                                                
006820 01  DYNAMISKA-SUBPROGRAM.                                                
006830     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006840     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006850     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006860     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006870     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
006871     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
006880     SKIP2                                                                
006890 01  FELTEXT.                                                             
006891     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006892     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006896     EJECT                                                                
006905*    --- PARAMETRAR TILL POSTSUM                                          
006910*01  -COPY W0005   -PRE  POSTSUM-                                         
007101     EJECT                                                                
007107 01  PARM-AREA-START             PIC X(24)   VALUE                        
007108                                 'PARM-AREA-START '.                      
007109 01  PARM-AREA                   PIC X(28).                               
007110 01  FILLER REDEFINES PARM-AREA.                                          
007111     03 PARM-KDSORT1             PIC 9.                                   
007113     03 PARM-IDDC-BEST           PIC X(2).                                
007114     03 PARM-KDLEVSP             PIC 9(2).                                
007115     03 PARM-IDFKNGRP            PIC 9(4).                                
007116     03 PARM-FLKVANT             PIC X.                                   
007117     03 PARM-IDDC                PIC X(2).                                
007118     03 PARM-IDUSER              PIC X(8).                                
007119     03 PARM-SIGNON-IDUSER       PIC X(8).                                
007122     EJECT                                                                
007123                                                                          
007124 01  IN-AREA-START               PIC X(24)   VALUE                        
007125                                 'IN-AREA-START  '.                       
007127*01  AREA -COPY W61410     -PRE IN-                                       
007128     EJECT                                                                
007129                                                                          
007202 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
007203                                  'SORTWS-AREA-START  '.                  
007210*01  AREA -COPY W61410      -PRE SORTWS-                                  
007300 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
007400     EJECT                                                                
007401                                                                          
007410 01  UT-AREA-START               PIC X(24)   VALUE                        
007411                                 'UT-AREA-START  '.                       
007412*01  AREA -COPY W61410     -PRE UT-                                       
007413     EJECT                                                                
007414                                                                          
007420*    --- AREOR FÖR KOMMUNIKATION                                          
007430 01  FILLER                  PIC X(16)   VALUE 'SEND-CONTROL'.            
007440*01  -COPY WZ01SEND                                                       
007450     EJECT                                                                
007460 01  UT-AREA-START               PIC X(24)   VALUE                        
007470                                 'UT-AREA-START '.                        
007480 01  HDR-AREA.                                                            
007490*    03  -COPY WZ01REQU                                                   
007491*    03  -COPY WZ04HDR                                                    
007492     EJECT                                                                
007493 01  DOC-HEAD-AREA.                                                       
007494*    03  -COPY WL10101   -PRE UT1-                                        
007495     EJECT                                                                
007496 01  DOC-LINE-AREA.                                                       
007497*    03  -COPY WL10102   -PRE UT2-                                        
007498     EJECT                                                                
007499*01  -COPY WWDC99                                                         
007500                                                                          
007501     EJECT                                                                
007502 01  W-BEART                PIC X(25).                                    
007503 01  FILLER                 PIC X(16)  VALUE 'WTRAUTF8-AREA'.             
007504*01  -COPY WTRAUTF8                                                       
007505                                                                          
007506     EJECT                                                                
       01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
       01  DLI-IO-AREA-B601.                                                    
      *    03  -COPY WDB601                                                     
       01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD311'.                      
       01  DLI-IO-WDD311.                                                       
      *    03  -COPY WDD311                                                     
           EJECT                                                                
007507 LINKAGE SECTION.                                                         
007508*01  -COPY W0009   -PRE MSG-                                              
007509     EJECT                                                                
007510 01  DAP-PCB       PIC X.                                                 
007511     EJECT                                                                
      *01  -COPY W0008  -PRE WDB6-                                              
           05  FILLER                  PIC X.                                   
           EJECT                                                                
      *01  -COPY W0008  -PRE WDD3-                                              
           05  FILLER                  PIC X.                                   
           EJECT                                                                
007512                                                                          
007513 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB WDB6-PCB WDD3-PCB.             
007514 MAIN SECTION.                                                            
007515                                                                          
007516     ENTRY 'DLITCBL' USING MSG-PCB DAP-PCB WDB6-PCB WDD3-PCB.             
007520                                                                          
008000     PERFORM A-INIT                                                       
008010     PERFORM S01-LAS-WL10PP                                               
008020     DISPLAY 'SORT '         PARM-KDSORT1                                 
008030     DISPLAY 'IDDCBEST '     PARM-IDDC-BEST                               
008031     DISPLAY 'KDLEVSP '      PARM-KDLEVSP                                 
008032     DISPLAY 'KDFKNGRP '     PARM-IDFKNGRP                                
008033     DISPLAY 'FLKVANT '      PARM-FLKVANT                                 
008034     DISPLAY 'IDDC '         PARM-IDDC                                    
008035     DISPLAY 'IDUSER '       PARM-IDUSER                                  
008036     DISPLAY 'SIGNON '       PARM-SIGNON-IDUSER                           
008037                                                                          
008040     IF PARM-KDSORT1 = 0 OR 1                                             
008200        SORT SORTFIL ASCENDING SORT-IDARTNR                               
008300                               SORT-IDDC                                  
008310        INPUT PROCEDURE B-SORT-INPUT                                      
008311        OUTPUT PROCEDURE C-SORT-OUTPUT                                    
008312     ELSE                                                                 
008313        IF PARM-KDSORT1 = 2                                               
008320           SORT SORTFIL ASCENDING SORT-TISPARR-KVAL                       
008330                                  SORT-IDARTNR                            
008340                                  SORT-IDDC                               
008350           INPUT PROCEDURE B-SORT-INPUT                                   
008354           OUTPUT PROCEDURE C-SORT-OUTPUT                                 
008355        ELSE                                                              
008360           SORT SORTFIL ASCENDING SORT-IDFKNGRP                           
008370                                  SORT-IDARTNR                            
008380                                  SORT-IDDC                               
008381           INPUT PROCEDURE B-SORT-INPUT                                   
008382           OUTPUT PROCEDURE C-SORT-OUTPUT                                 
008383        END-IF                                                            
008384     END-IF                                                               
008386                                                                          
008387     IF SORT-RETURN NOT = 0                                               
008388        MOVE SORT-RETURN TO SORT-RETURN-X                                 
008389        STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                     
008390        DELIMITED BY SIZE INTO FELTEXT-STR                                
008391        DISPLAY FELTEXT                                                   
008392        MOVE RKOD-ABEND-NO-DUMP TO RKOD-ABEND                             
008393        PERFORM S99-ABEND                                                 
008396     ELSE                                                                 
008397        IF WZ04-SEND-IDCOM > ZERO                                         
008398           PERFORM S90-SEND-CLOSE                                         
008400           MOVE ZERO TO WZ04-SEND-IDCOM                                   
008410        END-IF                                                            
008500                                                                          
009500        PERFORM Z-FINIT                                                   
009700        MOVE ZERO TO RETURN-CODE                                          
009800        GOBACK                                                            
009900     END-IF                                                               
010100     .                                                                    
010200     EJECT                                                                
010210                                                                          
010300 A-INIT SECTION.                                                          
010402     OPEN INPUT  WL10PP                                                   
010410                 W61410                                                   
010600                                                                          
010700     ACCEPT DAGENS-DATUM  FROM DATE                                       
010800     ACCEPT DAGENS-KLOCKA FROM TIME                                       
010801     MOVE DAGENS-DATUM      TO DAGENS-DATUM-AAMMDD                        
010802     MOVE DAGENS-KLOCKA-1-4 TO DAGENS-KLOCKA-TTMM                         
010810     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
010820     MOVE NEJ   TO SW-HEAD-KLAR                                           
010900     .                                                                    
011001     EJECT                                                                
011002                                                                          
011003 B-SORT-INPUT SECTION.                                                    
011005     PERFORM S02-LAES-W61410                                              
011006     PERFORM UNTIL END-OF-W61410                                          
011007       PERFORM BA-URVAL                                                   
011008       IF WS-URVAL = JA                                                   
011009         MOVE IN-AREA TO SORTWS-AREA                                      
011011                                                                          
011012         IF PARM-KDSORT1 = 2                                              
011013            MOVE SORTWS-TISPARR-KVAL TO TMP1-YYMMDD                       
011014            PERFORM WY2000P1                                              
011015            MOVE TMP1-YYMMDD         TO SORTWS-TISPARR-KVAL               
011016         END-IF                                                           
011017         PERFORM S31-SORT-RELEASE                                         
011018       END-IF                                                             
011019       PERFORM S02-LAES-W61410                                            
011020     END-PERFORM                                                          
011030     .                                                                    
011100     EJECT                                                                
011120                                                                          
011121 BA-URVAL SECTION.                                                        
011130     MOVE NEJ          TO WS-URVAL                                        
011151     MOVE IN-IDFKNGRP  TO WS-IDFKNGRP                                     
011152                                                                          
011153     IF PARM-IDDC = IN-IDDC                                               
011154        MOVE JA        TO WS-URVAL                                        
011155     ELSE                                                                 
011156        MOVE NEJ       TO WS-URVAL                                        
011175     END-IF                                                               
011176                                                                          
011177     IF WS-URVAL = JA                                                     
011178        IF PARM-IDUSER = SPACE                                            
011179           CONTINUE                                                       
011180        ELSE                                                              
011181           IF PARM-IDUSER = IN-IDUSER-SPKVAL                              
011182              CONTINUE                                                    
011190           ELSE                                                           
011191              MOVE NEJ TO WS-URVAL                                        
011194           END-IF                                                         
011195        END-IF                                                            
011196     END-IF                                                               
011197                                                                          
011198     IF WS-URVAL = JA                                                     
011199        IF PARM-FLKVANT = SPACE                                           
011200           CONTINUE                                                       
011201        ELSE                                                              
011202           IF PARM-FLKVANT = JA                                           
011203              IF IN-KVSPARR-KVAL > 0                                      
011204                 CONTINUE                                                 
011205              ELSE                                                        
011206                 MOVE NEJ    TO WS-URVAL                                  
011209              END-IF                                                      
011210           ELSE                                                           
011211              IF PARM-FLKVANT = NEJ                                       
011212                 IF IN-KVSPARR-KVAL = 0                                   
011213                    CONTINUE                                              
011214                 ELSE                                                     
011215                    MOVE NEJ TO WS-URVAL                                  
011216                                                                          
011217                 END-IF                                                   
011218              END-IF                                                      
011219           END-IF                                                         
011220        END-IF                                                            
011221     END-IF                                                               
011222                                                                          
011223     IF WS-URVAL = JA                                                     
011224        IF PARM-KDLEVSP = 0                                               
011225           CONTINUE                                                       
011226        ELSE                                                              
011227           IF PARM-KDLEVSP = IN-KDLEVSP                                   
011228              CONTINUE                                                    
011229           ELSE                                                           
011230              MOVE NEJ TO WS-URVAL                                        
011233           END-IF                                                         
011234        END-IF                                                            
011235     END-IF                                                               
011240                                                                          
011243     IF WS-URVAL = JA                                                     
011245        IF PARM-IDFKNGRP = ZERO                                           
011246           CONTINUE                                                       
011247        ELSE                                                              
011248           IF WS-PARM-RESTEN-000 = ZERO                                   
011249              IF WS-PARM-GRP-000 = WS-GRP-000                             
011250                 CONTINUE                                                 
011251              ELSE                                                        
011252                 MOVE NEJ    TO WS-URVAL                                  
011254              END-IF                                                      
011255           ELSE                                                           
011256              IF WS-PARM-RESTEN-00 = ZERO                                 
011257                 IF WS-PARM-GRP-00 = WS-GRP-00                            
011258                    CONTINUE                                              
011259                 ELSE                                                     
011260                    MOVE NEJ TO WS-URVAL                                  
011262                 END-IF                                                   
011263              ELSE                                                        
011270                 IF IN-IDFKNGRP = PARM-IDFKNGRP                           
011271                    CONTINUE                                              
011272                 ELSE                                                     
011273                    MOVE NEJ TO WS-URVAL                                  
011275                 END-IF                                                   
011276              END-IF                                                      
011277           END-IF                                                         
011278        END-IF                                                            
011279     END-IF                                                               
011280     .                                                                    
011281     EJECT                                                                
011282                                                                          
011290 C-SORT-OUTPUT SECTION.                                                   
011400     PERFORM S32-SORT-RETURN                                              
011500     PERFORM UNTIL END-OF-SORTFIL                                         
011600       IF PARM-KDSORT1 = 2                                                
011601          MOVE SORTWS-TISPARR-KVAL TO TMP1-YYMMDD                         
011602          PERFORM WY2000P1                                                
011603          MOVE TMP1-YYMMDD         TO SORTWS-TISPARR-KVAL                 
011604       END-IF                                                             
011610       MOVE SORTWS-AREA            TO UT-AREA                             
011620       MOVE PARM-KDSORT1           TO UT-KDSORT1                          
011630       MOVE PARM-IDDC-BEST         TO UT-IDDC-BEST                        
011640       IF PARM-IDDC = SPACE                                               
011650       AND (PARM-KDLEVSP = ZERO OR 20)                                    
011670          IF SORTWS-KDLEVSP = 20                                          
011672             CONTINUE                                                     
011673          ELSE                                                            
011674             PERFORM CA-SEND-DAP                                          
011675                                                                          
011676          END-IF                                                          
011680       ELSE                                                               
011700          PERFORM CA-SEND-DAP                                             
011810       END-IF                                                             
011820       PERFORM S32-SORT-RETURN                                            
011900     END-PERFORM                                                          
012000     .                                                                    
012100     EJECT                                                                
012101                                                                          
012110 CA-SEND-DAP SECTION.                                                     
012111*    DISPLAY '** C-SEND-DAP ***'                                          
012112     IF SW-HEAD-KLAR = NEJ                                                
012113        MOVE 1                   TO REQU-IDMSGVER                         
012114        MOVE 'E'                 TO REQU-KDPGMACT                         
012120                                                                          
012121        MOVE PARM-SIGNON-IDUSER   TO REQU-IDUSER                          
012122                                                                          
012123        MOVE SPACE               TO HDR-IDOUTREC                          
012124        MOVE 'QUALITY-BLOCKS'    TO HDR-IDOUTTYPE                         
012125        MOVE PARM-IDDC           TO HDR-IDOUTREC(1:2)                     
012131        MOVE PARM-SIGNON-IDUSER  TO HDR-IDOUTREC(3:8)                     
012132        MOVE DAGENS-DATUM-KLOCKA TO HDR-IDLIST                            
012133                                                                          
012134                                                                          
012135        IF WZ04-SEND-IDCOM = ZERO                                         
012136           PERFORM S90-SEND-OPEN                                          
012137           MOVE SEND-IDCOM       TO WZ04-SEND-IDCOM                       
012138        END-IF                                                            
012139        PERFORM S90-PUT-HEADER                                            
012140*       DISPLAY 'HEADER ' HDR-AREA                                        
012141        MOVE '1'                 TO UT1-IDAFPRCD                          
012142                                                                          
012148        MOVE PARM-SIGNON-IDUSER  TO UT1-IDUSER                            
012149        MOVE PARM-IDDC-BEST      TO UT1-IDDC-BEST                         
012150                                                                          
012151        PERFORM S90-PUT-DOC-HEAD                                          
012152*       DISPLAY 'DOC-HEAD ' UT1-WL10101                                   
012153        MOVE JA                  TO SW-HEAD-KLAR                          
012154     END-IF                                                               
012155                                                                          
012156     MOVE '2'                    TO UT2-IDAFPRCD                          
012157                                                                          
012158     MOVE UT-IDDC TO WS-IDDC                                              
                           W-IDDC-B6                                            
           MOVE UT-IDARTNR TO W-IDARTNR                                         
           PERFORM IMS-GU-WDB601                                                
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
           PERFORM IMS-GU-WDD311                                                
           IF SEGMENT-FINNS                                                     
             MOVE TEXT-BEART TO TRAUTF8-TECONV-FROM                             
           ELSE                                                                 
             MOVE SPACE      TO TRAUTF8-TECONV-FROM                             
           END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GU-WDD311                                               
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
012181*    -- STRIP SPACE OR CONVERT TO UNICODE                                 
012182     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
012184*    -- MOVE CONVERTED DESCRIPTION TO OUTPUT FIELD                        
012185     MOVE TRAUTF8-TECONV-TO      TO UT2-BEART                             
012186                                                                          
012187     MOVE UT-IDARTNR             TO UT2-IDARTNR                           
012188     MOVE UT-IDFKNGRP            TO UT2-IDFKNGRP                          
012189     MOVE UT-IDDC                TO UT2-IDDC                              
012190     MOVE UT-IDUSER-SPKVAL       TO UT2-IDUSER-SPKVAL                     
012191     MOVE UT-KDLEVSP             TO UT2-KDLEVSP                           
012192     MOVE UT-KVLS                TO UT2-KVLS                              
012193     MOVE UT-KVROS               TO UT2-KVROS                             
012194     MOVE UT-KVSPARR-KVAL        TO UT2-KVSPARR-KVAL                      
012195     MOVE UT-TISPARR-KVAL        TO UT2-TISPARR-KVAL                      
012196*    DISPLAY 'UT2-AREA ' UT2-WL10102                                      
012200     PERFORM S90-PUT-DOC-LINE                                             
012207     .                                                                    
012208     EJECT                                                                
012209                                                                          
012210 Z-FINIT SECTION.                                                         
012301     CLOSE WL10PP                                                         
012302           W61410                                                         
012401                                                                          
012402     MOVE 'S' TO POSTSUM-OPKOD                                            
012410     CALL POSTSUM USING POSTSUM-PARM                                      
012500     .                                                                    
012601     EJECT                                                                
012602 IMS-GU-WDB601 SECTION.                                                   
                                                                                
           STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  ' TO GODK-STATUSKODER                                        
           CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
           MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           SKIP3                                                                
       IMS-GU-WDD311 SECTION.                                                   
                                                                                
           STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
                   DELIMITED BY SIZE INTO SSA1                                  
           STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
                    DELIMITED BY SIZE INTO SSA2                                 
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
           MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           EJECT                                                                
       IMS-STATUSKONTROLL SECTION.                                              
           SET STATUS-IX TO 1                                                   
           SEARCH GODK-STATUS                                                   
             AT END                                                             
               STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
               DELIMITED BY SIZE INTO FELTEXT                                   
               CALL FELLOG                                                      
             WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
               CONTINUE                                                         
           END-SEARCH                                                           
           .                                                                    
012603 S01-LAS-WL10PP SECTION.                                                  
012605     READ WL10PP INTO PARM-AREA                                           
012606     MOVE PARM-IDFKNGRP TO WS-PARM-IDFKNGRP                               
012610     .                                                                    
012611     EJECT                                                                
012612                                                                          
012618 S02-LAES-W61410  SECTION.                                                
012620     READ W61410 INTO IN-AREA                                             
012622     AT END                                                               
012623        SET END-OF-W61410 TO TRUE                                         
012624                                                                          
012625     NOT AT END                                                           
012626        MOVE 'W61410'   TO POSTSUM-FDNAMN                                 
012627        MOVE 'W61412D2' TO POSTSUM-DDNAMN2                                
012630        MOVE SPACE      TO POSTSUM-TRANSTYP                               
012631        CALL POSTSUM USING POSTSUM-PARM                                   
012632     END-READ                                                             
012640     .                                                                    
012701     EJECT                                                                
012702                                                                          
013000 S31-SORT-RELEASE  SECTION.                                               
013200     RELEASE SORT-POST FROM SORTWS-AREA                                   
013300     .                                                                    
013400                                                                          
013500 S32-SORT-RETURN  SECTION.                                                
013700     RETURN SORTFIL INTO SORTWS-AREA                                      
013800     AT END                                                               
013900         SET END-OF-SORTFIL TO TRUE                                       
014000     .                                                                    
014100     EJECT                                                                
014120                                                                          
014121 S90-SEND-OPEN SECTION.                                                   
014130     MOVE WS-ADRESS                       TO SEND-ADDISPABS               
014140     MOVE 'OPEN'                          TO SEND-KDFUNC                  
014150     CALL WZ01SEND USING SEND-CONTROL-AREA                                
014160                         SEND-OPEN-AREA                                   
014170     IF SEND-KDRC > ZERO                                                  
014180       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
014190       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
014191       DELIMITED BY SIZE INTO FELTEXT-STR                                 
014192       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
014193     END-IF                                                               
014194     .                                                                    
014197                                                                          
014198 S90-PUT-HEADER SECTION.                                                  
014199     MOVE 'PUT'                           TO SEND-KDFUNC                  
014200     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
014201     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
014202     CALL WZ01SEND USING SEND-CONTROL-AREA                                
014203                         SEND-KVDLEN                                      
014204                         HDR-AREA                                         
014205     IF SEND-KDRC > ZERO                                                  
014206       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
014207       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
014208       DELIMITED BY SIZE INTO FELTEXT-STR                                 
014209       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
014210     END-IF                                                               
014211     .                                                                    
014214                                                                          
014215 S90-PUT-DOC-HEAD SECTION.                                                
014216     MOVE 'PUT'                           TO SEND-KDFUNC                  
014217     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
014218     MOVE LENGTH OF DOC-HEAD-AREA         TO SEND-KVDLEN                  
014219     CALL WZ01SEND USING SEND-CONTROL-AREA                                
014220                         SEND-KVDLEN                                      
014221                         DOC-HEAD-AREA                                    
014222     IF SEND-KDRC > ZERO                                                  
014223       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
014224       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
014225       DELIMITED BY SIZE INTO FELTEXT-STR                                 
014226       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
014227     END-IF                                                               
014228     .                                                                    
014231                                                                          
014232 S90-PUT-DOC-LINE SECTION.                                                
014233     MOVE 'PUT'                           TO SEND-KDFUNC                  
014234     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
014235     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
014236     CALL WZ01SEND USING SEND-CONTROL-AREA                                
014237                         SEND-KVDLEN                                      
014238                         DOC-LINE-AREA                                    
014239     IF SEND-KDRC > ZERO                                                  
014240       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
014241       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
014242       DELIMITED BY SIZE INTO FELTEXT-STR                                 
014243       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
014244     END-IF                                                               
014245     .                                                                    
014248                                                                          
014249 S90-SEND-CLOSE SECTION.                                                  
014250     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
014251     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
014252     CALL WZ01SEND USING SEND-CONTROL-AREA                                
014253     .                                                                    
014254     EJECT                                                                
014300                                                                          
014400 S99-ABEND SECTION.                                                       
014402     MOVE 'S' TO POSTSUM-OPKOD                                            
014410     CALL POSTSUM USING POSTSUM-PARM                                      
014500     CALL ABEND USING RKOD-ABEND                                          
014600     .                                                                    
014610     EJECT                                                                
014700*    -COPY WY2000P1                                                       
