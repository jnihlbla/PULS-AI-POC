001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W4791400.                                                
001300*AUTHOR.         LARS CALAIS.                                             
001400*DATE-WRITTEN.   91/06/13.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        FÖR ORDER MED STATUS "E" LÄSES O-RAD, RO-RAD OCH OBEKR.          
001910*        OM NÅGON AV DESSA FINNS, SKRIVS POST TILL LARMLISTA FÖR          
001920*        ORDERKONTORET, ANNARS SKRIVS POST TILL RENSNINGSPGMET            
002200*                                                                         
002301*        PROGRAMMET LÄSER     WLORQM (WDQ1)                               
002303*        PROGRAMMET LÄSER     WLORQI (WDQ2)                               
002304*        PROGRAMMET LÄSER     WLORQA (WDQ3)                               
002305*        PROGRAMMET LÄSER     WLORQF (WDQ4)                               
002310*        PROGRAMMET LÄSER     WLORDP (WDA5)                               
002400*                                                                         
002500*    ABENDKODER:                                                          
002600*        U0016 -  . . . .                                                 
002700*        U1000 -  . . . .                                                 
002800*                                                                         
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP2                                                                
003300 INPUT-OUTPUT SECTION.                                                    
003400                                                                          
003500 FILE-CONTROL.                                                            
003601     SKIP2                                                                
003602*          --- ORDER I STATUS "E"                                         
003603     SELECT W47916                     ASSIGN TO W47914D1.                
003604     SKIP2                                                                
003605*          --- RENSNINGSPOSTER                                            
003606     SELECT W47914                     ASSIGN TO W47914D2.                
003607     SKIP2                                                                
003608*          --- FELLISTA                                                   
003610     SELECT W47915                     ASSIGN TO W47914D3.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004201     SKIP3                                                                
004202 FD  W47916                                                               
004203     RECORDING       F                                                    
004204     BLOCK CONTAINS  0.                                                   
004205     SKIP2                                                                
004206*01  -COPY W479016      -L.                                               
004207     SKIP3                                                                
004208 FD  W47914                                                               
004209     RECORDING       F                                                    
004210     BLOCK CONTAINS  0.                                                   
004211     SKIP2                                                                
004212*01  POST -COPY W479014 -PRE  RENS-  -L.                                  
004213     SKIP3                                                                
004214 FD  W47915                                                               
004215     RECORDING       F                                                    
004216     BLOCK CONTAINS  0.                                                   
004217     SKIP2                                                                
004220*01  POST -COPY W479015 -PRE  LIST-  -L.                                  
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004501*    -- CHECKED BY WY2000                                                 
004510     SKIP3                                                                
004600 77  IDPGM                       PIC X(8)    VALUE 'W4791400'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
005001                                                                          
005002 01  RAD-FINNS                   PIC X(3)    VALUE '001'.                 
005003 01  RORAD-FINNS                 PIC X(3)    VALUE '002'.                 
005004 01  OBKR-FINNS                  PIC X(3)    VALUE '003'.                 
005005 01  STATUS-U-FINNS              PIC X(3)    VALUE '004'.                 
005006                                                                          
005007 77  WDA5-FINNS-SW               PIC X.                                   
005010     88  WDA5-FINNS                          VALUE 'J'.                   
005011     88  WDA5-SAKNAS                         VALUE 'N'.                   
005020                                                                          
005030 77  W47916-EOF-SW               PIC X       VALUE 'N'.                   
005040     88  END-OF-W47916                       VALUE 'J'.                   
005100     EJECT                                                                
005200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005300 01  FILLER REDEFINES DAGENS-DATUM.                                       
005400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005700     EJECT                                                                
005800 01  DYNAMISKA-SUBPROGRAM.                                                
005900*                                                                         
006000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006310     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006400     SKIP2                                                                
006500*    --- PARAMETRAR TILL ABEND                                            
006600                                                                          
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
007502 01  IN-AREA-START               PIC X(24)   VALUE                        
007503                                 'IN-AREA-START  '.                       
007504     SKIP2                                                                
007505                                                                          
007506*01  AREA -COPY W479016     -PRE IN-                                      
007507     EJECT                                                                
007508 01  RENS-AREA-START             PIC X(24)   VALUE                        
007509                                 'RENS-AREA-START  '.                     
007510     SKIP2                                                                
007511                                                                          
007512*01  AREA -COPY W479014     -PRE RENS-                                    
007513     EJECT                                                                
007514 01  LIST-AREA-START             PIC X(24)   VALUE                        
007515                                 'LIST-AREA-START  '.                     
007516     SKIP2                                                                
007517                                                                          
007520*01  AREA -COPY W479015     -PRE LIST-                                    
007600     EJECT                                                                
007700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007800*                                                                         
007900     EJECT                                                                
008000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008100     SKIP3                                                                
008200 01  NYCKLAR-TILL-DLI.                                                    
008300     03  W-WDQ101KY-FOM.                                                  
008301         05  W-IDORDER-Q1-F      PIC S9(7)    COMP-3.                     
008302         05  FILLER              PIC X(13)    VALUE LOW-VALUE.            
008303     03  W-WDQ101KY-TOM.                                                  
008304         05  W-IDORDER-Q1-T      PIC S9(7)    COMP-3.                     
008305         05  FILLER              PIC X(13)    VALUE HIGH-VALUE.           
008306     03  W-IDORDER-X.                                                     
008307         05  W-IDORDER           PIC S9(7)    COMP-3.                     
008308     03  W-WDQ301KY-FOM.                                                  
008309         05  W-IDORDER-Q3-F      PIC S9(7)    COMP-3.                     
008310         05  FILLER              PIC X(8)     VALUE LOW-VALUE.            
008311     03  W-WDQ301KY-TOM.                                                  
008312         05  W-IDORDER-Q3-T      PIC S9(7)    COMP-3.                     
008313         05  FILLER              PIC X(8)     VALUE HIGH-VALUE.           
008314     03  W-STATUS-U.                                                      
008315         05  W-KDODELSTA         PIC X(1)     VALUE 'U'.                  
008316     03  W-WDA501KY-FOM.                                                  
008317         05  W-IDDISTR-A5-F      PIC S9(5)    COMP-3.                     
008318         05  W-IDKUNDNR-A5-F     PIC S9(7)    COMP-3.                     
008319         05  W-IDKUNDRF-A5-F     PIC X(10).                               
008320         05  FILLER              PIC X(7)     VALUE LOW-VALUE.            
008321     03  W-WDA501KY-TOM.                                                  
008322         05  W-IDDISTR-A5-T      PIC S9(5)    COMP-3.                     
008323         05  W-IDKUNDNR-A5-T     PIC S9(7)    COMP-3.                     
008324         05  W-IDKUNDRF-A5-T     PIC X(10).                               
008325         05  FILLER              PIC X(7)     VALUE HIGH-VALUE.           
008326     03  W-WDQ401KY-FOM.                                                  
008327         05  W-IDORDER-Q4-F      PIC S9(7)    COMP-3.                     
008328         05  FILLER              PIC X(16)    VALUE LOW-VALUE.            
008329     03  W-WDQ401KY-TOM.                                                  
008330         05  W-IDORDER-Q4-T      PIC S9(7)    COMP-3.                     
008340         05  FILLER              PIC X(16)    VALUE HIGH-VALUE.           
008400     SKIP2                                                                
008500*    --- STATUS-KOD FRÅN IMS                                              
008600 01  STATUS-WS                   PIC XX.                                  
008700     88  SEGMENT-FINNS                       VALUE '  '.                  
008900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009000     SKIP2                                                                
009100 01  GODK-STATUSKODER.                                                    
009200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009300     SKIP3                                                                
009400 01  SSA1                        PIC X(96).                               
009500 01  SSA2                        PIC X(64).                               
009600     EJECT                                                                
009700*    --- IMS FUNKTIONSKODER                                               
009800*01  -COPY W0003                                                          
009900     EJECT                                                                
010100*    ---  DLI INPUT-OUTPUT AREA                                           
010200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010300     SKIP3                                                                
010400 01  DLI-IO-AREA.                                                         
010500     03  IO-AREA                 PIC X(700)  VALUE SPACE.                 
010601     SKIP3                                                                
010602     03  WLORDP01 REDEFINES IO-AREA.                                      
010603*        05  -COPY WDA501                                                 
010604     EJECT                                                                
010605     03  WLORQM01 REDEFINES IO-AREA.                                      
010606*        05  -COPY WDQ101                                                 
010607     EJECT                                                                
010901     03  WLORQI01 REDEFINES IO-AREA.                                      
010902*        05  -COPY WDQ201                                                 
010903     EJECT                                                                
010910     03  WLORQF01 REDEFINES IO-AREA.                                      
010920*        05  -COPY WDQ401                                                 
010930     EJECT                                                                
011000 LINKAGE SECTION.                                                         
011100                                                                          
011201     EJECT                                                                
011202*01  -COPY W0008  -PRE ORQA-                                              
011203     05  FILLER                  PIC X.                                   
011204     EJECT                                                                
011205*01  -COPY W0008  -PRE ORQM-                                              
011206     05  FILLER                  PIC X.                                   
011207     EJECT                                                                
011208*01  -COPY W0008  -PRE ORQI-                                              
011209     05  FILLER                  PIC X.                                   
011210     EJECT                                                                
011211*01  -COPY W0008  -PRE ORQF-                                              
011212     05  FILLER                  PIC X.                                   
011213     EJECT                                                                
011214*01  -COPY W0008  -PRE ORDP-                                              
011220     05  FILLER                  PIC X.                                   
011300     EJECT                                                                
011401 PROCEDURE DIVISION  USING ORQA-PCB                                       
011402                           ORQM-PCB                                       
011403                           ORQI-PCB                                       
011404                           ORQF-PCB                                       
011405                           ORDP-PCB.                                      
011406                                                                          
011407     ENTRY 'DLITCBL' USING ORQA-PCB                                       
011408                           ORQM-PCB                                       
011409                           ORQI-PCB                                       
011410                           ORQF-PCB                                       
011420                           ORDP-PCB.                                      
011500                                                                          
011800     PERFORM A-INIT                                                       
011910     PERFORM S01-LAES-W47916                                              
012000     PERFORM UNTIL END-OF-W47916                                          
012100       MOVE IN-IDORDER             TO W-IDORDER-Q1-F                      
012110                                      W-IDORDER-Q1-T                      
012120                                      W-IDORDER-Q3-F                      
012130                                      W-IDORDER-Q3-T                      
012131                                      W-IDORDER-Q4-F                      
012132                                      W-IDORDER-Q4-T                      
012140                                      W-IDORDER                           
012200       PERFORM IMS-GET-ORQI01                                             
012201       IF SEGMENT-FINNS                                                   
012202          MOVE OHUV-IDORDER           TO LIST-IDORDER                     
012203          MOVE OHUV-TIREGDAT          TO LIST-TIREGDAT                    
012204          MOVE OHUV-KDORDKL           TO LIST-KDORDKL                     
012205          MOVE OHUV-IDSYSTEM          TO LIST-IDSYSTEM                    
012206          MOVE OHUV-IDDISTR           TO LIST-IDDISTR                     
012207          MOVE OHUV-IDKUNDNR          TO LIST-IDKUNDNR                    
012208          MOVE OHUV-IDKUNDRF          TO LIST-IDKUNDRF                    
012209          MOVE OHUV-IDUSER            TO LIST-IDUSER                      
012210          MOVE OHUV-IDDC-PRIM         TO LIST-IDDC                        
012211          PERFORM IMS-GET-ORQF01                                          
012212          IF SEGMENT-FINNS                                                
012213              MOVE RAD-FINNS          TO LIST-IDPTYP                      
012219              PERFORM S12-SKRIV-W47915                                    
012220          ELSE                                                            
012221             PERFORM IMS-GET-ORQM01                                       
012222             IF SEGMENT-FINNS                                             
012223                MOVE OBKR-FINNS       TO LIST-IDPTYP                      
012228                PERFORM S12-SKRIV-W47915                                  
012229             ELSE                                                         
012230                PERFORM B-KOLLA-RO                                        
012231                IF WDA5-SAKNAS                                            
012232                   PERFORM IMS-GET-ORQA-U                                 
012233                   IF SEGMENT-SAKNAS                                      
012234                      MOVE '014'       TO RENS-IDPTYP                     
012235                      MOVE IN-IDORDER  TO RENS-IDORDER                    
012236                      MOVE IN-IDDISTR  TO RENS-IDDISTR                    
012237                      MOVE IN-IDKUNDNR TO RENS-IDKUNDNR                   
012238                      PERFORM S11-SKRIV-W47914                            
012239                   ELSE                                                   
012240                      MOVE STATUS-U-FINNS TO LIST-IDPTYP                  
012241                      PERFORM S12-SKRIV-W47915                            
012242                   END-IF                                                 
012243                ELSE                                                      
012244                    MOVE RORAD-FINNS  TO LIST-IDPTYP                      
012245                    PERFORM S12-SKRIV-W47915                              
012246                END-IF                                                    
012247             END-IF                                                       
012248          END-IF                                                          
012249       END-IF                                                             
012250       PERFORM S01-LAES-W47916                                            
012260     END-PERFORM                                                          
013100     PERFORM Z-FINIT                                                      
013200                                                                          
013300     MOVE ZERO TO RETURN-CODE                                             
013400     GOBACK                                                               
013500     .                                                                    
013600     EJECT                                                                
013700 A-INIT             SECTION.                                              
013801                                                                          
013810     OPEN INPUT  W47916                                                   
013901                                                                          
013902     OPEN OUTPUT W47914                                                   
013910                 W47915                                                   
014000     SKIP2                                                                
014100*    ACCEPT DAGENS-DATUM  FROM DATE                                       
014210     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014400     .                                                                    
014500     EJECT                                                                
014510 B-KOLLA-RO         SECTION.                                              
014511                                                                          
014512     MOVE NEJ                      TO WDA5-FINNS-SW                       
014519     MOVE OHUV-IDDISTR             TO W-IDDISTR-A5-F                      
014520                                      W-IDDISTR-A5-T                      
014521     MOVE OHUV-IDKUNDNR            TO W-IDKUNDNR-A5-F                     
014522                                      W-IDKUNDNR-A5-T                     
014523     MOVE OHUV-IDKUNDRF (3:5)      TO W-IDKUNDRF-A5-F                     
014524                                      W-IDKUNDRF-A5-T                     
014525     PERFORM IMS-GET-ORDP01                                               
014526     IF SEGMENT-FINNS                                                     
014527        MOVE JA                    TO WDA5-FINNS-SW                       
014528     END-IF                                                               
014532     .                                                                    
014540     EJECT                                                                
014600 Z-FINIT            SECTION.                                              
014701     CLOSE W47916                                                         
014702           W47914                                                         
014710           W47915                                                         
014801     SKIP2                                                                
014802     MOVE 'S' TO POSTSUM-OPKOD                                            
014810     CALL POSTSUM USING POSTSUM-PARM                                      
014900     .                                                                    
015001     EJECT                                                                
015002 S01-LAES-W47916    SECTION.                                              
015003     SKIP2                                                                
015004     READ W47916 INTO IN-AREA                                             
015005     AT END                                                               
015006        MOVE HIGH-VALUE TO IN-AREA                                        
015007        SET END-OF-W47916 TO TRUE                                         
015008                                                                          
015009     NOT AT END                                                           
015010        MOVE 'W47916' TO POSTSUM-FDNAMN                                   
015011        MOVE 'W47914D1' TO POSTSUM-DDNAMN2                                
015012        MOVE '016'     TO POSTSUM-TRANSTYP                                
015013        CALL POSTSUM USING POSTSUM-PARM                                   
015014     END-READ                                                             
015020     .                                                                    
015101     EJECT                                                                
015102 S11-SKRIV-W47914   SECTION.                                              
015103     SKIP2                                                                
015104     WRITE RENS-POST FROM RENS-AREA                                       
015105                                                                          
015106     MOVE RENS-IDPTYP TO POSTSUM-TRANSTYP                                 
015107     MOVE 'W47914' TO POSTSUM-FDNAMN                                      
015108     MOVE 'W47914D2' TO POSTSUM-DDNAMN2                                   
015109     CALL POSTSUM USING POSTSUM-PARM                                      
015110     .                                                                    
015111     EJECT                                                                
015112 S12-SKRIV-W47915   SECTION.                                              
015113     SKIP2                                                                
015114     WRITE LIST-POST FROM LIST-AREA                                       
015115                                                                          
015116     MOVE LIST-IDPTYP TO POSTSUM-TRANSTYP                                 
015117     MOVE 'W47915' TO POSTSUM-FDNAMN                                      
015118     MOVE 'W47914D3' TO POSTSUM-DDNAMN2                                   
015119     CALL POSTSUM USING POSTSUM-PARM                                      
015120     .                                                                    
015300     EJECT                                                                
015900* --- IMS SEKTIONER ---                                                   
016000     SKIP3                                                                
016101     EJECT                                                                
016102 IMS-GET-ORQA-U     SECTION.                                              
016103     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-FOM                          
016104                    '&WDQ301KY<=' W-WDQ301KY-TOM                          
016105                    '&KDODELST =' W-STATUS-U     ')'                      
016106          DELIMITED BY SIZE INTO SSA1                                     
016107     MOVE '  GE' TO GODK-STATUSKODER                                      
016108     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA SSA1                      
016109     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
016110     PERFORM IMS-STATUSKONTROLL                                           
016111     .                                                                    
016112     EJECT                                                                
016113 IMS-GET-ORQF01     SECTION.                                              
016114     STRING 'WLORQF01(WDQ401KY>=' W-WDQ401KY-FOM                          
016115                    '&WDQ401KY<=' W-WDQ401KY-TOM ')'                      
016116          DELIMITED BY SIZE INTO SSA1                                     
016117     MOVE '  GE' TO GODK-STATUSKODER                                      
016118     CALL CBLTDLI USING GU ORQF-PCB DLI-IO-AREA SSA1                      
016119     MOVE ORQF-STATUS-CODE TO STATUS-WS                                   
016120     PERFORM IMS-STATUSKONTROLL                                           
016121     .                                                                    
016122     EJECT                                                                
016123 IMS-GET-ORQI01     SECTION.                                              
016124     STRING 'WLORQI01(IDORDER  =' W-IDORDER-X ')'                         
016125          DELIMITED BY SIZE INTO SSA1                                     
016126     MOVE '  GE' TO GODK-STATUSKODER                                      
016127     CALL CBLTDLI USING GU  ORQI-PCB DLI-IO-AREA SSA1                     
016128     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
016129     PERFORM IMS-STATUSKONTROLL                                           
016130     .                                                                    
016140     EJECT                                                                
016141 IMS-GET-ORQM01     SECTION.                                              
016142     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101KY-FOM                          
016143                    '&WDQ101KY<=' W-WDQ101KY-TOM ')'                      
016144          DELIMITED BY SIZE INTO SSA1                                     
016145     MOVE '  GE' TO GODK-STATUSKODER                                      
016146     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-AREA SSA1                      
016147     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
016148     PERFORM IMS-STATUSKONTROLL                                           
016149     .                                                                    
016150     EJECT                                                                
016151 IMS-GET-ORDP01     SECTION.                                              
016152     STRING 'WLORDP01(WDA501KY>=' W-WDA501KY-FOM                          
016153                    '&WDA501KY<=' W-WDA501KY-TOM ')'                      
016154          DELIMITED BY SIZE INTO SSA1                                     
016155     MOVE '  GE' TO GODK-STATUSKODER                                      
016156     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-AREA SSA1                      
016157     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
016158     PERFORM IMS-STATUSKONTROLL                                           
016160     .                                                                    
016200     EJECT                                                                
016300 IMS-STATUSKONTROLL SECTION.                                              
016400     SKIP2                                                                
016500     SET STATUS-IX TO 1                                                   
016600     SEARCH GODK-STATUS                                                   
016700       AT END                                                             
016800         MOVE 'FELAKTIG STATUSKOD FRÅN IMS' TO FELTEXT-STR                
016900         DISPLAY FELTEXT                                                  
017000         CALL FELLOG                                                      
017100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
017200     END-SEARCH                                                           
017300     .                                                                    
