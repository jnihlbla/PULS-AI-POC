000001 ID DIVISION.                                                             
000002     SKIP2                                                                
000003 PROGRAM-ID.     W1117800.                                                
000004*AUTHOR.         BODIL LINDAHL.                                           
000005*DATE-WRITTEN.   OKT 2002.                                                
000006*DATE-COMPILED.                                                           
000007*                                                                         
000008*    FUNKTION:                                                            
000009*        PROGRAMMET LÄSER FIL W11170 MED ERSÄTTNINGS-TRANSAR              
000010*        -  SKAPAR FIL W11178 TILL VCI.                                   
000011*        -  SKAPAR FIL W11179 UNDERLAG MEMOFIL TILL VCI.                  
000020*                                                                         
000030*                                                                         
000040                                                                          
000050     SKIP3                                                                
000060 ENVIRONMENT DIVISION.                                                    
000070     SKIP2                                                                
000080 INPUT-OUTPUT SECTION.                                                    
000090                                                                          
000100 FILE-CONTROL.                                                            
000200     SKIP2                                                                
000300*          --- ERSÄTTNINGS-TRANSAR FRÅN W111P070                          
000400     SELECT W11170                     ASSIGN TO W11178D1.                
000500*                                                                         
000600*          --- ERSÄTTNINGS-TRANSAR TILL VCI                               
000700     SELECT W11178                     ASSIGN TO W11178D2.                
000800*                                                                         
000810*          --- ERSÄTTNINGS-TRANSAR TILL VCI VIA MEMO                      
000820     SELECT W11179                     ASSIGN TO W11178D3.                
000830     EJECT                                                                
000900 DATA DIVISION.                                                           
001000     SKIP3                                                                
001100 FILE SECTION.                                                            
001200     SKIP3                                                                
001300 FD  W11170                                                               
001400     RECORDING       V                                                    
001500     BLOCK CONTAINS  0.                                                   
001600     SKIP2                                                                
001700*01  -COPY W111701A      -L.                                              
001800     SKIP2                                                                
001900*01  -COPY W111702A      -L.                                              
002000     SKIP3                                                                
002100 FD  W11178                                                               
002200     RECORDING       F                                                    
002300     BLOCK CONTAINS  0.                                                   
002400     SKIP2                                                                
002500*01  POST -COPY W11178   -PRE  UT-    -L.                                 
003110     EJECT                                                                
003111 FD  W11179                                                               
003112     RECORDING       F                                                    
003113     BLOCK CONTAINS  0.                                                   
003114     SKIP2                                                                
003115*01  POST -COPY W11179   -PRE  MEMO-  -L.                                 
003116     EJECT                                                                
003120 WORKING-STORAGE SECTION.                                                 
003130     SKIP2                                                                
003140                                                                          
003141*    -- CHECKED BY WY2000                                                 
003142 77  IDPGM                       PIC X(8)   VALUE 'W1117800'.             
003143 77  JA                          PIC X      VALUE 'J'.                    
003144 77  NEJ                         PIC X      VALUE 'N'.                    
003148 77  WS-IDARTNR-ERS              PIC S9(9)  VALUE  ZERO   COMP-3.         
003149 77  WS-REKSIFFR-ERS             PIC S9     VALUE  ZERO   COMP-3.         
003150 77  WS-DIERS-ERS                PIC S9(4)V9(3) VALUE ZERO.               
003161 77  WS-TIERSDAT                 PIC 9(5)   VALUE ZERO.                   
003171 77  SKRIV-SW                    PIC X      VALUE 'N'.                    
003172 77  SKRIV-MEMO-SW               PIC X      VALUE 'N'.                    
003180                                                                          
003181 77  WS-KDERS-NEW                PIC 9(3).                                
003182     88 ENTYDIG-ERSATTNING          VALUE 21 22 23.                       
003183     88 EJ-ENTYDIG-ERSATTNING       VALUE 24 25 26.                       
003184                                                                          
003185 77  W11170-EOF-SW               PIC X      VALUE 'N'.                    
003186     88  END-OF-W11170                      VALUE 'J'.                    
003187                                                                          
003190 77  BEHANDLA-702-SW             PIC X      VALUE 'N'.                    
003191     88  BEHANDLA-702                       VALUE 'J'.                    
003192                                                                          
003193 77  BEHANDLA-702-MEMO-SW        PIC X      VALUE 'N'.                    
003194     88  BEHANDLA-702-MEMO                  VALUE 'J'.                    
003195                                                                          
003196 01  KORNINGSDATUM           PIC 9(6).                                    
003197 01  FILLER REDEFINES KORNINGSDATUM.                                      
003198     03  AA                  PIC 99.                                      
003199     03  MM                  PIC 99.                                      
003200     03  DD                  PIC 99.                                      
003300     EJECT                                                                
003600 01  DYNAMISKA-SUBPROGRAM.                                                
003700*                                                                         
003800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
003900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
004000     EJECT                                                                
004100*    --- PARAMETRAR TILL POSTSUM                                          
004200*                                                                         
004300*01  -COPY W0005   -PRE  POSTSUM-                                         
004400     EJECT                                                                
004500*    --- PARAMETRAR TILL DATUMKORT                                        
004600*                                                                         
004700 01  PROGRAM-NAMN               PIC X(6)     VALUE 'W11178'.              
004800 01  DATUMKORT-ID               PIC X(6)     VALUE 'WDATUM'.              
004900                                                                          
005000*01  -COPY WDATKORT                                                       
005100     EJECT                                                                
005200 01  IN-AREA-START               PIC X(24)   VALUE                        
005300                                 'IN-AREA-START  '.                       
005400 01  IN-AREA.                                                             
005500     03  IN-IDPTYP               PIC X(3).                                
005600     03  FILLER                  PIC X(150).                              
005700*01  FILLER -COPY W111701A      -PRE IN701-   -RED  IN-AREA               
005800     EJECT                                                                
005900*01  FILLER -COPY W111702A      -PRE IN702-   -RED  IN-AREA               
006000     EJECT                                                                
006100 01  UT-AREA-START               PIC X(24)   VALUE                        
006200                                 'UT-AREA-START '.                        
006400*01  AREA   -COPY W11178        -PRE UT-                                  
006500     EJECT                                                                
006600 01  MEMO-AREA-START             PIC X(24)   VALUE                        
006700                                 'MEMO-AREA-START '.                      
006720*01  AREA   -COPY W11179        -PRE MEMO-                                
006730     EJECT                                                                
006740 01  SPAR-MEMO-AREA-START        PIC X(24)   VALUE                        
006741                                 'SPAR-MEMO-AREA-START '.                 
006742*01  AREA   -COPY W11179        -PRE SPAR-MEMO-                           
006743     EJECT                                                                
006744 PROCEDURE DIVISION.                                                      
006745                                                                          
006746     PERFORM A-INIT                                                       
006747                                                                          
006748     PERFORM S01-LAES-W11170                                              
006749     PERFORM UNTIL END-OF-W11170                                          
006750                                                                          
006751        EVALUATE TRUE                                                     
006752           WHEN IN-IDPTYP = '701'                                         
006753              IF SKRIV-SW = JA                                            
006754                 PERFORM S11-SKRIV-UTPOST                                 
006755                 MOVE NEJ TO SKRIV-SW                                     
006756                             SKRIV-MEMO-SW                                
006757              END-IF                                                      
006758              IF SKRIV-MEMO-SW = JA                                       
006759                 PERFORM S12-SKRIV-MEMOFIL-FRAN-SPAR                      
006760                 MOVE NEJ TO SKRIV-MEMO-SW                                
006761              END-IF                                                      
006762              PERFORM B-KOLLA-SKAPA-RIVNING                               
006763              IF IN701-KDERS-NEW > 20                                     
006764                 MOVE IN701-IDARTNR-ERS  TO WS-IDARTNR-ERS                
006765                 MOVE IN701-REKSIFFR-ERS TO WS-REKSIFFR-ERS               
006766                 MOVE IN701-KDERS-NEW    TO WS-KDERS-NEW                  
006768                 MOVE IN701-DIERS-ERS    TO WS-DIERS-ERS                  
006769                 MOVE IN701-TIERSDAT     TO WS-TIERSDAT                   
006770                 IF ENTYDIG-ERSATTNING                                    
006771                    MOVE JA  TO BEHANDLA-702-SW                           
006772                 ELSE                                                     
006773                    MOVE NEJ TO BEHANDLA-702-SW                           
006775                 END-IF                                                   
006776                 IF IN701-KDERS-NEW = 29 OR 52                            
006777                    PERFORM D-SKAPA-UTAN-TILLK-POST                       
006778                    MOVE NEJ TO BEHANDLA-702-SW                           
006779                 END-IF                                                   
006780              END-IF                                                      
006781           WHEN IN-IDPTYP = '702'                                         
006782              IF BEHANDLA-702                                             
006783                 IF IN702-IDARTNR-ERS = WS-IDARTNR-ERS                    
006784                    PERFORM C-KOLLA-SKAPA-MED-TILLK-POST                  
006785                 END-IF                                                   
006786              END-IF                                                      
006787        END-EVALUATE                                                      
006788                                                                          
006789        EVALUATE TRUE                                                     
006800           WHEN IN-IDPTYP = '701'                                         
006812              WHEN IN-IDPTYP = '701'                                      
006819                 IF IN701-KDERS-NEW > 20                                  
006820                    IF IN701-KDERS-NEW = 29 OR 52                         
006821                       MOVE NEJ TO BEHANDLA-702-MEMO-SW                   
006822                    ELSE                                                  
006823                       MOVE JA  TO BEHANDLA-702-MEMO-SW                   
006824                    END-IF                                                
006825                 ELSE                                                     
006826                    MOVE NEJ TO BEHANDLA-702-MEMO-SW                      
006827                 END-IF                                                   
006833              WHEN IN-IDPTYP = '702'                                      
006834                 IF BEHANDLA-702-MEMO                                     
006835                    IF IN702-IDARTNR-ERS = WS-IDARTNR-ERS                 
006836                       PERFORM E-SKAPA-MEMOFIL                            
006837                    END-IF                                                
006838                 END-IF                                                   
006839        END-EVALUATE                                                      
006840                                                                          
006841        PERFORM S01-LAES-W11170                                           
006842     END-PERFORM                                                          
006843                                                                          
006844     IF SKRIV-SW = JA                                                     
006845        PERFORM S11-SKRIV-UTPOST                                          
006846        MOVE NEJ TO SKRIV-SW                                              
006847                    SKRIV-MEMO-SW                                         
006848     END-IF                                                               
006849                                                                          
006850     IF SKRIV-MEMO-SW = JA                                                
006851        PERFORM S12-SKRIV-MEMOFIL-FRAN-SPAR                               
006852     END-IF                                                               
006853                                                                          
006854     PERFORM Z-FINIT                                                      
006855     MOVE ZERO TO RETURN-CODE                                             
006856     GOBACK                                                               
006860     .                                                                    
006900     EJECT                                                                
007000 A-INIT SECTION.                                                          
007100                                                                          
007200     OPEN INPUT  W11170                                                   
007300     OPEN OUTPUT W11178                                                   
007310                 W11179                                                   
007311                                                                          
007320     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
007340     MOVE D-AAR         TO AA                                             
007350     MOVE D-MAANAD      TO MM                                             
007360     MOVE D-DAG         TO DD                                             
007400                                                                          
007410     MOVE NEJ TO BEHANDLA-702-SW                                          
007411                 BEHANDLA-702-MEMO-SW                                     
007420                 SKRIV-SW                                                 
007430                 SKRIV-MEMO-SW                                            
007440     PERFORM S02-NOLLSTALL                                                
007900     .                                                                    
008000     EJECT                                                                
008100 B-KOLLA-SKAPA-RIVNING SECTION.                                           
008300*****************************************************************         
008400*  UT-POST SKAPAS                                               *         
008500*  - NÄR GAMMAL EK > 20 OCH NY EK = 00                          *         
008800*  - NÄR GAMMAL EK > 20 OCH NY EK > 20                          *         
008900*      ERSÄTTNINGEN RIVS DÅ OCH LÄGGS UPP PÅ NYTT               *         
009000*    - OM OLIKA EK = BYTE AV EK                                 *         
009100*    - OM SAMMA EK = UPPDATERING AV TILLKOMMANDE ARTIKLAR       *         
009200*****************************************************************         
009300                                                                          
009416     IF (IN701-KDERS-OLD > 20 AND IN701-KDERS-NEW > 20)                   
009417     OR (IN701-KDERS-OLD > 20 AND IN701-KDERS-NEW = ZERO)                 
009426        MOVE IN701-IDARTNR-ERS     TO UT-IDARTNR-ERS                      
009427        MOVE IN701-REKSIFFR-ERS    TO UT-REKSIFFR-ERS                     
009430        MOVE ZERO                  TO UT-KDERS-NEW                        
009440                                      UT-IDARTNR-TILLK                    
009450                                      UT-REKSIFFR-TILLK                   
009451                                      UT-TIERSDAT                         
009460        PERFORM S11-SKRIV-UTPOST                                          
009461        MOVE NEJ TO SKRIV-SW                                              
009462                    SKRIV-MEMO-SW                                         
009470     END-IF                                                               
009480     .                                                                    
009490     EJECT                                                                
009518 C-KOLLA-SKAPA-MED-TILLK-POST SECTION.                                    
009520*****************************************************************         
009530*  UT-POST SKAPAS NÄR                                           *         
009541*  - NÄR ERSÄTTNINGEN GÅR UPPÅT TILL > 20                       *         
009542*  - NÄR GAMMAL EK > 20 OCH NY EK > 20                          *         
009543*  GÄLLER VID 1:1 ENTYDIGA ERSÄTTNINGAR OCH EK 29 OCH 52        *         
009547*****************************************************************         
009548                                                                          
009550     IF IN702-IDKORTNR = 1                                                
009590        MOVE IN702-IDARTNR-ERS     TO UT-IDARTNR-ERS                      
009591        MOVE WS-REKSIFFR-ERS       TO UT-REKSIFFR-ERS                     
009593        MOVE IN702-IDARTNR-TILLK   TO UT-IDARTNR-TILLK                    
009594        MOVE IN702-REKSIFFR-TILLK  TO UT-REKSIFFR-TILLK                   
009596        MOVE WS-KDERS-NEW          TO UT-KDERS-NEW                        
009598        MOVE WS-TIERSDAT           TO UT-TIERSDAT                         
009605        MOVE JA TO SKRIV-SW                                               
009606     ELSE                                                                 
009607        MOVE NEJ TO SKRIV-SW                                              
009608     END-IF                                                               
009612     .                                                                    
009613     EJECT                                                                
009614                                                                          
009615 D-SKAPA-UTAN-TILLK-POST SECTION.                                         
009616                                                                          
009660     MOVE IN701-IDARTNR-ERS     TO UT-IDARTNR-ERS                         
009670     MOVE IN701-REKSIFFR-ERS    TO UT-REKSIFFR-ERS                        
009671     MOVE ZERO                  TO UT-IDARTNR-TILLK                       
009672                                   UT-REKSIFFR-TILLK                      
009690     MOVE IN701-KDERS-NEW       TO UT-KDERS-NEW                           
009691     MOVE WS-TIERSDAT           TO UT-TIERSDAT                            
009692                                                                          
009700     PERFORM S11-SKRIV-UTPOST                                             
009710     MOVE NEJ TO SKRIV-SW                                                 
009711                 SKRIV-MEMO-SW                                            
009720     .                                                                    
009730     EJECT                                                                
009743 E-SKAPA-MEMOFIL SECTION.                                                 
009745*****************************************************************         
009746*  MEMO-POST SKAPAS FÖR SAMTLIGA                                *         
009747*  - EJ ENTYDIGA 1:1 ERSÄTTNINGAR                               *         
009748*  - EJ 29 52 ERSÄTTNINGAR                                      *         
009749*  - EJ RIVNING TILL 00                                         *         
009750*****************************************************************         
009751                                                                          
009752     IF IN702-IDKORTNR = 1                                                
009756        MOVE IN702-IDARTNR-ERS        TO SPAR-MEMO-IDARTNR-ERS            
009757        MOVE WS-REKSIFFR-ERS          TO SPAR-MEMO-REKSIFFR-ERS           
009758        MOVE WS-DIERS-ERS             TO SPAR-MEMO-DIERS-ERS              
009759        MOVE WS-KDERS-NEW             TO SPAR-MEMO-KDERS-NEW              
009761        MOVE WS-TIERSDAT              TO SPAR-MEMO-TIERSDAT               
009763        MOVE IN702-IDKORTNR           TO SPAR-MEMO-IDKORTNR               
009764        MOVE IN702-FLTEXT             TO SPAR-MEMO-FLTEXT                 
009765        IF IN702-FLTEXT = 'J'                                             
009766           MOVE IN702-BEERS           TO SPAR-MEMO-BEERS                  
009767        ELSE                                                              
009768           MOVE IN702-IDARTNR-TILLK   TO SPAR-MEMO-IDARTNR-TILLK          
009769           MOVE IN702-REKSIFFR-TILLK  TO SPAR-MEMO-REKSIFFR-TILLK         
009770           MOVE IN702-DIERS-TILLK     TO SPAR-MEMO-DIERS-TILLK            
009771           MOVE SPACE                 TO SPAR-MEMO-FILLERX3               
009772        END-IF                                                            
009773        MOVE JA TO SKRIV-MEMO-SW                                          
009774     ELSE                                                                 
009775        IF IN702-IDKORTNR = 2                                             
009776           PERFORM S12-SKRIV-MEMOFIL-FRAN-SPAR                            
009777           MOVE NEJ TO SKRIV-MEMO-SW                                      
009778        END-IF                                                            
009779        MOVE IN702-IDARTNR-ERS        TO MEMO-IDARTNR-ERS                 
009780        MOVE WS-REKSIFFR-ERS          TO MEMO-REKSIFFR-ERS                
009781        MOVE WS-DIERS-ERS             TO MEMO-DIERS-ERS                   
009782        MOVE WS-KDERS-NEW             TO MEMO-KDERS-NEW                   
009784        MOVE WS-TIERSDAT              TO MEMO-TIERSDAT                    
009785        MOVE IN702-IDKORTNR           TO MEMO-IDKORTNR                    
009786        MOVE IN702-FLTEXT             TO MEMO-FLTEXT                      
009787        IF IN702-FLTEXT = 'J'                                             
009788           MOVE IN702-BEERS           TO MEMO-BEERS                       
009789        ELSE                                                              
009790           MOVE IN702-IDARTNR-TILLK   TO MEMO-IDARTNR-TILLK               
009791           MOVE IN702-REKSIFFR-TILLK  TO MEMO-REKSIFFR-TILLK              
009792           MOVE IN702-DIERS-TILLK     TO MEMO-DIERS-TILLK                 
009793           MOVE SPACE                 TO MEMO-FILLERX3                    
009794        END-IF                                                            
009795        PERFORM S13-SKRIV-MEMOFIL                                         
009796     END-IF                                                               
009797     .                                                                    
009798     EJECT                                                                
009799 Z-FINIT SECTION.                                                         
009800                                                                          
009801     CLOSE W11170                                                         
009802           W11178                                                         
009803           W11179                                                         
009804                                                                          
009805     MOVE 'S' TO POSTSUM-OPKOD                                            
009806     CALL POSTSUM USING POSTSUM-PARM                                      
009807     .                                                                    
009808     EJECT                                                                
009809 S01-LAES-W11170  SECTION.                                                
009810                                                                          
009811     READ W11170 INTO IN-AREA                                             
009812     AT END                                                               
009813        SET END-OF-W11170 TO TRUE                                         
009814     NOT AT END                                                           
009815        MOVE 'W11178'   TO POSTSUM-FDNAMN                                 
009816        MOVE 'W11178D1' TO POSTSUM-DDNAMN2                                
009817        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
009818        CALL POSTSUM  USING POSTSUM-PARM                                  
009819     END-READ                                                             
009820     .                                                                    
009900     EJECT                                                                
009901 S02-NOLLSTALL  SECTION.                                                  
009902                                                                          
009903     MOVE ZERO TO UT-IDARTNR-ERS                                          
009904                  UT-REKSIFFR-ERS                                         
009905                  UT-IDARTNR-TILLK                                        
009906                  UT-REKSIFFR-TILLK                                       
009907                  UT-KDERS-NEW                                            
009909                  UT-TIERSDAT                                             
009910                  MEMO-IDARTNR-ERS                                        
009911                  MEMO-REKSIFFR-ERS                                       
009912                  MEMO-DIERS-ERS                                          
009913                  MEMO-IDKORTNR                                           
009915                  MEMO-KDERS-NEW                                          
009916                  MEMO-TIERSDAT                                           
009917     MOVE SPACE TO MEMO-FLTEXT                                            
009918     .                                                                    
009919     EJECT                                                                
009920 S11-SKRIV-UTPOST SECTION.                                                
009921                                                                          
009922     WRITE UT-POST FROM UT-AREA                                           
009923     MOVE 'W11178'   TO POSTSUM-FDNAMN                                    
009924     MOVE 'W11178D2' TO POSTSUM-DDNAMN2                                   
009925     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
009926     CALL POSTSUM USING POSTSUM-PARM                                      
009927     .                                                                    
009928     EJECT                                                                
009929 S12-SKRIV-MEMOFIL-FRAN-SPAR SECTION.                                     
009930                                                                          
009931     WRITE MEMO-POST FROM SPAR-MEMO-AREA                                  
009940     MOVE 'W11179'   TO POSTSUM-FDNAMN                                    
009950     MOVE 'W11178D3' TO POSTSUM-DDNAMN2                                   
009960     MOVE 'MEMO'     TO POSTSUM-TRANSTYP                                  
009970     CALL POSTSUM USING POSTSUM-PARM                                      
009980     .                                                                    
009981     EJECT                                                                
009990 S13-SKRIV-MEMOFIL SECTION.                                               
010000                                                                          
010100     WRITE MEMO-POST FROM MEMO-AREA                                       
010200     MOVE 'W11179'   TO POSTSUM-FDNAMN                                    
010300     MOVE 'W11178D3' TO POSTSUM-DDNAMN2                                   
010400     MOVE 'MEMO'     TO POSTSUM-TRANSTYP                                  
010500     CALL POSTSUM USING POSTSUM-PARM                                      
010600     .                                                                    
