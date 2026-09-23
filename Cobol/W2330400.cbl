001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W2330400.                                                
001300*AUTHOR.         STEFAN KIHLBERG.                                         
001400*DATE-WRITTEN.   93/03/12.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        LÄSER FILEN W21155 FRÅN PGM W21195 I W211P105                    
002000*        LÄSER INKÖPARNUMMER PÅ WDK611 OCH SKRIVER FILEN W23305           
002100*        INKÖPARNUMMER PÅ INFILEN KAN VARA OFULLSTÄNDIDIGT.               
002200*                                                                         
002310*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
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
003602*          --- 310-POSTER                                                 
003603     SELECT W21155                     ASSIGN TO W23304D1.                
003604     SKIP2                                                                
003605*          --- R310-POSTER MED JUSTERAT INKÖPARNR                         
003610     SELECT W23305                     ASSIGN TO W23304D2.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004201     SKIP3                                                                
004202 FD  W21155                                                               
004203     RECORDING       F                                                    
004204     BLOCK CONTAINS  0.                                                   
004205     SKIP2                                                                
004206*01  -COPY W211310      -L.                                               
004207     SKIP3                                                                
004208 FD  W23305                                                               
004209     RECORDING       F                                                    
004210     BLOCK CONTAINS  0.                                                   
004211     SKIP2                                                                
004220*01  POST -COPY W211310 -PRE  W23305-  -L.                                
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004501                                                                          
004510*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W2330400'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
005001                                                                          
005002 77  W21155-EOF-SW               PIC X       VALUE 'N'.                   
005010     88  END-OF-W21155                       VALUE 'J'.                   
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
007502 01  W21155-AREA-START          PIC X(24)   VALUE                         
007503                                 'W21155-AREA-START  '.                   
007504     SKIP2                                                                
007505                                                                          
007506*01  AREA -COPY W211310     -PRE W21155-                                  
007507     EJECT                                                                
007508 01  W23305-AREA-START           PIC X(24)   VALUE                        
007509                                 'W23305-AREA-START  '.                   
007510     SKIP2                                                                
007511                                                                          
007520*01  AREA -COPY W211310    -PRE W23305-                                   
007600     EJECT                                                                
007700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007800*                                                                         
007900     EJECT                                                                
008000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008100     SKIP3                                                                
008200 01  NYCKLAR-TILL-DLI.                                                    
008301     03  W-IDARTNR-X.                                                     
008302         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
008303     03  W-KDSEGKEY-X.                                                    
008310         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
008400     SKIP2                                                                
008500*    --- STATUS-KOD FRÅN IMS                                              
008600 01  STATUS-WS                   PIC XX.                                  
008700     88  SEGMENT-FINNS                       VALUE '  '.                  
008900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
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
010200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010300     SKIP3                                                                
010400 01  DLI-IO-AREA.                                                         
010500     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
010601     SKIP3                                                                
010602     03  WLARTC01 REDEFINES IO-AREA.                                      
010603*        05  -COPY WDK601                                                 
010604     SKIP3                                                                
010605     03  WLARTC11 REDEFINES IO-AREA.                                      
010610*        05  -COPY WDK611                                                 
010900     EJECT                                                                
011000 LINKAGE SECTION.                                                         
011100                                                                          
011201     EJECT                                                                
011202*01  -COPY W0008  -PRE ARTC-                                              
011210     05  FILLER                  PIC X.                                   
011300     EJECT                                                                
011401 PROCEDURE DIVISION  USING ARTC-PCB.                                      
011410     ENTRY 'DLITCBL' USING ARTC-PCB.                                      
011500                                                                          
011700     SKIP2                                                                
011800     PERFORM A-INIT                                                       
011910     PERFORM S01-LAES-W21155                                              
012000     PERFORM UNTIL END-OF-W21155                                          
012100        IF W21155-KDPKINR NUMERIC                                         
012210           MOVE W21155-AREA TO W23305-AREA                                
012300        ELSE                                                              
012400           MOVE W21155-IDARTNR TO W-IDARTNR                               
012410           PERFORM IMS-GET-WDK601                                         
012420           IF SEGMENT-FINNS                                               
012430              IF ART-KDERS-UTG = 0                                        
012440                 PERFORM IMS-GET-ARTC11                                   
012450                 MOVE W21155-AREA TO W23305-AREA                          
012451******                                                                    
012452                 IF CLAG-IDINK (1:3) NUMERIC                              
012453                    MOVE CLAG-IDINK (1:3) TO W23305-KDPKINR               
012905                 ELSE                                                     
012906                    IF CLAG-IDINK (2:3) NUMERIC                           
012908                       MOVE CLAG-IDINK (2:3) TO W23305-KDPKINR            
012909                    ELSE                                                  
012910                       MOVE ZERO TO W23305-KDPKINR                        
012911                    END-IF                                                
012912                 END-IF                                                   
012913******                                                                    
012914              END-IF                                                      
012915           END-IF                                                         
012916        END-IF                                                            
012917        PERFORM S11-SKRIV-W23305                                          
012918        PERFORM S01-LAES-W21155                                           
012919     END-PERFORM                                                          
012920                                                                          
013000                                                                          
013100     PERFORM Z-FINIT                                                      
013200                                                                          
013300     MOVE ZERO TO RETURN-CODE                                             
013400     GOBACK                                                               
013500     .                                                                    
013600     EJECT                                                                
013700 A-INIT SECTION.                                                          
013801                                                                          
013810     OPEN INPUT  W21155                                                   
013901                                                                          
013910     OPEN OUTPUT W23305                                                   
014000     SKIP2                                                                
014100     ACCEPT DAGENS-DATUM  FROM DATE                                       
014210     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014400     .                                                                    
014500     EJECT                                                                
014600 Z-FINIT SECTION.                                                         
014701     CLOSE W21155                                                         
014710           W23305                                                         
014801     SKIP2                                                                
014802     MOVE 'S' TO POSTSUM-OPKOD                                            
014810     CALL POSTSUM USING POSTSUM-PARM                                      
014900     .                                                                    
015001     EJECT                                                                
015002 S01-LAES-W21155  SECTION.                                                
015003     SKIP2                                                                
015004     READ W21155 INTO W21155-AREA                                         
015005     AT END                                                               
015007        SET END-OF-W21155 TO TRUE                                         
015008                                                                          
015009     NOT AT END                                                           
015010        MOVE 'W21155' TO POSTSUM-FDNAMN                                   
015011        MOVE 'W23304D1' TO POSTSUM-DDNAMN2                                
015013        CALL POSTSUM USING POSTSUM-PARM                                   
015014     END-READ                                                             
015020     .                                                                    
015101     EJECT                                                                
015102 S11-SKRIV-W23305 SECTION.                                                
015103     SKIP2                                                                
015104     WRITE W23305-POST FROM W23305-AREA                                   
015105                                                                          
015106     MOVE W23305-IDPTYP TO POSTSUM-TRANSTYP                               
015107     MOVE 'W23305' TO POSTSUM-FDNAMN                                      
015108     MOVE 'W23304D2' TO POSTSUM-DDNAMN2                                   
015109     CALL POSTSUM USING POSTSUM-PARM                                      
015110     .                                                                    
015300     EJECT                                                                
016000* --- IMS SEKTIONER ---                                                   
016100     SKIP3                                                                
016201     EJECT                                                                
016202 IMS-GET-WDK601 SECTION.                                                  
016203     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
016204          DELIMITED BY SIZE INTO SSA1                                     
016205     MOVE '  GE' TO GODK-STATUSKODER                                      
016206     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
016207     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
016208     PERFORM IMS-STATUSKONTROLL                                           
016209     .                                                                    
016210     EJECT                                                                
016211 IMS-GET-ARTC11 SECTION.                                                  
016212     MOVE 'WLARTC11 ' TO SSA1                                             
016214     MOVE '  GE' TO GODK-STATUSKODER                                      
016215     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
016216     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
016217     PERFORM IMS-STATUSKONTROLL                                           
016220     .                                                                    
016300     EJECT                                                                
016400 IMS-STATUSKONTROLL SECTION.                                              
016500     SKIP2                                                                
016600     SET STATUS-IX TO 1                                                   
016700     SEARCH GODK-STATUS                                                   
016800       AT END                                                             
016900         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
017000         DISPLAY FELTEXT                                                  
017100         CALL FELLOG                                                      
017200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017300         CONTINUE                                                         
017400     END-SEARCH                                                           
017500     .                                                                    
