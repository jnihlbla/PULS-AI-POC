001000 ID DIVISION.                                                             
001100                                                                          
001200 PROGRAM-ID.     W3350300.                                                
001300 AUTHOR.         THOMAS LARSSON.                                          
001400 DATE-WRITTEN.   94/04/26.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001800*        LÄSER NER WDC1 OCH SKAPAR EN FIL MED VARJE EN ARTIKELS           
001900*        ALLA MARKNADS PRARTBTO-MARK (PRIS) OCH KDARTRAB (RABATT)         
002000*                                                                         
002100*                                                                         
002110*        PROGRAMMET LÄSER      WPRIA (WDC1)                               
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401     SKIP2                                                                
003402*          --- FIL MED ALLA ARTIKLAR OCH DESS PRISER PER MARKNAD          
003410     SELECT W33504                     ASSIGN TO W33503D1.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W33504                                                               
004003     RECORDING       F                                                    
004004     BLOCK CONTAINS  0.                                                   
004005     SKIP2                                                                
004010*01  POST -COPY W33504 -PRE  UT-  -L.                                     
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W3350300'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  SPAR-IDARTNR                PIC S9(9)   VALUE ZERO COMP-3.           
004800 77  FIRST-TIME                  PIC X       VALUE SPACE.                 
004810                                                                          
004820 77  SKRIV-SW                    PIC X       VALUE 'N'.                   
004830     88  SKRIV-POST                          VALUE 'J'.                   
004840                                                                          
004900     EJECT                                                                
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM.                                       
005200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200     SKIP2                                                                
006300*    --- PARAMETRAR TILL ABEND                                            
006400                                                                          
006500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006700     SKIP2                                                                
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007302 01  UT-AREA-START               PIC X(24)   VALUE                        
007303                                 'UT-AREA-START  '.                       
007304     SKIP2                                                                
007305                                                                          
007310*01  AREA -COPY W33504     -PRE UT-                                       
007400     EJECT                                                                
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  NYCKLAR-TILL-DLI.                                                    
008101     03  W-IDARTNR-X.                                                     
008110         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008200     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008610     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008700     SKIP2                                                                
008800 01  GODK-STATUSKODER.                                                    
008900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009000     SKIP3                                                                
009100 01  SSA1                        PIC X(64).                               
009200 01  SSA2                        PIC X(64).                               
009300     EJECT                                                                
009400*    --- IMS FUNKTIONSKODER                                               
009500*01  -COPY W0003                                                          
009600     EJECT                                                                
009800*    ---  DLI INPUT-OUTPUT AREA                                           
009900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010000     SKIP3                                                                
010100 01  DLI-IO-AREA.                                                         
010302     03  WPRIA01.                                                         
010310*        05  -COPY WDC101  -PRE PRIA-                                     
010600     EJECT                                                                
010700 LINKAGE SECTION.                                                         
010902*01  -COPY W0008  -PRE PRIA-                                              
010910     05  FILLER                  PIC X.                                   
011000     EJECT                                                                
011101 PROCEDURE DIVISION  USING PRIA-PCB.                                      
011102 MAIN SECTION.                                                            
011110     ENTRY 'DLITCBL' USING PRIA-PCB.                                      
011200                                                                          
011400     SKIP2                                                                
011500     PERFORM A-INIT                                                       
011600     PERFORM IMS-GN-WDC1                                                  
011610                                                                          
011700     PERFORM UNTIL SEGMENT-SLUT                                           
011800                                                                          
011900       PERFORM BA-NOLLSTAELL-AREA                                         
012000       PERFORM B-FLYTTA-FAELT                                             
012300                                                                          
012400       PERFORM IMS-GN-WDC1                                                
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
013601                                                                          
013610     OPEN OUTPUT W33504                                                   
013700     SKIP2                                                                
013800     ACCEPT DAGENS-DATUM  FROM DATE                                       
013910     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014100     .                                                                    
014200     EJECT                                                                
014210 B-FLYTTA-FAELT SECTION.                                                  
014220     SKIP2                                                                
014231     IF PRIA-ART-IDMARKBO = 'A'                                           
014233       MOVE PRIA-ART-KDARTRAB TO UT-KDARTRAB-A                            
014234       MOVE PRIA-ART-PRARTBTO-MARK TO UT-PRARTBTO-MARK-A                  
014235       MOVE PRIA-ART-IDARTNR TO UT-IDARTNR                                
014236       PERFORM S11-SKRIV-W33504                                           
014237     ELSE                                                                 
014238       IF PRIA-ART-IDMARKBO = 'B'                                         
014239         MOVE PRIA-ART-KDARTRAB TO UT-KDARTRAB-B                          
014240         MOVE PRIA-ART-PRARTBTO-MARK TO UT-PRARTBTO-MARK-B                
014241         MOVE PRIA-ART-IDARTNR TO UT-IDARTNR                              
014242         PERFORM S11-SKRIV-W33504                                         
014243       ELSE                                                               
014244         IF PRIA-ART-IDMARKBO = 'C'                                       
014245           MOVE PRIA-ART-KDARTRAB TO UT-KDARTRAB-C                        
014246           MOVE PRIA-ART-PRARTBTO-MARK TO UT-PRARTBTO-MARK-C              
014247           MOVE PRIA-ART-IDARTNR TO UT-IDARTNR                            
014248           PERFORM S11-SKRIV-W33504                                       
014249         ELSE                                                             
014256           IF PRIA-ART-IDMARKBO = 'E'                                     
014257             MOVE PRIA-ART-KDARTRAB TO UT-KDARTRAB-E                      
014258             MOVE PRIA-ART-PRARTBTO-MARK TO                               
014259                  UT-PRARTBTO-MARK-E                                      
014260             MOVE PRIA-ART-IDARTNR TO UT-IDARTNR                          
014261             PERFORM S11-SKRIV-W33504                                     
014262           ELSE                                                           
014263             IF PRIA-ART-IDMARKBO = 'F'                                   
014264               MOVE PRIA-ART-KDARTRAB TO UT-KDARTRAB-F                    
014265               MOVE PRIA-ART-PRARTBTO-MARK TO                             
014266                    UT-PRARTBTO-MARK-F                                    
014267               MOVE PRIA-ART-IDARTNR TO UT-IDARTNR                        
014268               PERFORM S11-SKRIV-W33504                                   
014278             END-IF                                                       
014279           END-IF                                                         
014281         END-IF                                                           
014282       END-IF                                                             
014283     END-IF                                                               
014284     .                                                                    
014285     EJECT                                                                
014286 BA-NOLLSTAELL-AREA SECTION.                                              
014290     SKIP2                                                                
014291     MOVE ZERO TO UT-IDARTNR                                              
014292                  UT-KDARTRAB-A                                           
014293                  UT-PRARTBTO-MARK-A                                      
014294                  UT-KDARTRAB-B                                           
014295                  UT-PRARTBTO-MARK-B                                      
014296                  UT-KDARTRAB-C                                           
014297                  UT-PRARTBTO-MARK-C                                      
014298                  UT-KDARTRAB-D                                           
014299                  UT-PRARTBTO-MARK-D                                      
014300                  UT-KDARTRAB-E                                           
014310                  UT-PRARTBTO-MARK-E                                      
014320                  UT-KDARTRAB-F                                           
014330                  UT-PRARTBTO-MARK-F                                      
014331                  UT-KDARTRAB-G                                           
014332                  UT-PRARTBTO-MARK-G                                      
014340     .                                                                    
014350     EJECT                                                                
014400 Z-FINIT SECTION.                                                         
014410     CLOSE W33504                                                         
014501     SKIP2                                                                
014502     MOVE 'S' TO POSTSUM-OPKOD                                            
014510     CALL POSTSUM USING POSTSUM-PARM                                      
014600     .                                                                    
014801     EJECT                                                                
014802 S11-SKRIV-W33504 SECTION.                                                
014803     SKIP2                                                                
014804     WRITE UT-POST FROM UT-AREA                                           
014805                                                                          
014806     MOVE 'WDC1'    TO POSTSUM-TRANSTYP                                   
014807     MOVE 'W33504' TO POSTSUM-FDNAMN                                      
014808     MOVE 'W33503D1' TO POSTSUM-DDNAMN2                                   
014809     CALL POSTSUM USING POSTSUM-PARM                                      
014810     .                                                                    
015000     EJECT                                                                
015700* --- IMS SEKTIONER ---                                                   
015800     SKIP3                                                                
015902 IMS-GN-WDC1    SECTION.                                                  
015903     SKIP2                                                                
015904     CALL CBLTDLI USING GN PRIA-PCB DLI-IO-AREA                           
015905     MOVE PRIA-STATUS-CODE TO STATUS-WS                                   
015906     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
015907     PERFORM IMS-STATUSKONTROLL                                           
015910     .                                                                    
016000     SKIP2                                                                
016100 IMS-STATUSKONTROLL SECTION.                                              
016200     SKIP2                                                                
016300     SET STATUS-IX TO 1                                                   
016400     SEARCH GODK-STATUS                                                   
016500       AT END                                                             
016600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016610         DELIMITED BY SIZE INTO FELTEXT                                   
016700         DISPLAY FELTEXT                                                  
016800         CALL FELLOG                                                      
016900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017000         CONTINUE                                                         
017100     END-SEARCH                                                           
017200     .                                                                    
