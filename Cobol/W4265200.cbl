001100 ID DIVISION.                                                             
001200                                                                          
001300 PROGRAM-ID.     W4265200.                                                
001400 AUTHOR.         INGER NILSSON.                                           
001500 DATE-WRITTEN.   93/09/21.                                                
001510 DATE-COMPILED.                                                           
001600                                                                          
001900*    FUNKTION:                                                            
002000*        LÄSER SB-FIL MED W6H7 OCH SKAPAR UTFIL KOMPLETTERAD              
002001*        MED LAGERKONTO, PRODUKTSLAG OCH STDPRIS FÖR UTSKRIFT             
002002*        AV LISTA "KONTROLLRAPPORT EJ BEHANDLADE AV EKONOMI               
002003*        VID REDOVISNINGSPERIOD" I W4263800.                              
002004*                                                                         
002005*     URVAL:                                                              
002006*     - EJ ANNULERADE KR                                                  
002007*     - EJ ÖVER/UNDERLEVERANS KR                                          
002008*     - STATUS 2-4 KR                                                     
002009*     - ÅTGÄRD MED ANTAL I RETUR, SKROT OCH/EL SALDOJUSTERING KR          
002010*     - KR I LAGER                                                        
002011*     - KR I INLEVERANS SOM ÄR R32-RAPPORTERADE                           
002020*                                                                         
002210*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002220*        PROGRAMMET LÄSER      W6D1                                       
002300*                                                                         
002400*    ABENDKODER:                                                          
002500*        U0016 -  . . . .                                                 
002600*        U1000 -  . . . .                                                 
002700*                                                                         
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003501     SKIP2                                                                
003502*          --- SB-FIL W6H7                                                
003503     SELECT W42620                     ASSIGN TO W42652D1.                
003504     SKIP2                                                                
003505*          --- FIL FÖR LISTA KR EJ BEHANDLADE AV EKONOMI                  
003510     SELECT W42652                     ASSIGN TO W42652D2.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004101     SKIP3                                                                
004102 FD  W42620                                                               
004103     RECORDING       V                                                    
004104     BLOCK CONTAINS  0.                                                   
004106 01  FILLER                      PIC X(398).                              
004112     SKIP3                                                                
004113 FD  W42652                                                               
004114     RECORDING       F                                                    
004115     BLOCK CONTAINS  0.                                                   
004116     SKIP2                                                                
004120*01  POST -COPY W4265201 -PRE  UT-  -L.                                   
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401*    -COPY WY2000W1                                                       
004410     SKIP3                                                                
004500 77  IDPGM                       PIC X(8)    VALUE 'W4265200'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004901                                                                          
004902 77  W42620-EOF-SW               PIC X       VALUE 'N'.                   
004910     88  END-OF-W42620                       VALUE 'J'.                   
005000     EJECT                                                                
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800*                                                                         
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006210     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006500                                                                          
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007201     EJECT                                                                
007402*    --- PARAMETRAR TILL POSTSUM                                          
007403*                                                                         
007404*01  -COPY W0005   -PRE  POSTSUM-                                         
007407     EJECT                                                                
007408 01  IN-AREA-START               PIC X(24)   VALUE                        
007409                                 'IN-AREA-START  '.                       
007410     SKIP2                                                                
007411 01  IN-AREA.                                                             
007412     03  IN-IDPTYP               PIC X(3).                                
007413*    03  H01-AREA -COPY W6H701  -PRE IN-                                  
007414     EJECT                                                                
007415 01  UT-AREA-START               PIC X(24)   VALUE                        
007416                                 'UT-AREA-START  '.                       
007417     SKIP2                                                                
007418                                                                          
007420*01  AREA -COPY W4265201     -PRE UT-                                     
007500     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008201     03  W-IDARTNR-X.                                                     
008202         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008203     03  W-W6D1BSEQ-X.                                                    
008204         05  W-BSEQ-IDLOPNRM     PIC S9(9)  VALUE ZERO COMP-3.            
008300     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FINNS                       VALUE '  '.                  
008700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008900     SKIP2                                                                
009000 01  GODK-STATUSKODER.                                                    
009100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     SKIP3                                                                
009300 01  SSA1                        PIC X(64).                               
009400 01  SSA2                        PIC X(64).                               
009500     EJECT                                                                
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010200     SKIP3                                                                
010300 01  DLI-IO-AREA.                                                         
010400     03  IO-AREA                 PIC X(900) VALUE SPACE.                  
010501     SKIP3                                                                
010502     03  WLARTC01 REDEFINES IO-AREA.                                      
010503*        05  -COPY WDK601  -PRE ARTC-                                     
010504     EJECT                                                                
010505     03  WLARTC11 REDEFINES IO-AREA.                                      
010510*        05  -COPY WDK611  -PRE ARTC-                                     
010800     EJECT                                                                
010840 01  DLI-IO-AREA2.                                                        
010850     03  IO-AREA2                PIC X(290)  VALUE SPACE.                 
010860     SKIP3                                                                
010870     03  W6INLA01 REDEFINES IO-AREA2.                                     
010880*        05  -COPY W6D111  -PRE INLA-                                     
010890     EJECT                                                                
010900 LINKAGE SECTION.                                                         
011000                                                                          
011101     EJECT                                                                
011105*01  -COPY W0008  -PRE ARTC-                                              
011110     05  FILLER                  PIC X.                                   
011200     EJECT                                                                
011300*01  -COPY W0008  -PRE INLA-                                              
011301     05  FILLER                  PIC X.                                   
011302     EJECT                                                                
011303 PROCEDURE DIVISION  USING ARTC-PCB INLA-PCB.                             
011310     ENTRY 'DLITCBL' USING ARTC-PCB INLA-PCB.                             
011400                                                                          
011700     PERFORM A-INIT                                                       
011810     PERFORM S01-LAES-W42620                                              
011900     PERFORM UNTIL END-OF-W42620                                          
012010       PERFORM B-BEHANDLING                                               
012610       PERFORM S01-LAES-W42620                                            
012700     END-PERFORM                                                          
012900                                                                          
013000     PERFORM Z-FINIT                                                      
013100                                                                          
013200     MOVE ZERO TO RETURN-CODE                                             
013300     GOBACK                                                               
013400     .                                                                    
013500     EJECT                                                                
013600 A-INIT SECTION.                                                          
013701                                                                          
013710     OPEN INPUT  W42620                                                   
013801                                                                          
013810     OPEN OUTPUT W42652                                                   
014110     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014300     .                                                                    
014400     EJECT                                                                
014410 B-BEHANDLING SECTION.                                                    
014411                                                                          
014420     IF IN-IDPTYP = '701'                                                 
014440        IF IN-KR-FLANNULL = 'N'                                           
014450           IF IN-KR-KDKRSTA = '2' OR '3' OR '4'                           
014460              IF IN-KR-IDKRFEL NOT = 'PA' AND 'PB'                        
014470                 IF IN-KR-KVART-RET   > +0                                
014480                 OR IN-KR-KVART-SKROT > +0                                
014490                 OR IN-KR-KVART-SJUST > +0                                
014491                 OR IN-KR-KVART-SJUST < +0                                
014492                    IF IN-KR-IDLOPNRM = +0                                
014493                       PERFORM S11-SKRIV-W42652                           
014494                    ELSE                                                  
014496                       MOVE IN-KR-IDLOPNRM TO W-BSEQ-IDLOPNRM             
014498                       PERFORM IMS-GU-W6D1                                
014499                       IF SEGMENT-FINNS                                   
014500                         IF INLA-ART-FLKLAR = JA                          
014509                           PERFORM S11-SKRIV-W42652                       
014510                         END-IF                                           
014511                       END-IF                                             
014520                    END-IF                                                
014523                 END-IF                                                   
014524              END-IF                                                      
014525           END-IF                                                         
014526        END-IF                                                            
014527     END-IF                                                               
014528     .                                                                    
014530     EJECT                                                                
014600 Z-FINIT SECTION.                                                         
014601     CLOSE W42620                                                         
014610           W42652                                                         
014701     SKIP2                                                                
014702     MOVE 'S' TO POSTSUM-OPKOD                                            
014710     CALL POSTSUM USING POSTSUM-PARM                                      
014800     .                                                                    
014901     EJECT                                                                
014902 S01-LAES-W42620  SECTION.                                                
014903                                                                          
014904     READ W42620 INTO IN-AREA                                             
014905     AT END                                                               
014907        SET END-OF-W42620 TO TRUE                                         
014908                                                                          
014909     NOT AT END                                                           
014910        MOVE 'W42620'   TO POSTSUM-FDNAMN                                 
014911        MOVE 'W42652D1' TO POSTSUM-DDNAMN2                                
014912        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
014913        CALL POSTSUM USING POSTSUM-PARM                                   
014914     END-READ                                                             
014920     .                                                                    
015001     EJECT                                                                
015002 S11-SKRIV-W42652 SECTION.                                                
015003                                                                          
015004     MOVE IN-KR-IDARTNR     TO UT-IDARTNR                                 
015005     MOVE IN-KR-IDKR        TO UT-IDKR                                    
015006     MOVE IN-KR-KVART-RET   TO UT-KVART-RET                               
015007     MOVE IN-KR-KVART-SKROT TO UT-KVART-SKROT                             
015008     MOVE IN-KR-KVART-SJUST TO UT-KVART-SJUST                             
015009     MOVE IN-KR-IDARTNR     TO W-IDARTNR                                  
015010     MOVE IN-KR-IDDC        TO UT-IDDC                                    
015011                                                                          
015012     PERFORM IMS-GET-ARTC01                                               
015013     IF SEGMENT-FINNS                                                     
015014        MOVE ARTC-ART-KDPRODSL  TO UT-KDPRODSL                            
015015        PERFORM IMS-GNP-ARTC11                                            
015016        IF SEGMENT-FINNS                                                  
015017           MOVE ARTC-CLAG-IDLKTO    TO UT-IDLKTO                          
015018           MOVE ARTC-CLAG-PRARTSTD  TO UT-PRARTSTD                        
015019        ELSE                                                              
015020           MOVE +0             TO UT-IDLKTO                               
015021                                  UT-PRARTSTD                             
015022        END-IF                                                            
015023     ELSE                                                                 
015024        MOVE +0             TO UT-KDPRODSL                                
015025                               UT-IDLKTO                                  
015026                               UT-PRARTSTD                                
015027     END-IF                                                               
015028     WRITE UT-POST FROM UT-AREA                                           
015029                                                                          
015030     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
015031     MOVE 'W42652'   TO POSTSUM-FDNAMN                                    
015032     MOVE 'W42652D2' TO POSTSUM-DDNAMN2                                   
015033     CALL POSTSUM USING POSTSUM-PARM                                      
015040     .                                                                    
015800     EJECT                                                                
015900* --- IMS SEKTIONER ---                                                   
016000                                                                          
016102 IMS-GET-ARTC01 SECTION.                                                  
016103     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
016104          DELIMITED BY SIZE INTO SSA1                                     
016105     MOVE '  GE' TO GODK-STATUSKODER                                      
016106     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
016107     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
016108     PERFORM IMS-STATUSKONTROLL                                           
016109     .                                                                    
016110     SKIP2                                                                
016111 IMS-GNP-ARTC11 SECTION.                                                  
016114     MOVE 'WLARTC11 '         TO SSA1                                     
016116     MOVE '  GE' TO GODK-STATUSKODER                                      
016117     CALL CBLTDLI USING  GNP  ARTC-PCB DLI-IO-AREA SSA1                   
016118     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
016119     PERFORM IMS-STATUSKONTROLL                                           
016120     .                                                                    
016201     SKIP2                                                                
016202 IMS-GU-W6D1 SECTION.                                                     
016203     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X ')'                        
016204          DELIMITED BY SIZE INTO SSA1                                     
016205     MOVE '  GE' TO GODK-STATUSKODER                                      
016206     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA2 SSA1                     
016207     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
016208     PERFORM IMS-STATUSKONTROLL                                           
016209     .                                                                    
016210     SKIP3                                                                
016300 IMS-STATUSKONTROLL SECTION.                                              
016500     SET STATUS-IX TO 1                                                   
016600     SEARCH GODK-STATUS                                                   
016700       AT END                                                             
016800         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
016900         DISPLAY FELTEXT                                                  
017000         CALL FELLOG                                                      
017100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017200         CONTINUE                                                         
017300     END-SEARCH                                                           
017400     .                                                                    
017410     EJECT                                                                
