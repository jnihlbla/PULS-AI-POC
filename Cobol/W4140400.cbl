000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4140400.                                                
000400 AUTHOR.         GUNNAR LARSSON.                                          
000500 DATE-WRITTEN.   92/03/20.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET LÄSER INFIL MED INFORMATION OM VILKA                  
001100*        TRANSAKTIONER SOM SKALL RENSAS PÅ WDR6-BASEN.                    
001200*                                                                         
001300*        DET ÄR OK ATT SEGMENTET SAKNAS.                                  
001400*        DETTA FÖR ATT MAN LÄTT SKALL KUNNA ÅTERSTARTA MED                
001500*        MED SAMMA INFIL VID EN EVENTUELL ABEND.                          
001600*                                                                         
001700*        PROGRAMMET UPPATERAR WLFILA (WDR6)                               
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- WDR6 TRANSAR SOM SKALL RENSAS                              
003200     SELECT W41404                     ASSIGN TO W41404D1.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W41404                                                               
003900     LABEL RECORD    STANDARD                                             
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200     SKIP2                                                                
004300*01  -COPY W414004         -L.                                            
004400                                                                          
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700     SKIP2                                                                
004701                                                                          
004710*    -- CHECKED BY WY2000                                                 
004800 77  IDPGM                       PIC X(8)    VALUE 'W4140400'.            
004900 77  MSG-IO-AREA-LENGTH          PIC S9(9)   VALUE +32 COMP SYNC.         
005000 77  MSG-IO-AREA                 PIC X(32)   VALUE SPACE.                 
005100 77  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005200 77  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005300 77  CHKP-ANT                    PIC S9(3)   VALUE +0     COMP-3.         
005400 77  CHKP-MAX                    PIC S9(3)   VALUE +100   COMP-3.         
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700                                                                          
005800 77  W41404-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-W41404                       VALUE 'J'.                   
006000     EJECT                                                                
006100 01  DAGENS-DATUM.                                                        
006200     03  DAGENS-DATUM-AAR        PIC X(2)    VALUE SPACE.                 
006300     03  DAGENS-DATUM-MAANAD     PIC X(2)    VALUE SPACE.                 
006400     03  DAGENS-DATUM-DAG        PIC X(2)    VALUE SPACE.                 
006500     EJECT                                                                
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL POSTSUM                                          
007300*                                                                         
007400*01  -COPY W0005      -PRE  POSTSUM-                                      
007500     EJECT                                                                
007600 01  W41404-AREA-START           PIC X(24)   VALUE                        
007700                                             'W41404-AREA-START'.         
007800     SKIP2                                                                
007900                                                                          
008000*01  AREA -COPY W414004    -PRE W41404-                                   
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008300     SKIP3                                                                
008400 01  NYCKLAR-TILL-DLI.                                                    
008500     03  W-WDR601KY-X.                                                    
008600         05  W-IDPGM             PIC X(8)    VALUE SPACE.                 
008700         05  W-TIREGDAT          PIC S9(7)   VALUE ZERO COMP-3.           
008800         05  W-TIKLOCK           PIC S9(9)   VALUE ZERO COMP-3.           
008900         05  W-IDSEKVNR          PIC S9(3)   VALUE ZERO COMP-3.           
009000         05  W-IDSYSTEM          PIC X(4)    VALUE SPACE.                 
009100         05  W-IDPTYP            PIC X(3)    VALUE SPACE.                 
009200         05  W-IDVTYP            PIC X(1)    VALUE SPACE.                 
009300     SKIP2                                                                
009400*    --- STATUS-KOD FRÅN IMS                                              
009500 01  STATUS-WS                   PIC XX.                                  
009600     88  SEGMENT-FINNS                       VALUE '  '.                  
009700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010000     88  IMS-EJ-OK                           VALUE 'XD'.                  
010100     SKIP2                                                                
010200 01  GODK-STATUSKODER.                                                    
010300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010400     SKIP3                                                                
010500 01  SSA1                        PIC X(64).                               
010600 01  SSA2                        PIC X(64).                               
010700     EJECT                                                                
010800*    --- IMS FUNKTIONSKODER                                               
010900*01  -COPY W0003                                                          
011000     EJECT                                                                
011100*    ---  DLI INPUT-OUTPUT AREA                                           
011200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011300     SKIP3                                                                
011400 01  DLI-IO-AREA.                                                         
011500     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
011600     SKIP3                                                                
011700     03  WLFILA01 REDEFINES IO-AREA.                                      
011800*        05  -COPY WDR601     -PRE FILA-                                  
011900     EJECT                                                                
012000 LINKAGE SECTION.                                                         
012100                                                                          
012200*01  -COPY W0009      -PRE MSG-                                           
012300     EJECT                                                                
012400*01  -COPY W0008      -PRE FILA-                                          
012500     05  FILLER                  PIC X.                                   
012600     EJECT                                                                
012700 PROCEDURE DIVISION  USING MSG-PCB FILA-PCB.                              
012800     ENTRY 'DLITCBL' USING MSG-PCB FILA-PCB.                              
012900                                                                          
013000     PERFORM A-INIT                                                       
013100     PERFORM S01-LAES-W41404                                              
013200                                                                          
013300     PERFORM UNTIL END-OF-W41404                                          
013400                                                                          
013500       IF CHKP-ANT             > CHKP-MAX                                 
013600         PERFORM X-TAG-CHECKPOINT                                         
013700       END-IF                                                             
013800                                                                          
013900       PERFORM B-BEHANDLA-INPOST                                          
014000                                                                          
014100       PERFORM S01-LAES-W41404                                            
014200     END-PERFORM                                                          
014300                                                                          
014400     PERFORM Z-FINIT                                                      
014500                                                                          
014600     MOVE ZERO                 TO RETURN-CODE                             
014700     GOBACK                                                               
014800     .                                                                    
014900     EJECT                                                                
015000 A-INIT SECTION.                                                          
015100                                                                          
015200     PERFORM IMS-RESTART                                                  
015300                                                                          
015400     OPEN INPUT W41404                                                    
015500                                                                          
015600     ACCEPT DAGENS-DATUM       FROM DATE                                  
015700                                                                          
015800     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
015900     MOVE +0                   TO  CHKP-ANT                               
016000     .                                                                    
016100     EJECT                                                                
016200 B-BEHANDLA-INPOST      SECTION.                                          
016300                                                                          
016400     MOVE W41404-RENS-IDPGM      TO W-IDPGM                               
016500     MOVE W41404-RENS-TIREGDAT   TO W-TIREGDAT                            
016600     MOVE W41404-RENS-TIKLOCK    TO W-TIKLOCK                             
016700     MOVE W41404-RENS-IDSEKVNR   TO W-IDSEKVNR                            
016800     MOVE W41404-RENS-CT-IDSYSTEM TO W-IDSYSTEM                           
016900     MOVE W41404-RENS-CT-IDPTYP  TO W-IDPTYP                              
017000     MOVE W41404-RENS-CT-IDVTYP  TO W-IDVTYP                              
017100                                                                          
017200     PERFORM IMS-GET-FILA-FIL                                             
017300     IF SEGMENT-FINNS                                                     
017400       ADD +1                    TO  CHKP-ANT                             
017500       PERFORM IMS-DLET-FILA                                              
017600     END-IF                                                               
017700     .                                                                    
017800     EJECT                                                                
017900 Z-FINIT SECTION.                                                         
018000                                                                          
018100     CLOSE W41404                                                         
018200                                                                          
018300     MOVE 'S'                  TO POSTSUM-OPKOD                           
018400     CALL POSTSUM              USING POSTSUM-PARM                         
018500     .                                                                    
018600     EJECT                                                                
018700 S01-LAES-W41404  SECTION.                                                
018800                                                                          
018900     READ W41404 INTO W41404-AREA                                         
019000     AT END                                                               
019100        SET END-OF-W41404      TO TRUE                                    
019200                                                                          
019300     NOT AT END                                                           
019400        MOVE 'W41404'          TO POSTSUM-FDNAMN                          
019500        MOVE 'W41404D1'        TO POSTSUM-DDNAMN2                         
019600        MOVE W41404-RENS-CT-IDPTYP                                        
019700                               TO POSTSUM-TRANSTYP                        
019800        CALL POSTSUM           USING POSTSUM-PARM                         
019900     END-READ                                                             
020000     .                                                                    
020100     EJECT                                                                
020200 X-TAG-CHECKPOINT   SECTION.                                              
020300                                                                          
020400     PERFORM IMS-CHECKPOINT                                               
020500     MOVE +0                   TO  CHKP-ANT                               
020600     .                                                                    
020700     EJECT                                                                
020800* --- IMS SEKTIONER ---                                                   
020900     SKIP3                                                                
021000 IMS-GET-FILA-FIL SECTION.                                                
021100     STRING 'WLFILA01(WDR601KY =' W-WDR601KY-X ')'                        
021200          DELIMITED BY SIZE INTO SSA1                                     
021300     MOVE '  GE'               TO GODK-STATUSKODER                        
021400     CALL CBLTDLI USING GHU FILA-PCB DLI-IO-AREA SSA1                     
021500     MOVE FILA-STATUS-CODE     TO STATUS-WS                               
021600     PERFORM IMS-STATUS-KONTROLL                                          
021700     .                                                                    
021800     SKIP3                                                                
021900 IMS-DLET-FILA SECTION.                                                   
022000                                                                          
022100     MOVE '  '                 TO GODK-STATUSKODER                        
022200     CALL CBLTDLI USING DLET FILA-PCB DLI-IO-AREA                         
022300     MOVE FILA-STATUS-CODE     TO STATUS-WS                               
022400     PERFORM IMS-STATUS-KONTROLL                                          
022500     .                                                                    
022600     EJECT                                                                
022700 IMS-RESTART SECTION.                                                     
022800     SKIP2                                                                
022900     MOVE SPACE                TO MSG-IO-AREA                             
023000     MOVE '  '                 TO GODK-STATUSKODER                        
023100     CALL CBLTDLI USING        XRST MSG-PCB                               
023200                               MSG-IO-AREA-LENGTH MSG-IO-AREA             
023300                               CHKP-AREA-LENGTH CHKP-AREA                 
023400     MOVE MSG-STATUS-CODE      TO STATUS-WS                               
023500     PERFORM IMS-STATUS-KONTROLL                                          
023600     .                                                                    
023700     EJECT                                                                
023800 IMS-CHECKPOINT SECTION.                                                  
023900     SKIP2                                                                
024000     MOVE SPACE TO MSG-IO-AREA                                            
024100     MOVE '  XD' TO GODK-STATUSKODER                                      
024200     CALL CBLTDLI USING CHKP MSG-PCB                                      
024300                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
024400                        CHKP-AREA-LENGTH CHKP-AREA                        
024500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024600     PERFORM IMS-STATUS-KONTROLL                                          
024700                                                                          
024800     IF IMS-EJ-OK                                                         
024900       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
025000       CALL FELLOG                                                        
025100     END-IF                                                               
025200     .                                                                    
025300     EJECT                                                                
025400 IMS-STATUS-KONTROLL SECTION.                                             
025500     SKIP2                                                                
025600     SET STATUS-IX TO 1                                                   
025700     SEARCH GODK-STATUS                                                   
025800       AT END CALL FELLOG                                                 
025900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
026000     END-SEARCH                                                           
026100     .                                                                    
