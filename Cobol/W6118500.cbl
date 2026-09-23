000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6118500.                                                
000400*AUTHOR.         LARS THELL.                                              
000500*DATE-WRITTEN.   92/08/12.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        RENSAR POSTER PÅ W6G3                                            
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR W6FILA (W6G3)                              
001300*                                                                         
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- FIL MED NYCKLAR TILL DE POSTER SOM SKALL RENSAS            
002400     SELECT W61185                     ASSIGN TO W61185D1.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W61185                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300     SKIP2                                                                
003400*01  -COPY W6118401      -L.                                              
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700     SKIP2                                                                
003701                                                                          
003710*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(8)    VALUE 'W6118500'.            
003900 01  CHKP-VAR.                                                            
004000 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004100 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004200 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004300 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004400 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004500 03  CHKP-MAX                    PIC S9(3)   VALUE +200.                  
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800     SKIP2                                                                
004900 01  FELTEXT.                                                             
005000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005200                                                                          
005300 77  W61185-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W61185                       VALUE 'J'.                   
005500     EJECT                                                                
005600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005700 01  FILLER REDEFINES DAGENS-DATUM.                                       
005800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006100     EJECT                                                                
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300*                                                                         
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006700     EJECT                                                                
006800*    --- PARAMETRAR TILL POSTSUM                                          
006900*                                                                         
007000*01  -COPY W0005   -PRE  POSTSUM-                                         
007100     EJECT                                                                
007200 01  IN-AREA-START               PIC X(24)   VALUE                        
007300                                             'IN-AREA-START'.             
007400     SKIP2                                                                
007500                                                                          
007600*01  AREA -COPY W6118401     -PRE IN-                                     
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008200     03  W-W6G301KY-X.                                                    
008300         05  W-G301KY-IDPGM      PIC X(8)     VALUE SPACE.                
008400         05  W-G301KY-TIREGDAT   PIC S9(7)    COMP-3 VALUE ZERO.          
008500         05  W-G301KY-TIKLOCK    PIC S9(9)    COMP-3 VALUE ZERO.          
008600         05  W-G301KY-IDSEKVNR   PIC S9(3)    COMP-3 VALUE ZERO.          
008700         05  W-G301KY-IDCPYTXT   PIC X(8)     VALUE SPACE.                
008800                                                                          
008900     SKIP2                                                                
009000*    --- STATUS-KOD FRÅN IMS                                              
009100 01  STATUS-WS                   PIC XX.                                  
009200     88  SEGMENT-FINNS                       VALUE '  '.                  
009300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009600     88  IMS-EJ-OK                           VALUE 'XD'.                  
009700     SKIP2                                                                
009800 01  GODK-STATUSKODER.                                                    
009900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010000     SKIP3                                                                
010100 01  SSA1                        PIC X(64).                               
010200 01  SSA2                        PIC X(64).                               
010300     EJECT                                                                
010400*    --- IMS FUNKTIONSKODER                                               
010500*01  -COPY W0003                                                          
010600     EJECT                                                                
010700*    ---  DLI INPUT-OUTPUT AREA                                           
010800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010900     SKIP3                                                                
011000 01  DLI-IO-AREA.                                                         
011100     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
011200     SKIP3                                                                
011300     03  W6FILA01 REDEFINES IO-AREA.                                      
011400*        05  -COPY W6G301  -PRE FILA-                                     
011500 LINKAGE SECTION.                                                         
011600                                                                          
011700*01  -COPY W0009   -PRE MSG-                                              
011800     EJECT                                                                
011900*01  -COPY W0008  -PRE FILA-                                              
012000     05  FILLER                  PIC X.                                   
012100     EJECT                                                                
012200 PROCEDURE DIVISION  USING MSG-PCB FILA-PCB.                              
012300     ENTRY 'DLITCBL' USING MSG-PCB FILA-PCB.                              
012400                                                                          
012500     SKIP2                                                                
012600     PERFORM A-INIT                                                       
012700     PERFORM S01-LAES-W61185                                              
012800     PERFORM UNTIL END-OF-W61185                                          
012900         IF CHKP-ANT > CHKP-MAX                                           
013000             PERFORM X-TAG-CHECKPOINT                                     
013100             MOVE ZERO          TO CHKP-ANT                               
013200         END-IF                                                           
013300         PERFORM B-TA-BORT-FILA01                                         
013400         PERFORM S01-LAES-W61185                                          
013500     END-PERFORM                                                          
013600                                                                          
013700     PERFORM Z-FINIT                                                      
013800                                                                          
013900     MOVE ZERO TO RETURN-CODE                                             
014000     GOBACK                                                               
014100     .                                                                    
014200     EJECT                                                                
014300 A-INIT SECTION.                                                          
014400                                                                          
014500     PERFORM IMS-RESTART                                                  
014600                                                                          
014700     OPEN INPUT W61185                                                    
014800                                                                          
014900     ACCEPT DAGENS-DATUM       FROM DATE                                  
015000                                                                          
015100     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
015200     .                                                                    
015300     EJECT                                                                
015400 B-TA-BORT-FILA01 SECTION.                                                
015500                                                                          
015600     ADD +1                    TO CHKP-ANT                                
015700     MOVE IN-IDPGM             TO W-G301KY-IDPGM                          
015800     MOVE IN-TIREGDAT          TO W-G301KY-TIREGDAT                       
015900     MOVE IN-TIKLOCK           TO W-G301KY-TIKLOCK                        
016000     MOVE IN-IDSEKVNR          TO W-G301KY-IDSEKVNR                       
016100     MOVE IN-IDCPYTXT          TO W-G301KY-IDCPYTXT                       
016200                                                                          
016300     PERFORM IMS-GHU-FILA-FILA01                                          
016400     IF SEGMENT-FINNS                                                     
016500         PERFORM IMS-DLET-FILA-FILA01                                     
016600     END-IF                                                               
016700     .                                                                    
016800     EJECT                                                                
016900 Z-FINIT SECTION.                                                         
017000                                                                          
017100     CLOSE W61185                                                         
017200     SKIP2                                                                
017300     MOVE 'S' TO POSTSUM-OPKOD                                            
017400     CALL POSTSUM USING POSTSUM-PARM                                      
017500     .                                                                    
017600     EJECT                                                                
017700 S01-LAES-W61185  SECTION.                                                
017800     SKIP2                                                                
017900     READ W61185 INTO IN-AREA                                             
018000     AT END                                                               
018100        SET END-OF-W61185 TO TRUE                                         
018200                                                                          
018300     NOT AT END                                                           
018400        MOVE 'W61185' TO POSTSUM-FDNAMN                                   
018500        MOVE 'W61185D1' TO POSTSUM-DDNAMN2                                
018600        CALL POSTSUM USING POSTSUM-PARM                                   
018700                                                                          
018800     END-READ                                                             
018900     .                                                                    
019000     EJECT                                                                
019100 X-TAG-CHECKPOINT   SECTION.                                              
019200                                                                          
019300     PERFORM IMS-CHECKPOINT                                               
019400     .                                                                    
019500     EJECT                                                                
019600* --- IMS SEKTIONER ---                                                   
019700     SKIP3                                                                
019800 IMS-GHU-FILA-FILA01 SECTION.                                             
019900     STRING 'W6FILA01(W6G301KY =' W-W6G301KY-X ')'                        
020000          DELIMITED BY SIZE INTO SSA1                                     
020100     MOVE '  GE' TO GODK-STATUSKODER                                      
020200     CALL CBLTDLI USING GHU FILA-PCB DLI-IO-AREA SSA1                     
020300     MOVE FILA-STATUS-CODE TO STATUS-WS                                   
020400     PERFORM IMS-STATUSKONTROLL                                           
020500     .                                                                    
020600     SKIP3                                                                
020700 IMS-DLET-FILA-FILA01 SECTION.                                            
020800                                                                          
020900     MOVE '  ' TO GODK-STATUSKODER                                        
021000     CALL CBLTDLI USING DLET FILA-PCB DLI-IO-AREA                         
021100     MOVE FILA-STATUS-CODE TO STATUS-WS                                   
021200     PERFORM IMS-STATUSKONTROLL                                           
021300     .                                                                    
021400     EJECT                                                                
021500 IMS-RESTART SECTION.                                                     
021600     SKIP2                                                                
021700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021800     MOVE '  ' TO GODK-STATUSKODER                                        
021900     CALL CBLTDLI USING XRST MSG-PCB                                      
022000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
022100                        CHKP-AREA-LENGTH CHKP-AREA                        
022200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022300     PERFORM IMS-STATUSKONTROLL                                           
022400     .                                                                    
022500     EJECT                                                                
022600 IMS-CHECKPOINT SECTION.                                                  
022700     SKIP2                                                                
022800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
022900     MOVE '  XD' TO GODK-STATUSKODER                                      
023000     CALL CBLTDLI USING CHKP MSG-PCB                                      
023100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
023200                        CHKP-AREA-LENGTH CHKP-AREA                        
023300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
023400     PERFORM IMS-STATUSKONTROLL                                           
023500                                                                          
023600     IF IMS-EJ-OK                                                         
023700       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
023800       DISPLAY FELTEXT                                                    
023900       CALL FELLOG                                                        
024000     END-IF                                                               
024100     .                                                                    
024200     EJECT                                                                
024300 IMS-STATUSKONTROLL SECTION.                                              
024400     SKIP2                                                                
024500     SET STATUS-IX TO 1                                                   
024600     SEARCH GODK-STATUS                                                   
024700       AT END                                                             
024800         MOVE 'FEL VID DL1 ANROP' TO FELTEXT-STR                          
024900         DISPLAY FELTEXT                                                  
025000         CALL FELLOG                                                      
025100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
025200         CONTINUE                                                         
025300     END-SEARCH                                                           
025400     .                                                                    
