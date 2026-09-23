000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3350100.                                                
000300 AUTHOR.         RONNY STENHOLM.                                          
000400 DATE-WRITTEN.   93/10/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDAT WLPRIA MED MARKNADSBOLAGS SUGGESTED RETAIL PRISER.        
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WLPRIA (WDC1)                              
001100*                                                                         
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- FIL MED PRISER FRÅN MARKNADSBOLAGEN                        
002100     SELECT W33501                     ASSIGN TO W33501D1.                
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400     SKIP3                                                                
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002700 FD  W33501                                                               
002800     RECORDING       V                                                    
002900     BLOCK CONTAINS  0.                                                   
003000     SKIP2                                                                
003100*01  -COPY W335011B    -L.                                                
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(8)    VALUE 'W3350100'.            
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000 01  CHKP-VAR.                                                            
004100 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004200 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004300 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004400 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004500 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004600 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
004700     SKIP2                                                                
004800 01  FELTEXT.                                                             
004900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005100                                                                          
005200 77  W33501-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W33501                       VALUE 'J'.                   
005400     EJECT                                                                
005500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES DAGENS-DATUM.                                       
005700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006000     EJECT                                                                
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL ABEND                                            
006800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007000*    --- PARAMETRAR TILL POSTSUM                                          
007100*                                                                         
007200*01  -COPY W0005   -PRE  POSTSUM-                                         
007300     EJECT                                                                
007400 01  PRIS-AREA-START             PIC X(24)   VALUE                        
007500                                             'PRIS-AREA-START'.           
007600     SKIP2                                                                
007700                                                                          
007800*01  AREA -COPY W335011B   -PRE PRIS-                                     
007900*                                                                         
008000     EJECT                                                                
008100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008200     SKIP3                                                                
008300 01  NYCKLAR-TILL-DLI.                                                    
008400     03  W-WDC101KY-X.                                                    
008500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008600         05  W-IDMARKBO          PIC X       VALUE SPACE.                 
008700     SKIP2                                                                
008800*    --- STATUS-KOD FRÅN IMS                                              
008900 01  STATUS-WS                   PIC XX.                                  
009000     88  SEGMENT-FINNS                       VALUE '  '.                  
009100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009400     88  IMS-EJ-OK                           VALUE 'XD'.                  
009500     SKIP2                                                                
009600 01  GODK-STATUSKODER.                                                    
009700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009800     SKIP3                                                                
009900 01  SSA1                        PIC X(64).                               
010000 01  SSA2                        PIC X(64).                               
010100     EJECT                                                                
010200*    --- IMS FUNKTIONSKODER                                               
010300*01  -COPY W0003                                                          
010400     EJECT                                                                
010500*    ---  DLI INPUT-OUTPUT AREA                                           
010600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010700                                                                          
010800 01  DLI-IO-AREA.                                                         
010900     03  WLPRIA01.                                                        
011000*        05  -COPY WDC101  -PRE PRIA-                                     
011100     EJECT                                                                
011200 LINKAGE SECTION.                                                         
011300*01  -COPY W0009   -PRE MSG-                                              
011400     EJECT                                                                
011500*01  -COPY W0008  -PRE PRIA-                                              
011600     05  FILLER                  PIC X.                                   
011700     EJECT                                                                
011800 PROCEDURE DIVISION  USING MSG-PCB PRIA-PCB.                              
011900 MAIN SECTION.                                                            
012000     ENTRY 'DLITCBL' USING MSG-PCB PRIA-PCB.                              
012100                                                                          
012200     PERFORM A-INIT                                                       
012300     PERFORM S01-LAES-W33501                                              
012400     PERFORM UNTIL END-OF-W33501                                          
012500       IF CHKP-ANT > CHKP-MAX                                             
012600         PERFORM X-TAG-CHECKPOINT                                         
012700       END-IF                                                             
012800       IF PRIS-PRARTBTO-MARK > ZERO                                       
012900        IF PRIS-IDARTNR > ZERO                                            
013000         PERFORM B-UPPDATERA-PRIS                                         
013100         ADD  +1 TO CHKP-ANT                                              
013200        END-IF                                                            
013300       ELSE                                                               
013400         DISPLAY 'ARTIKELNR MED PRIS NOLL ' PRIS-IDARTNR                  
013500*        PERFORM S99-ABEND                                                
013600       END-IF                                                             
013700       PERFORM S01-LAES-W33501                                            
013800     END-PERFORM                                                          
013900     PERFORM Z-FINIT                                                      
014000     MOVE ZERO TO RETURN-CODE                                             
014100     GOBACK                                                               
014200     .                                                                    
014300     EJECT                                                                
014400 A-INIT SECTION.                                                          
014500                                                                          
014600     PERFORM IMS-RESTART                                                  
014700     OPEN INPUT W33501                                                    
014800                                                                          
014900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015000     ACCEPT DAGENS-DATUM FROM DATE                                        
015100     .                                                                    
015200     EJECT                                                                
015300 B-UPPDATERA-PRIS SECTION.                                                
015400                                                                          
015500     MOVE PRIS-IDMARKBO TO W-IDMARKBO                                     
015600     MOVE PRIS-IDARTNR TO W-IDARTNR                                       
015700     PERFORM IMS-GET-PRIA-PRIS                                            
015800     IF PRIS-IDMARKBO = 'A' OR 'B' OR 'C' OR 'E' OR 'F'                   
015900                            OR 'X' OR 'Y'                                 
016000       IF SEGMENT-FINNS                                                   
016100         PERFORM BA-JUSTERA-PRIS-INFO                                     
016200       ELSE                                                               
016300         PERFORM BB-NY-PRIS-INFO                                          
016400       END-IF                                                             
016500     ELSE                                                                 
016600       DISPLAY 'FEL MARKNADSBOLAGSKOD>' PRIS-IDMARKBO '<'                 
016700*      PERFORM S99-ABEND                                                  
016800     END-IF                                                               
016900     .                                                                    
017000     EJECT                                                                
017100 BA-JUSTERA-PRIS-INFO SECTION.                                            
017200                                                                          
017300     MOVE PRIS-KDRABATT TO PRIA-ART-KDARTRAB                              
017400     IF PRIS-PRARTBTO-MARK NOT = PRIA-ART-PRARTBTO-MARK                   
017500       MOVE DAGENS-DATUM  TO PRIA-ART-TIUPPDAT                            
017600     END-IF                                                               
017700     MOVE PRIS-PRARTBTO-MARK TO PRIA-ART-PRARTBTO-MARK                    
017800     MOVE PRIS-KDARTKAM TO PRIA-ART-KDARTKAM                              
017810     IF PRIS-KDARTRAB-ALT IS NUMERIC                                      
017820       MOVE PRIS-KDARTRAB-ALT TO PRIA-ART-KDARTRAB-ALT                    
017830     ELSE                                                                 
017831       MOVE ZEROES            TO PRIA-ART-KDARTRAB-ALT                    
017840     END-IF                                                               
017900     PERFORM IMS-REPL-PRIA-PRIS                                           
018000     .                                                                    
018100 BB-NY-PRIS-INFO      SECTION.                                            
018200                                                                          
018300     MOVE PRIS-IDMARKBO TO PRIA-ART-IDMARKBO                              
018400     MOVE PRIS-IDARTNR  TO PRIA-ART-IDARTNR                               
018500     MOVE PRIS-KDRABATT TO PRIA-ART-KDARTRAB                              
018600     MOVE PRIS-PRARTBTO-MARK TO PRIA-ART-PRARTBTO-MARK                    
018700     MOVE PRIS-KDARTKAM TO PRIA-ART-KDARTKAM                              
018800     MOVE DAGENS-DATUM  TO PRIA-ART-TIUPPDAT                              
018910     IF PRIS-KDARTRAB-ALT IS NUMERIC                                      
018920       MOVE PRIS-KDARTRAB-ALT TO PRIA-ART-KDARTRAB-ALT                    
018930     ELSE                                                                 
018940       MOVE ZEROES            TO PRIA-ART-KDARTRAB-ALT                    
018950     END-IF                                                               
019000     PERFORM IMS-ISRT-PRIA-PRIS                                           
019100     .                                                                    
019200     EJECT                                                                
019300 X-TAG-CHECKPOINT   SECTION.                                              
019400                                                                          
019500     PERFORM IMS-CHECKPOINT                                               
019600     MOVE ZERO TO CHKP-ANT                                                
019700     .                                                                    
019800     EJECT                                                                
019900* --- IMS SEKTIONER ---                                                   
020000 Z-FINIT SECTION.                                                         
020100                                                                          
020200     CLOSE W33501                                                         
020300                                                                          
020400     MOVE 'S' TO POSTSUM-OPKOD                                            
020500     CALL POSTSUM USING POSTSUM-PARM                                      
020600     .                                                                    
020700     EJECT                                                                
020800 S01-LAES-W33501  SECTION.                                                
020900                                                                          
020910     INITIALIZE PRIS-KDARTRAB-ALT                                         
021000     READ W33501 INTO PRIS-AREA                                           
021100     AT END                                                               
021200        SET END-OF-W33501 TO TRUE                                         
021300                                                                          
021400     NOT AT END                                                           
021500        MOVE 'W33501' TO POSTSUM-FDNAMN                                   
021600        MOVE 'W33501D1' TO POSTSUM-DDNAMN2                                
021700        MOVE 'PRIS'      TO POSTSUM-TRANSTYP                              
021800        CALL POSTSUM USING POSTSUM-PARM                                   
021900     END-READ                                                             
022000     .                                                                    
022100 S99-ABEND SECTION.                                                       
022200                                                                          
022300     SKIP2                                                                
022400     MOVE 'S' TO POSTSUM-OPKOD                                            
022500     CALL POSTSUM USING POSTSUM-PARM                                      
022600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
022700     .                                                                    
022800     EJECT                                                                
022900* --- IMS SEKTIONER ---                                                   
023000                                                                          
023100 IMS-GET-PRIA-PRIS SECTION.                                               
023200     STRING 'WLPRIA01(WDC101KY =' W-WDC101KY-X ')'                        
023300          DELIMITED BY SIZE INTO SSA1                                     
023400     MOVE '  GE' TO GODK-STATUSKODER                                      
023500     CALL CBLTDLI USING GHU PRIA-PCB DLI-IO-AREA SSA1                     
023600     MOVE PRIA-STATUS-CODE TO STATUS-WS                                   
023700     PERFORM IMS-STATUSKONTROLL                                           
023800     .                                                                    
023900     SKIP3                                                                
024000 IMS-ISRT-PRIA-PRIS SECTION.                                              
024100                                                                          
024200     MOVE 'WLPRIA01 ' TO SSA1                                             
024300     MOVE '  II' TO GODK-STATUSKODER                                      
024400     CALL CBLTDLI USING ISRT PRIA-PCB DLI-IO-AREA SSA1                    
024500     MOVE PRIA-STATUS-CODE TO STATUS-WS                                   
024600     PERFORM IMS-STATUSKONTROLL                                           
024700     .                                                                    
024800     SKIP3                                                                
024900 IMS-REPL-PRIA-PRIS SECTION.                                              
025000                                                                          
025100     MOVE '  ' TO GODK-STATUSKODER                                        
025200     CALL CBLTDLI USING REPL PRIA-PCB DLI-IO-AREA                         
025300     MOVE PRIA-STATUS-CODE TO STATUS-WS                                   
025400     PERFORM IMS-STATUSKONTROLL                                           
025500     .                                                                    
025600     EJECT                                                                
025700 IMS-RESTART SECTION.                                                     
025800                                                                          
025900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
026000     MOVE '  ' TO GODK-STATUSKODER                                        
026100     CALL CBLTDLI USING XRST MSG-PCB                                      
026200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
026300                        CHKP-AREA-LENGTH CHKP-AREA                        
026400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026500     PERFORM IMS-STATUSKONTROLL                                           
026600     .                                                                    
026700     SKIP3                                                                
026800 IMS-CHECKPOINT SECTION.                                                  
026900                                                                          
027000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
027100     MOVE '  XD' TO GODK-STATUSKODER                                      
027200     CALL CBLTDLI USING CHKP MSG-PCB                                      
027300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027400                        CHKP-AREA-LENGTH CHKP-AREA                        
027500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027600     PERFORM IMS-STATUSKONTROLL                                           
027700                                                                          
027800     IF IMS-EJ-OK                                                         
027900       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
028000       DISPLAY FELTEXT                                                    
028100       CALL FELLOG                                                        
028200     END-IF                                                               
028300     .                                                                    
028400     EJECT                                                                
028500 IMS-STATUSKONTROLL SECTION.                                              
028600                                                                          
028700     SET STATUS-IX TO 1                                                   
028800     SEARCH GODK-STATUS                                                   
028900       AT END                                                             
029000         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
029100         DISPLAY FELTEXT                                                  
029200         CALL FELLOG                                                      
029300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
029400         CONTINUE                                                         
029500     END-SEARCH                                                           
029600     .                                                                    
