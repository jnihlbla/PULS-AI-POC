000100 ID DIVISION.                                                             
000300 PROGRAM-ID.        W1227500.                                             
000400 AUTHOR.            CONNY EGHOLT.                                         
000500 DATE-WRITTEN.      92/11/17.                                             
000510 DATE-COMPILED.                                                           
000600*REWRITTEN TO BMP   93/12/06.                                             
000620                                                                          
000800*        CHECKPOINT EVERY 5 UPDATES BECAUSE OF POSSIBLE DELETIONS         
000900*        OF UP TO ESTIMATED 20 CHILDREN SEGMENTS.                         
001000*                                                                         
001300*    FUNKTION:                                                            
001400*        LÄSER INFIL W12251 MED ARTIKLAR SOM ÄR RENSADE.                  
001500*        DESSA ARTIKLAR TAS BORT PÅ WDD2.                                 
002300*                                                                         
002400*        PROGRAMMET UPPDATERAR WLARTG (WDD2)                              
002540*                                                                         
002600*    ABENDKODER:                                                          
002700*        U0016 -  . . . .                                                 
002800*        U1000 -  . . . .                                                 
002900*                                                                         
003000                                                                          
003200 ENVIRONMENT DIVISION.                                                    
003300                                                                          
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003700     SKIP2                                                                
003800*          --- FIL MED RENSADE ARTIKLAR                                   
003900     SELECT W12251                     ASSIGN TO W12275D1.                
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP3                                                                
004300 FILE SECTION.                                                            
004400     SKIP3                                                                
004500 FD  W12251                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800     SKIP2                                                                
004900*01  POST  -COPY W12251 -PRE W12251-  -L.                                 
005000     EJECT                                                                
005100 WORKING-STORAGE SECTION.                                                 
005200     SKIP2                                                                
005201                                                                          
005210*    -- CHECKED BY WY2000                                                 
005300 77  IDPGM                       PIC X(8)    VALUE 'W1227500'.            
005301 01  CHKP-VAR.                                                            
005302 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005303 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005304 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005305 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005306 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005310 03  CHKP-MAX                    PIC S9(3)   VALUE +5.                    
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005500 77  W-DLET-WDD201               PIC 9(7)    VALUE ZERO.                  
005700 77  W12251-EOF-SW               PIC X       VALUE 'N'.                   
005800     88  END-OF-W12251                       VALUE 'J'.                   
005900                                                                          
006000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES DAGENS-DATUM.                                       
006200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006500     EJECT                                                                
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007200     SKIP2                                                                
007300*    --- PARAMETRAR TILL ABEND                                            
007400                                                                          
007500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007700     SKIP2                                                                
007800 01  FELTEXT.                                                             
007900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL POSTSUM                                          
008400*01  -COPY W0005   -PRE  POSTSUM-                                         
008500     EJECT                                                                
008700 01  IN-AREA-START               PIC X(16)   VALUE  'IN-AREA'.            
009000*01  W12251-AREA -COPY W12251     -PRE IN-                                
009300     EJECT                                                                
009400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009800     SKIP3                                                                
009900 01  NYCKLAR-TILL-DLI.                                                    
010000     03  W-IDARTNR-X.                                                     
010100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010200     SKIP2                                                                
010300*    --- STATUS-KOD FRÅN IMS                                              
010400 01  STATUS-WS                   PIC XX.                                  
010500     88  SEGMENT-FINNS                       VALUE '  '.                  
010600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010701     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010710     88  IMS-EJ-OK                           VALUE 'XD'.                  
010800     SKIP2                                                                
010900 01  GODK-STATUSKODER.                                                    
011000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011100     SKIP3                                                                
011200 01  SSA1                        PIC X(64).                               
011300 01  SSA2                        PIC X(64).                               
011400     EJECT                                                                
011500*    --- IMS FUNKTIONSKODER                                               
011600*01  -COPY W0003                                                          
011700     EJECT                                                                
011800*    ---  DLI INPUT-OUTPUT AREA                                           
011900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012000     SKIP3                                                                
012100 01  DLI-IO-AREA.                                                         
012200     03  IO-AREA                 PIC X(550)  VALUE SPACE.                 
012300     SKIP3                                                                
012400     03  WLARTG01 REDEFINES IO-AREA.                                      
012500*        05  -COPY WDD201  -PRE ARTG-                                     
012600     EJECT                                                                
012700 LINKAGE SECTION.                                                         
012901*01  -COPY W0009   -PRE MSG-                                              
012910                                                                          
013000*01  -COPY W0008  -PRE ARTG-                                              
013100     05  FILLER                  PIC X.                                   
013200     EJECT                                                                
013300 PROCEDURE DIVISION  USING MSG-PCB ARTG-PCB.                              
013310 MAIN SECTION.                                                            
013400     ENTRY 'DLITCBL' USING MSG-PCB ARTG-PCB.                              
013500                                                                          
013600     PERFORM A-INIT                                                       
013700     PERFORM S01-LAES-W12251                                              
013810                                                                          
013900     PERFORM UNTIL END-OF-W12251                                          
014000       MOVE IN-IDARTNR TO W-IDARTNR                                       
014010       IF CHKP-ANT > CHKP-MAX                                             
014020         PERFORM X-TAG-CHECKPOINT                                         
014030       END-IF                                                             
014100       PERFORM IMS-GHU-ARTG01                                             
014200       IF SEGMENT-FINNS                                                   
015000          PERFORM IMS-DLET-ARTG01                                         
                ADD 1 TO W-DLET-WDD201                                          
015010          ADD +1 TO CHKP-ANT                                              
015801       END-IF                                                             
015900       PERFORM S01-LAES-W12251                                            
016000     END-PERFORM                                                          
016100                                                                          
016200     PERFORM Z-FINIT                                                      
016300                                                                          
016400     MOVE ZERO TO RETURN-CODE                                             
016500     GOBACK                                                               
016600     .                                                                    
016700     EJECT                                                                
016800 A-INIT SECTION.                                                          
016900                                                                          
016910     PERFORM IMS-RESTART                                                  
017000     OPEN INPUT W12251                                                    
017100                                                                          
017200     ACCEPT DAGENS-DATUM  FROM DATE                                       
017300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017400     .                                                                    
017500     EJECT                                                                
017501 X-TAG-CHECKPOINT   SECTION.                                              
017502                                                                          
017505     PERFORM IMS-CHECKPOINT                                               
017506     MOVE ZERO TO CHKP-ANT                                                
017508     .                                                                    
017510     EJECT                                                                
017600 Z-FINIT SECTION.                                                         
017610                                                                          
           DISPLAY 'ANTAL BORTTAGNA WDD201: ' W-DLET-WDD201                     
017700     CLOSE W12251                                                         
017900     MOVE 'S' TO POSTSUM-OPKOD                                            
018000     CALL POSTSUM USING POSTSUM-PARM                                      
018100     .                                                                    
018200     EJECT                                                                
018300 S01-LAES-W12251  SECTION.                                                
018400                                                                          
018500     READ W12251 INTO IN-W12251-AREA                                      
018600     AT END                                                               
018700        SET END-OF-W12251 TO TRUE                                         
018800                                                                          
018900     NOT AT END                                                           
019000        MOVE 'W12251' TO POSTSUM-FDNAMN                                   
019100        MOVE 'W12275D1' TO POSTSUM-DDNAMN2                                
019200        CALL POSTSUM USING POSTSUM-PARM                                   
019300     END-READ                                                             
019400     .                                                                    
019500     EJECT                                                                
020400* --- IMS SEKTIONER ---                                                   
020500     SKIP3                                                                
020600     EJECT                                                                
020700 IMS-GHU-ARTG01    SECTION.                                               
020800     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
020900          DELIMITED BY SIZE INTO SSA1                                     
021000     MOVE '  GE' TO GODK-STATUSKODER                                      
021100     CALL CBLTDLI USING GHU ARTG-PCB DLI-IO-AREA SSA1                     
021200     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
021300     PERFORM IMS-STATUSKONTROLL                                           
021400     .                                                                    
021500     EJECT                                                                
021600 IMS-DLET-ARTG01   SECTION.                                               
021700     MOVE '  ' TO GODK-STATUSKODER                                        
021800     CALL CBLTDLI USING DLET ARTG-PCB DLI-IO-AREA                         
021900     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
022000     PERFORM IMS-STATUSKONTROLL                                           
022100     .                                                                    
022200     EJECT                                                                
023001 IMS-RESTART SECTION.                                                     
023002     SKIP2                                                                
023003     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
023004     MOVE '  ' TO GODK-STATUSKODER                                        
023005     CALL CBLTDLI USING XRST MSG-PCB                                      
023006                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
023007                        CHKP-AREA-LENGTH CHKP-AREA                        
023008     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
023009     PERFORM IMS-STATUSKONTROLL                                           
023010     .                                                                    
023011     EJECT                                                                
023012 IMS-CHECKPOINT SECTION.                                                  
023013     SKIP2                                                                
023014     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
023015     MOVE '  XD' TO GODK-STATUSKODER                                      
023016     CALL CBLTDLI USING CHKP MSG-PCB                                      
023017                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
023018                        CHKP-AREA-LENGTH CHKP-AREA                        
023019     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
023020     PERFORM IMS-STATUSKONTROLL                                           
023021                                                                          
023022     IF IMS-EJ-OK                                                         
023023       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
023024       DISPLAY FELTEXT                                                    
023025       CALL FELLOG                                                        
023026     END-IF                                                               
023027     .                                                                    
023030     EJECT                                                                
023100 IMS-STATUSKONTROLL SECTION.                                              
023200     SKIP2                                                                
023300     SET STATUS-IX TO 1                                                   
023400     SEARCH GODK-STATUS                                                   
023500       AT END                                                             
023600         CALL FELLOG                                                      
023700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
023710         CONTINUE                                                         
023800     END-SEARCH                                                           
023900     .                                                                    
