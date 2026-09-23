001100 ID DIVISION.                                                             
001200                                                                          
001300 PROGRAM-ID.     W3715900.                                                
001400 AUTHOR.         INGVAR SKJELBRED.                                        
001500 DATE-WRITTEN.   96/09/16.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNKTION:                                                            
002000*        FLAGGA BYTBEK UPPDATERAS MED STATUS M(MOTTAGEN) ELLER            
002100*        K (KLAR) DETTA INTRÄFFAR NÄR STATUS ÄR TRE ELLER FYRA            
002200*                                                                         
002310*        PROGRAMMET UPPDATERAR WLBYTF (WDM6)                              
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
003602*          --- NYTT RAPPORTSTATUS TILL WDM601                             
003610     SELECT W37159                     ASSIGN TO W37159D1.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004201     SKIP3                                                                
004202 FD  W37159                                                               
004203     RECORDING       F                                                    
004204     BLOCK CONTAINS  0.                                                   
004205                                                                          
004210*01  -COPY W37159      -L.                                                
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004501*    -- CHECKED BY WY2000                                                 
004510     SKIP3                                                                
004600 77  IDPGM                       PIC X(8)    VALUE 'W3715900'.            
004700 01  CHKP-VAR.                                                            
004800 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004900 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005000 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005100 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005200 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005300 03  CHKP-MAX                    PIC S9(3)   VALUE +500.                  
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005510 77  W-W37159-KVPOST-IN          PIC S9(7)   VALUE ZERO COMP-3.           
005600     SKIP2                                                                
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006101                                                                          
006102 77  W37159-EOF-SW               PIC X       VALUE 'N'.                   
006110     88  END-OF-W37159                       VALUE 'J'.                   
006400     EJECT                                                                
006500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006600 01  FILLER REDEFINES DAGENS-DATUM.                                       
006700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007000     EJECT                                                                
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200*                                                                         
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007501     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007510     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007601     EJECT                                                                
007602*    --- PARAMETRAR TILL POSTSUM                                          
007603*                                                                         
007610*01  -COPY W0005   -PRE  POSTSUM-                                         
007801     EJECT                                                                
007810*01  -COPY WDATAREA                                                       
007901     EJECT                                                                
007902 01  STATUS-AREA-START           PIC X(24)   VALUE                        
007903                                             'STATUS-AREA-START'.         
007904     SKIP2                                                                
007905                                                                          
007910*01  AREA -COPY W37159     -PRE STATUS-                                   
008000*                                                                         
008100     EJECT                                                                
008200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008300     SKIP3                                                                
008400 01  NYCKLAR-TILL-DLI.                                                    
008501     03  W-WDM601KY-X.                                                    
008503         05  W-IDDISTR           PIC S9(5)    VALUE ZERO COMP-3.          
008504         05  W-IDBYTRAP          PIC S9(7)    VALUE ZERO COMP-3.          
008600     SKIP2                                                                
008700*    --- STATUS-KOD FRÅN IMS                                              
008800 01  STATUS-WS                   PIC XX.                                  
008900     88  SEGMENT-FINNS                       VALUE '  '.                  
009000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009300     88  IMS-EJ-OK                           VALUE 'XD'.                  
009400     SKIP2                                                                
009500 01  GODK-STATUSKODER.                                                    
009600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009700     SKIP3                                                                
009800 01  SSA1                        PIC X(64).                               
009900 01  SSA2                        PIC X(64).                               
010000     EJECT                                                                
010100*    --- IMS FUNKTIONSKODER                                               
010200*01  -COPY W0003                                                          
010300     EJECT                                                                
010500*    ---  DLI INPUT-OUTPUT AREA                                           
010600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010700     SKIP3                                                                
010800 01  DLI-IO-AREA.                                                         
010900     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
011001     SKIP3                                                                
011002     03  WLBYTF01 REDEFINES IO-AREA.                                      
011003*        05  -COPY WDM601  -PRE BYTF-                                     
012000     EJECT                                                                
012100 LINKAGE SECTION.                                                         
012200                                                                          
012300*01  -COPY W0009   -PRE MSG-                                              
012401     EJECT                                                                
012402*01  -COPY W0008  -PRE BYTF-                                              
012410     05  FILLER                  PIC X.                                   
012700     EJECT                                                                
012801 PROCEDURE DIVISION  USING MSG-PCB BYTF-PCB.                              
012802 MAIN SECTION.                                                            
012810     ENTRY 'DLITCBL' USING MSG-PCB BYTF-PCB.                              
012900                                                                          
013100     SKIP2                                                                
013200     PERFORM A-INIT                                                       
013310     PERFORM S01-LAES-W37159                                              
013400     PERFORM UNTIL END-OF-W37159                                          
013500       IF CHKP-ANT > CHKP-MAX                                             
013600         PERFORM X-TAG-CHECKPOINT                                         
013700       END-IF                                                             
013800       PERFORM C-BEARBETA                                                 
014300                                                                          
014410       PERFORM S01-LAES-W37159                                            
014500     END-PERFORM                                                          
014600                                                                          
014800     PERFORM Z-FINIT                                                      
014900                                                                          
015000     MOVE ZERO TO RETURN-CODE                                             
015100     GOBACK                                                               
015200     .                                                                    
015300     EJECT                                                                
015400 A-INIT SECTION.                                                          
015500     SKIP2                                                                
015600                                                                          
015700     PERFORM IMS-RESTART                                                  
015901                                                                          
015910     OPEN INPUT W37159                                                    
016200                                                                          
016500                                                                          
016610     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016900     .                                                                    
017100     EJECT                                                                
017110 C-BEARBETA SECTION.                                                      
017120     SKIP2                                                                
017130                                                                          
017140     MOVE STATUS-IDDISTR   TO W-IDDISTR                                   
017141     MOVE STATUS-IDBYTRAP  TO W-IDBYTRAP                                  
017142     PERFORM IMS-GET-BYTF-WLBYTF01                                        
017143     IF SEGMENT-FINNS                                                     
017144        MOVE STATUS-KDBYTBEK TO BYTF-RAPP-KDBYTBEK                        
017145        PERFORM IMS-REPL-BYTF                                             
017146        ADD +1  TO  CHKP-ANT                                              
017147     END-IF                                                               
017180                                                                          
017191     .                                                                    
017192     EJECT                                                                
017200 Z-FINIT SECTION.                                                         
017300                                                                          
017701                                                                          
017710     CLOSE W37159                                                         
017901     SKIP2                                                                
017902     MOVE 'S' TO POSTSUM-OPKOD                                            
017910     CALL POSTSUM USING POSTSUM-PARM                                      
018100     .                                                                    
018201     EJECT                                                                
018202 S01-LAES-W37159  SECTION.                                                
018203     SKIP2                                                                
018204     READ W37159 INTO STATUS-AREA                                         
018205     AT END                                                               
018207        SET END-OF-W37159 TO TRUE                                         
018208                                                                          
018209     NOT AT END                                                           
018210        MOVE 'W37159' TO POSTSUM-FDNAMN                                   
018211        MOVE 'W37159D1' TO POSTSUM-DDNAMN2                                
018213        CALL POSTSUM USING POSTSUM-PARM                                   
018214                                                                          
018215        ADD 1 TO W-W37159-KVPOST-IN                                       
018216     END-READ                                                             
018220     .                                                                    
018500     EJECT                                                                
018600 X-TAG-CHECKPOINT   SECTION.                                              
018700                                                                          
018800* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
018900* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
019300     PERFORM IMS-CHECKPOINT                                               
019400     MOVE ZERO TO CHKP-ANT                                                
019500* --- LÄS OM DATABAS OM DET BEHÖVS                                        
019600     .                                                                    
019700     EJECT                                                                
019800* --- IMS SEKTIONER ---                                                   
019900     SKIP3                                                                
020001     EJECT                                                                
020002 IMS-GET-BYTF-WLBYTF01 SECTION.                                           
020003                                                                          
020004     STRING 'WLBYTF01(WDM601KY =' W-WDM601KY-X ')'                        
020005          DELIMITED BY SIZE INTO SSA1                                     
020006     MOVE '  GE' TO GODK-STATUSKODER                                      
020007     CALL CBLTDLI USING GHU BYTF-PCB DLI-IO-AREA SSA1                     
020008     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
020009     PERFORM IMS-STATUSKONTROLL                                           
020010     .                                                                    
020011     EJECT                                                                
020022 IMS-REPL-BYTF SECTION.                                                   
020023                                                                          
020024     MOVE '  ' TO GODK-STATUSKODER                                        
020025     CALL CBLTDLI USING REPL BYTF-PCB DLI-IO-AREA                         
020026     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
020027     PERFORM IMS-STATUSKONTROLL                                           
020030     .                                                                    
020100     EJECT                                                                
020200 IMS-RESTART SECTION.                                                     
020300     SKIP2                                                                
020400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020500     MOVE '  ' TO GODK-STATUSKODER                                        
020600     CALL CBLTDLI USING XRST MSG-PCB                                      
020700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020800                        CHKP-AREA-LENGTH CHKP-AREA                        
020900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021000     PERFORM IMS-STATUSKONTROLL                                           
021100     .                                                                    
021200     EJECT                                                                
021300 IMS-CHECKPOINT SECTION.                                                  
021400     SKIP2                                                                
021500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021600     MOVE '  XD' TO GODK-STATUSKODER                                      
021700     CALL CBLTDLI USING CHKP MSG-PCB                                      
021800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021900                        CHKP-AREA-LENGTH CHKP-AREA                        
022000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022100     PERFORM IMS-STATUSKONTROLL                                           
022200                                                                          
022300     IF IMS-EJ-OK                                                         
022400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
022500       DISPLAY FELTEXT                                                    
022600       CALL FELLOG                                                        
022700     END-IF                                                               
022800     .                                                                    
022900     EJECT                                                                
023000 IMS-STATUSKONTROLL SECTION.                                              
023100     SKIP2                                                                
023200     SET STATUS-IX TO 1                                                   
023300     SEARCH GODK-STATUS                                                   
023400       AT END                                                             
023500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
023600           DELIMITED BY SIZE INTO FELTEXT                                 
023700         DISPLAY FELTEXT                                                  
023800         CALL FELLOG                                                      
023900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
024000         CONTINUE                                                         
024100     END-SEARCH                                                           
024200     .                                                                    
