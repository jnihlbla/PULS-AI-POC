001100 ID DIVISION.                                                             
001200                                                                          
001300 PROGRAM-ID.     W6119300.                                                
001400 AUTHOR.         EVA LUNDELL.                                             
001500 DATE-WRITTEN.   96/08/06.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNKTION:                                                            
002000*        PROGRAMMET RENSAR LOGGPOSTER FRÅN W6L2. POSTER SOM               
002100*        INTE FINNS KVAR PÅ W6D1 RENSAS                                   
002200*                                                                         
002301*        PROGRAMMET UPPDATERAR W6UPPF (W6L2)                              
002310*        PROGRAMMET LÄSER      W6INLE (W6D1)                              
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
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP3                                                                
004100 FILE SECTION.                                                            
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004501                                                                          
004510*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W6119300'.            
004700 01  CHKP-VAR.                                                            
004800  03  CHKP-MSG-IO-AREA-LENGTH    PIC S9(9)   VALUE +32 COMP SYNC.         
004900  03  CHKP-MSG-IO-AREA           PIC X(32)   VALUE SPACE.                 
005000  03  CHKP-AREA-LENGTH           PIC S9(9)   VALUE +32 COMP SYNC.         
005100  03  CHKP-AREA                  PIC X(32)   VALUE SPACE.                 
005200  03  CHKP-ANT                   PIC S9(3)   VALUE +0.                    
005300  03  CHKP-MAX                   PIC S9(3)   VALUE +200.                  
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005510 01  ARBETSAREOR.                                                         
005520  03  SPARA-IDLOPNRM             PIC S9(9)   COMP-3 VALUE ZERO.           
005530  03  TEST-SEGMENT-FINNS         PIC X(2)    VALUE '  '.                  
005540  03  TEST-SEGMENT-SAKNAS        PIC X(2)    VALUE 'GE'.                  
005550  03  TEST-SEGMENT-SLUT          PIC X(2)    VALUE 'GB'.                  
005600     SKIP2                                                                
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006400     EJECT                                                                
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200*                                                                         
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007801     EJECT                                                                
008200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008300     SKIP3                                                                
008400 01  NYCKLAR-TILL-DLI.                                                    
008503     03  W-W6D1B1KY-X.                                                    
008504         05  W-IDLOPNRM-INLB     PIC S9(9)  COMP-3 VALUE ZERO.            
008511                                                                          
008520     03  W-W6L201KY-X.                                                    
008521         05  W-IDLOPNRM-UPFB     PIC S9(9)   VALUE ZERO COMP-3.           
008522         05  W-IDRADNR-UPFB      PIC S9(5)   VALUE ZERO COMP-3.           
008523         05  W-DAREGDAT-UPFB     PIC  9(8)   VALUE ZERO.                  
008524         05  W-TIKLOCK-UPFB      PIC S9(9)   VALUE ZERO COMP-3.           
008531     SKIP2                                                                
008700*    --- STATUS-KOD FRÅN IMS                                              
008800 01  STATUS-WS                   PIC XX.                                  
008900     88  SEGMENT-FINNS                       VALUE '  '.                  
009100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009300     88  IMS-EJ-OK                           VALUE 'XD'.                  
009400     SKIP2                                                                
009500 01  GODK-STATUSKODER.                                                    
009600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009700     SKIP3                                                                
009800 01  SSA1                        PIC X(128).                              
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
011002     03  W6UPFB01 REDEFINES IO-AREA.                                      
011003*        05  -COPY W6L201                                                 
011100*    ---  DLI INPUT-OUTPUT AREA                                           
011200 01  FILLER                      PIC X(16)                                
011300                             VALUE 'DLI-IO-AREA-2'.                       
011400     SKIP3                                                                
011500 01  DLI-IO-AREA-2.                                                       
011600     03  IO-AREA-2               PIC X(150)  VALUE SPACE.                 
011700     SKIP3                                                                
011800     03  W6INLC   REDEFINES IO-AREA-2.                                    
011900*        05  -COPY W6D1B1  -PRE INLB-                                     
012000     EJECT                                                                
012100 LINKAGE SECTION.                                                         
012200                                                                          
012300*01  -COPY W0009  -PRE MSG-                                               
012401     EJECT                                                                
012402*01  -COPY W0008  -PRE UPFB-                                              
012403     05  FILLER                  PIC X.                                   
012404     EJECT                                                                
012405*01  -COPY W0008  -PRE INLB-                                              
012410     05  FILLER                  PIC X.                                   
012700     EJECT                                                                
012801 PROCEDURE DIVISION  USING MSG-PCB UPFB-PCB INLB-PCB.                     
012802 MAIN SECTION.                                                            
012810     ENTRY 'DLITCBL' USING MSG-PCB UPFB-PCB INLB-PCB.                     
012900                                                                          
013100     SKIP2                                                                
013200     PERFORM A-INIT                                                       
013300                                                                          
013310     PERFORM IMS-GHN-W6UPFB01                                             
013350                                                                          
013400     PERFORM UNTIL UPFB-STATUS-CODE = TEST-SEGMENT-SLUT                   
013402                                                                          
013403       MOVE UPPF-IDLOPNRM  TO SPARA-IDLOPNRM                              
013410                              W-IDLOPNRM-INLB                             
013430       PERFORM IMS-GU-W6INLC01                                            
013440                                                                          
013450       PERFORM UNTIL (UPPF-IDLOPNRM NOT = SPARA-IDLOPNRM)                 
013451                  OR (UPFB-STATUS-CODE  = TEST-SEGMENT-SAKNAS)            
013452                  OR (UPFB-STATUS-CODE  = TEST-SEGMENT-SLUT)              
013460                                                                          
013500           IF CHKP-ANT > CHKP-MAX                                         
013600             PERFORM X-TAG-CHECKPOINT                                     
013700           END-IF                                                         
013800                                                                          
013900           IF INLB-STATUS-CODE = TEST-SEGMENT-FINNS                       
014000               CONTINUE                                                   
014100           ELSE                                                           
014110               PERFORM IMS-DLET-W6UPFB01                                  
014111               ADD +1 TO CHKP-ANT                                         
014120           END-IF                                                         
014130                                                                          
014140           PERFORM IMS-GHN-W6UPFB01                                       
014200                                                                          
014210       END-PERFORM                                                        
014300                                                                          
014500     END-PERFORM                                                          
014600                                                                          
015000     MOVE ZERO TO RETURN-CODE                                             
015100     GOBACK                                                               
015200     .                                                                    
015300     EJECT                                                                
015400 A-INIT SECTION.                                                          
015700     PERFORM IMS-RESTART                                                  
016900     .                                                                    
017100     EJECT                                                                
018600 X-TAG-CHECKPOINT   SECTION.                                              
018700                                                                          
018800* --- VID CHECKPOINT-TAGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
018900* --- SPARA DATABASNYCKLAR, LÄS MED GHU                                   
019000     MOVE UPPF-IDLOPNRM TO W-IDLOPNRM-UPFB                                
019100     MOVE UPPF-IDRADNR  TO W-IDRADNR-UPFB                                 
019200     MOVE UPPF-DAREGDAT TO W-DAREGDAT-UPFB                                
019210     MOVE UPPF-TIKLOCK  TO W-TIKLOCK-UPFB                                 
019220                                                                          
019300     PERFORM IMS-CHECKPOINT                                               
019400     MOVE ZERO TO CHKP-ANT                                                
019510     PERFORM IMS-GHU-W6UPFB01                                             
019600     .                                                                    
019700     EJECT                                                                
019800* --- IMS SEKTIONER ---                                                   
020018 IMS-GHN-W6UPFB01 SECTION.                                                
020019                                                                          
020020     MOVE 'W6UPFB01' TO SSA1                                              
020021     MOVE '  GB' TO GODK-STATUSKODER                                      
020022     CALL CBLTDLI USING GHN UPFB-PCB DLI-IO-AREA SSA1                     
020023     MOVE UPFB-STATUS-CODE TO STATUS-WS                                   
020024     PERFORM IMS-STATUSKONTROLL                                           
020025     .                                                                    
020026     EJECT                                                                
020027 IMS-GHU-W6UPFB01 SECTION.                                                
020028                                                                          
020029     STRING 'W6UPFB01(W6L201KY =' W-W6L201KY-X ')'                        
020030          DELIMITED BY SIZE INTO SSA1                                     
020031     MOVE '  ' TO GODK-STATUSKODER                                        
020032     CALL CBLTDLI USING GHU UPFB-PCB DLI-IO-AREA SSA1                     
020033     MOVE UPFB-STATUS-CODE TO STATUS-WS                                   
020034     PERFORM IMS-STATUSKONTROLL                                           
020035     .                                                                    
020036     EJECT                                                                
020037 IMS-DLET-W6UPFB01 SECTION.                                               
020038                                                                          
020039     MOVE '  ' TO GODK-STATUSKODER                                        
020040     CALL CBLTDLI USING DLET UPFB-PCB DLI-IO-AREA                         
020041     MOVE UPFB-STATUS-CODE TO STATUS-WS                                   
020042     PERFORM IMS-STATUSKONTROLL                                           
020043     .                                                                    
020044     EJECT                                                                
020045 IMS-GU-W6INLC01 SECTION.                                                 
020046                                                                          
020047     STRING 'W6INLC01(W6D1B1KY =' W-W6D1B1KY-X ')'                        
020048          DELIMITED BY SIZE INTO SSA1                                     
020049     MOVE '  GE' TO GODK-STATUSKODER                                      
020050     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA-2 SSA1                    
020051     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
020052     PERFORM IMS-STATUSKONTROLL                                           
020053     .                                                                    
020054     EJECT                                                                
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
