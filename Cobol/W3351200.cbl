000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3351200.                                                
000300 AUTHOR.         THOMAS LARSSON.                                          
000400 DATE-WRITTEN.   93/12/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET RENSAR BORT DE PRISOMRÅDEN SOM SAKNAR BETALARE        
000900*        DETTA SKER VECKOVIS                                              
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDC2                                       
001200*        PROGRAMMET LÄSER      WDB1A SEKUNDÄRINDEX                        
001300*                                                                         
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700                                                                          
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100                                                                          
002200 DATA DIVISION.                                                           
002300                                                                          
002400 FILE SECTION.                                                            
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(8)    VALUE 'W3351200'.            
003000 01  CHKP-VAR.                                                            
003100 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
003200 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
003300 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
003400 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
003500 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
003600 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900     SKIP2                                                                
004000 01  FELTEXT.                                                             
004100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004300     EJECT                                                                
004400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004500 01  FILLER REDEFINES DAGENS-DATUM.                                       
004600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004900     EJECT                                                                
005000 01  DYNAMISKA-SUBPROGRAM.                                                
005100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005300                                                                          
005400     EJECT                                                                
005500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
005600                                                                          
005700 01  NYCKLAR-TILL-DLI.                                                    
005800     03  W-IDPROMR-X.                                                     
005900         05  W-IDMARKBO          PIC X       VALUE SPACE.                 
006000         05  W-IDPROMRN          PIC X(2)    VALUE SPACE.                 
006100                                                                          
006200     03  W-WDB1A1KY-MIN.                                                  
006300         05  W-IDMARKBO-MIN      PIC X       VALUE SPACE.                 
006400         05  W-IDPROMRN-MIN      PIC X(2)    VALUE SPACE.                 
006500         05  W-IDPARTNR-MIN      PIC X(9)    VALUE LOW-VALUE.             
006600         05  W-IDFTG-MIN         PIC 9(2)    VALUE ZERO.                  
006700                                                                          
006800     03  W-WDB1A1KY-MAX.                                                  
006900         05  W-IDMARKBO-MAX      PIC X       VALUE SPACE.                 
007000         05  W-IDPROMRN-MAX      PIC X(2)    VALUE SPACE.                 
007100         05  W-IDPARTNR-MAX      PIC X(9)    VALUE HIGH-VALUE.            
007200         05  W-IDFTG-MAX         PIC 9(2)    VALUE 99.                    
007300                                                                          
007400*    --- STATUS-KOD FRÅN IMS                                              
007500 01  STATUS-WS                   PIC XX.                                  
007600     88  SEGMENT-FINNS                       VALUE '  '.                  
007700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
007800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
007900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008000     88  IMS-EJ-OK                           VALUE 'XD'.                  
008100     SKIP2                                                                
008200 01  GODK-STATUSKODER.                                                    
008300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008400     SKIP3                                                                
008500 01  SSA1                        PIC X(64).                               
008600 01  SSA2                        PIC X(64).                               
008700     EJECT                                                                
008800*    --- IMS FUNKTIONSKODER                                               
008900*01  -COPY W0003                                                          
009000     EJECT                                                                
009100*    ---  DLI INPUT-OUTPUT AREA                                           
009200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
009300                                                                          
009400 01  DLI-IO-AREA.                                                         
009500     03  WDC201.                                                          
009600*        05  -COPY WDC201  -PRE WDC2-                                     
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)                                
009900                             VALUE 'DLI-IO-AREA-2'.                       
010000                                                                          
010100 01  DLI-IO-AREA-2.                                                       
010200     03  WDB1A1.                                                          
010300*        05  -COPY WDB1A1  -PRE WDB1A-                                    
010400     EJECT                                                                
010500 LINKAGE SECTION.                                                         
010600*01  -COPY W0009   -PRE MSG-                                              
010700     EJECT                                                                
010800*01  -COPY W0008  -PRE WDC2-                                              
010900     05  FILLER                  PIC X.                                   
011000                                                                          
011100*01  -COPY W0008  -PRE WDB1A-                                             
011200     05  FILLER                  PIC X.                                   
011300     EJECT                                                                
011400 PROCEDURE DIVISION  USING MSG-PCB WDC2-PCB WDB1A-PCB.                    
011500 MAIN SECTION.                                                            
011600     ENTRY 'DLITCBL' USING MSG-PCB WDC2-PCB WDB1A-PCB.                    
011700                                                                          
011800     PERFORM A-INIT                                                       
011900     PERFORM IMS-GN-WDC201                                                
012000     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
012100       IF CHKP-ANT > CHKP-MAX                                             
012200         PERFORM X-TAG-CHECKPOINT                                         
012300       END-IF                                                             
012400       IF SEGMENT-FINNS                                                   
012500         MOVE WDC2-PRO-IDMARKBO TO W-IDMARKBO-MIN                         
012600                                   W-IDMARKBO-MAX                         
012700                                   W-IDMARKBO                             
012800         MOVE WDC2-PRO-IDPROMRN TO W-IDPROMRN-MIN                         
012900                                   W-IDPROMRN-MAX                         
013000                                   W-IDPROMRN                             
013100                                                                          
013200*********** LÄSER WDB1 MED SEKUNDÄR NYCKEL ( IDPROMR )                    
013300                                                                          
013400         PERFORM IMS-GU-WDB1                                              
013500         IF SEGMENT-SAKNAS                                                
013600           PERFORM IMS-GHU-WDC201                                         
013700*  FIX FÖR ATT HINDRA ATT A30 RABATTER NORDEN RENAULT EJ TAS BORT         
013800*  DÅ SVERIGE ÄR NUMERA KOPPLAD TILL B30...                               
013900*          IF SEGMENT-FINNS                                               
014000*            PERFORM IMS-DLET-WDC201                                      
014100*            ADD +1 TO CHKP-ANT                                           
014200*          END-IF                                                         
014300         END-IF                                                           
014400       END-IF                                                             
014500       PERFORM IMS-GN-WDC201                                              
014600     END-PERFORM                                                          
014700     MOVE ZERO TO RETURN-CODE                                             
014800     GOBACK                                                               
014900     .                                                                    
015000     EJECT                                                                
015100 A-INIT SECTION.                                                          
015200                                                                          
015300     PERFORM IMS-RESTART                                                  
015400     ACCEPT DAGENS-DATUM FROM DATE                                        
015500                                                                          
015600     .                                                                    
015700     EJECT                                                                
015800 X-TAG-CHECKPOINT   SECTION.                                              
015900                                                                          
016000     PERFORM IMS-CHECKPOINT                                               
016100     MOVE ZERO TO CHKP-ANT                                                
016200     .                                                                    
016300     EJECT                                                                
016400* --- IMS SEKTIONER ---                                                   
016500                                                                          
016600 IMS-GN-WDC201 SECTION.                                                   
016700     MOVE 'WDC201   ' TO SSA1                                             
016800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
016900     CALL CBLTDLI USING GN WDC2-PCB DLI-IO-AREA SSA1                      
017000     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
017100     PERFORM IMS-STATUSKONTROLL                                           
017200     .                                                                    
017300     SKIP3                                                                
017400 IMS-GHU-WDC201 SECTION.                                                  
017500     STRING 'WDC201  (IDPROMR  =' W-IDPROMR-X ')'                         
017600          DELIMITED BY SIZE INTO SSA1                                     
017700     MOVE '  GE' TO GODK-STATUSKODER                                      
017800     CALL CBLTDLI USING GHU WDC2-PCB DLI-IO-AREA SSA1                     
017900     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
018000     PERFORM IMS-STATUSKONTROLL                                           
018100     .                                                                    
018200     SKIP3                                                                
018300 IMS-DLET-WDC201 SECTION.                                                 
018400                                                                          
018500     MOVE '  ' TO GODK-STATUSKODER                                        
018600     CALL CBLTDLI USING DLET WDC2-PCB DLI-IO-AREA                         
018700     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
018800     PERFORM IMS-STATUSKONTROLL                                           
018900     .                                                                    
019000     EJECT                                                                
019100 IMS-GU-WDB1 SECTION.                                                     
019200     STRING 'WDB1A1  (WDB1A1KY>=' W-WDB1A1KY-MIN                          
019300                    '&WDB1A1KY<=' W-WDB1A1KY-MAX ')'                      
019400          DELIMITED BY SIZE INTO SSA1                                     
019500     MOVE '  GE' TO GODK-STATUSKODER                                      
019600     CALL CBLTDLI USING GU WDB1A-PCB DLI-IO-AREA-2 SSA1                   
019700     MOVE WDB1A-STATUS-CODE TO STATUS-WS                                  
019800     PERFORM IMS-STATUSKONTROLL                                           
019900     .                                                                    
020000     EJECT                                                                
020100 IMS-RESTART SECTION.                                                     
020200                                                                          
020300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020400     MOVE '  ' TO GODK-STATUSKODER                                        
020500     CALL CBLTDLI USING XRST MSG-PCB                                      
020600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020700                        CHKP-AREA-LENGTH CHKP-AREA                        
020800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020900     PERFORM IMS-STATUSKONTROLL                                           
021000     .                                                                    
021100     SKIP3                                                                
021200 IMS-CHECKPOINT SECTION.                                                  
021300                                                                          
021400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021500     MOVE '  XD' TO GODK-STATUSKODER                                      
021600     CALL CBLTDLI USING CHKP MSG-PCB                                      
021700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021800                        CHKP-AREA-LENGTH CHKP-AREA                        
021900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022000     PERFORM IMS-STATUSKONTROLL                                           
022100                                                                          
022200     IF IMS-EJ-OK                                                         
022300       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
022400       DISPLAY FELTEXT                                                    
022500       CALL FELLOG                                                        
022600     END-IF                                                               
022700     .                                                                    
022800     EJECT                                                                
022900 IMS-STATUSKONTROLL SECTION.                                              
023000                                                                          
023100     SET STATUS-IX TO 1                                                   
023200     SEARCH GODK-STATUS                                                   
023300       AT END                                                             
023400         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
023500         DISPLAY FELTEXT                                                  
023600         CALL FELLOG                                                      
023700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
023800         CONTINUE                                                         
023900     END-SEARCH                                                           
024000     .                                                                    
