000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4189000.                                                
000300 AUTHOR.         MÅNS SAMUELSSON.                                         
000400 DATE-WRITTEN.   96/04/22.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        RENSAR SÄNDNINGSREGISTRET (WDA3)                                 
000900*                                                                         
001000                                                                          
001100 ENVIRONMENT DIVISION.                                                    
001200                                                                          
001300 INPUT-OUTPUT SECTION.                                                    
001400                                                                          
001500 FILE-CONTROL.                                                            
001600                                                                          
001700 DATA DIVISION.                                                           
001800                                                                          
001900 FILE SECTION.                                                            
002000     EJECT                                                                
002100 WORKING-STORAGE SECTION.                                                 
002200**   --   CHECKED BY WY2000                                               
002300     SKIP3                                                                
002400 77  IDPGM                       PIC X(8)    VALUE 'W4189000'.            
002500 01  CHKP-VAR.                                                            
002600 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
002700 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
002800 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
002900 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
003000 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
003100 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400                                                                          
003500 01  FELTEXT.                                                             
003600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
003700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
003800     EJECT                                                                
003900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004000 01  FILLER REDEFINES DAGENS-DATUM.                                       
004100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004400     EJECT                                                                
004500 01  DYNAMISKA-SUBPROGRAM.                                                
004600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004800     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
004900     EJECT                                                                
005000*01  -COPY WORKAREA                                                       
005100*                                                                         
005200     EJECT                                                                
005300*01  -COPY  WWDC99                                                        
005400     EJECT                                                                
005500                                                                          
005600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
005700                                                                          
005800 01  NYCKLAR-TILL-DLI.                                                    
005900     03  W-WDA301KY-X.                                                    
006000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
006100         05  W-DAREGDAT          PIC  9(8)   VALUE ZERO.                  
006200         05  W-TIKLOCK           PIC S9(9)   VALUE ZERO COMP-3.           
006300*    --- STATUS-KOD FRÅN IMS                                              
006400 01  STATUS-WS                   PIC XX.                                  
006500     88  SEGMENT-FINNS                       VALUE '  '.                  
006600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
006700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
006800     88  IMS-EJ-OK                           VALUE 'XD'.                  
006900                                                                          
007000 01  GODK-STATUSKODER.                                                    
007100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007200                                                                          
007300 01  SSA1                        PIC X(64).                               
007400     EJECT                                                                
007500*    --- IMS FUNKTIONSKODER                                               
007600*01  -COPY W0003                                                          
007700     EJECT                                                                
007800*    ---  DLI INPUT-OUTPUT AREA                                           
007900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
008000                                                                          
008100 01  DLI-IO-AREA.                                                         
008200     03  WLRETA01.                                                        
008300*        05  -COPY WDA301  -PRE RETA-.                                    
008400     EJECT                                                                
008500 LINKAGE SECTION.                                                         
008600*01  -COPY W0009   -PRE MSG-                                              
008700     EJECT                                                                
008800*01  -COPY W0008  -PRE RETA-                                              
008900     05  FILLER                  PIC X.                                   
009000     EJECT                                                                
009100 PROCEDURE DIVISION  USING MSG-PCB RETA-PCB.                              
009200 MAIN SECTION.                                                            
009300     ENTRY 'DLITCBL' USING MSG-PCB RETA-PCB.                              
009400                                                                          
009500     PERFORM A-INIT                                                       
009600     PERFORM IMS-GHN-RETA01                                               
009700     PERFORM UNTIL SEGMENT-SLUT                                           
009800       IF CHKP-ANT > CHKP-MAX                                             
009900         PERFORM X-TAG-CHECKPOINT                                         
010000       END-IF                                                             
010100       IF RETA-RET-KDRETSTA = 6                                           
010200         PERFORM S01-KOLLA-DATUM                                          
010300         MOVE RETA-RET-IDDC TO WS-IDDC                                    
010400         IF WORK-KVWORKD > 10                                             
010500            PERFORM IMS-DLET-RETA                                         
010600            ADD +1 TO CHKP-ANT                                            
010700         END-IF                                                           
010800       ELSE                                                               
010900         IF RETA-RET-KDRETSTA = 0 AND                                     
011000            RETA-RET-IDDISTR  = 0                                         
011100           PERFORM IMS-DLET-RETA                                          
011200           ADD +1 TO CHKP-ANT                                             
011300         END-IF                                                           
011400       END-IF                                                             
011500       PERFORM IMS-GHN-RETA01                                             
011600     END-PERFORM                                                          
011700                                                                          
011800     MOVE ZERO TO RETURN-CODE                                             
011900     GOBACK                                                               
012000     .                                                                    
012100     EJECT                                                                
012200 A-INIT SECTION.                                                          
012300                                                                          
012400     PERFORM IMS-RESTART                                                  
012500     ACCEPT DAGENS-DATUM FROM DATE                                        
012600     .                                                                    
012700     EJECT                                                                
012800 S01-KOLLA-DATUM SECTION.                                                 
012900                                                                          
013000     MOVE +001            TO WORK-KDCALL                                  
013100     MOVE RETA-RET-IDDC   TO WORK-IDDC                                    
013200     MOVE RETA-RET-TIKLAR TO WORK-TIAAMMDD-FOM                            
013300     MOVE DAGENS-DATUM    TO WORK-TIAAMMDD-TOM                            
013400                                                                          
013500     CALL WORKDAY  USING WORK-KDCALL                                      
013600                         WORK-DATE-AREA                                   
013700                         WORK-KDSVAR                                      
013800                                                                          
013900     IF WORK-KDSVAR-FEL                                                   
014000       MOVE 'FEL FRÅN WORKDAY' TO FELTEXT                                 
014100     END-IF                                                               
014200     .                                                                    
014300     EJECT                                                                
014400 X-TAG-CHECKPOINT   SECTION.                                              
014500                                                                          
014600     MOVE RETA-RET-IDDC        TO W-IDDC                                  
014700     MOVE RETA-RET-DAREGDAT    TO W-DAREGDAT                              
014800     MOVE RETA-RET-TIKLOCK     TO W-TIKLOCK                               
014900     PERFORM IMS-CHECKPOINT                                               
015000     MOVE ZERO TO CHKP-ANT                                                
015100     PERFORM IMS-GHU-RETA01                                               
015200     .                                                                    
015300     EJECT                                                                
015400* --- IMS SEKTIONER ---                                                   
015500 IMS-GHU-RETA01   SECTION.                                                
015600                                                                          
015700     STRING 'WLRETA01(WDA301KY =' W-WDA301KY-X ')'                        
015800          DELIMITED BY SIZE INTO SSA1                                     
015900     MOVE '  ' TO GODK-STATUSKODER                                        
016000     CALL CBLTDLI USING GHU RETA-PCB DLI-IO-AREA SSA1                     
016100     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
016200     PERFORM IMS-STATUSKONTROLL                                           
016300     .                                                                    
016400     SKIP3                                                                
016500 IMS-GHN-RETA01   SECTION.                                                
016600                                                                          
016700     MOVE '  GB' TO GODK-STATUSKODER                                      
016800     CALL CBLTDLI USING GHN RETA-PCB DLI-IO-AREA                          
016900     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
017000     PERFORM IMS-STATUSKONTROLL                                           
017100     .                                                                    
017200     SKIP3                                                                
017300 IMS-DLET-RETA SECTION.                                                   
017400                                                                          
017500     MOVE '  ' TO GODK-STATUSKODER                                        
017600     CALL CBLTDLI USING DLET RETA-PCB DLI-IO-AREA                         
017700     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
017800     PERFORM IMS-STATUSKONTROLL                                           
017900     .                                                                    
018000     EJECT                                                                
018100 IMS-RESTART SECTION.                                                     
018200                                                                          
018300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
018400     MOVE '  ' TO GODK-STATUSKODER                                        
018500     CALL CBLTDLI USING XRST MSG-PCB                                      
018600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
018700                        CHKP-AREA-LENGTH CHKP-AREA                        
018800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
018900     PERFORM IMS-STATUSKONTROLL                                           
019000     .                                                                    
019100     EJECT                                                                
019200 IMS-CHECKPOINT SECTION.                                                  
019300                                                                          
019400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019500     MOVE '  XD' TO GODK-STATUSKODER                                      
019600     CALL CBLTDLI USING CHKP MSG-PCB                                      
019700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019800                        CHKP-AREA-LENGTH CHKP-AREA                        
019900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020000     PERFORM IMS-STATUSKONTROLL                                           
020100                                                                          
020200     IF IMS-EJ-OK                                                         
020300       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
020400       DISPLAY FELTEXT                                                    
020500       CALL FELLOG                                                        
020600     END-IF                                                               
020700     .                                                                    
020800     EJECT                                                                
020900 IMS-STATUSKONTROLL SECTION.                                              
021000                                                                          
021100     SET STATUS-IX TO 1                                                   
021200     SEARCH GODK-STATUS                                                   
021300       AT END                                                             
021400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
021500           DELIMITED BY SIZE INTO FELTEXT                                 
021600         DISPLAY FELTEXT                                                  
021700         CALL FELLOG                                                      
021800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
021900         CONTINUE                                                         
022000     END-SEARCH                                                           
022100     .                                                                    
