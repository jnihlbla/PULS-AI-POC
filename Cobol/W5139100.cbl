000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5139100.                                                
000300 AUTHOR.         LASSI OLGRENER.                                          
000400 DATE-WRITTEN.   JUN2013.                                                 
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*       ORDER ZERO TO TIINVDAT FOR A CERTAIN IDDC FROM 5321 SCREEN        
000900*                                                                         
001000 ENVIRONMENT DIVISION.                                                    
001100 DATA DIVISION.                                                           
001200 WORKING-STORAGE SECTION.                                                 
001300                                                                          
001400 77  IDPGM                       PIC X(8)    VALUE 'W5139100'.            
001500 77  FELTEXT                     PIC X(64)   VALUE SPACE.                 
001600 77  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
001700 77  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
001800 77  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
001900 77  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
002000 77  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
002100 77  CHKP-MAX                    PIC S9(3)   VALUE +600.                  
002200 77  K711-REPL                   PIC  9(7)   VALUE ZERO.                  
002300                                                                          
002400 01  PARM-SYSIN.                                                          
002500     03  PARM-IDDC               PIC X(2)  VALUE SPACE.                   
002600                                                                          
002700 01  DYNAMISKA-SUBPROGRAM.                                                
002800                                                                          
002900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
003000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
003100     EJECT                                                                
003200                                                                          
003300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
003400 01  NYCKLAR-TILL-DLI.                                                    
003500                                                                          
003600     03  W-IDDC-X.                                                        
003700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
003800     03  W-IDARTNR-X.                                                     
003900         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
004000     03  W-WDK7A1KY-MIN-X.                                                
004100         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
004200         05  W-ADART-MIN         PIC X(7)    VALUE LOW-VALUE.             
004300         05  W-IDARTNR-MIN       PIC S9(9)   COMP-3 VALUE ZERO.           
004400     03  W-WDK7A1KY-MAX-X.                                                
004500         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
004600         05  W-FILLER            PIC X(12)   VALUE HIGH-VALUE.            
004700                                                                          
004800 01  STATUS-WS                   PIC XX.                                  
004900     88  SEGMENT-EXISTS                      VALUE '  '.                  
005000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
005100     88  BASE-END                            VALUE 'GB'.                  
005200     88  IMS-NOT-OK                          VALUE 'XD'.                  
005300                                                                          
005400 01  GODK-STATUSKODER.                                                    
005500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
005600                                                                          
005700 01  SSA1                        PIC X(64).                               
005710 01  SSA2                        PIC X(64).                               
005800     EJECT                                                                
005900*    --- IMS FUNKTIONSKODER                                               
006000*01  -COPY W0003                                                          
006100     EJECT                                                                
006200*    ---  DLI INPUT-OUTPUT AREA                                           
006300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-K711'.         
006400     SKIP3                                                                
006500 01  DLI-IO-WDK711.                                                       
006600*    03  -COPY WDK711                                                     
006700     EJECT                                                                
006800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK7A1'.                      
006900 01  DLI-IO-WDK7A1.                                                       
007000*    03  -COPY WDK7A1                                                     
007100     EJECT                                                                
007200 LINKAGE SECTION.                                                         
007300*01  -COPY W0009   -PRE MSG-                                              
007400*01  -COPY W0008   -PRE WDK7-                                             
007500     05  FILLER                  PIC X.                                   
007600*01  -COPY W0008   -PRE WDK7A-                                            
007700     05  FILLER                  PIC X.                                   
007800     EJECT                                                                
007900 PROCEDURE DIVISION  USING MSG-PCB WDK7-PCB WDK7A-PCB.                    
008000 MAIN SECTION.                                                            
008100     ENTRY 'DLITCBL' USING MSG-PCB WDK7-PCB WDK7A-PCB.                    
008200                                                                          
008300     PERFORM IMS-RESTART                                                  
008400     ACCEPT PARM-SYSIN FROM SYSIN                                         
008500                                                                          
008600     MOVE PARM-IDDC    TO W-IDDC-MIN                                      
008700                          W-IDDC-MAX                                      
008800     PERFORM IMS-GN-WDK7A1                                                
008900     PERFORM UNTIL SEGMENT-MISSING OR BASE-END                            
009000       MOVE SEQA-IDDC            TO W-IDDC                                
009100       MOVE SEQA-IDARTNR         TO W-IDARTNR                             
009200       PERFORM IMS-GHU-WDK711                                             
009300                                                                          
009400       IF SLAG-TIINVDAT > ZERO                                            
009500         MOVE ZERO  TO SLAG-TIINVDAT                                      
009600         PERFORM IMS-REPL-K711                                            
009700         ADD 1 TO CHKP-ANT                                                
009800                  K711-REPL                                               
009900       END-IF                                                             
010000                                                                          
010100       IF CHKP-ANT > CHKP-MAX                                             
010200         PERFORM IMS-CHECKPOINT                                           
010300         MOVE ZERO TO CHKP-ANT                                            
010400* REPOSITIONING NEEDS AFTER CHECKPOINT                                    
010500         MOVE SEQA-IDDC            TO W-IDDC-MIN                          
010600         MOVE SEQA-ADART           TO W-ADART-MIN                         
010700         MOVE SEQA-IDARTNR         TO W-IDARTNR-MIN                       
010800         PERFORM IMS-GU-WDK7A1                                            
010900       END-IF                                                             
011000                                                                          
011100       PERFORM IMS-GN-WDK7A1                                              
011200     END-PERFORM                                                          
011300     DISPLAY 'K711 REPLACED: ' K711-REPL                                  
011400                                                                          
011500     MOVE ZERO TO RETURN-CODE                                             
011600     GOBACK                                                               
011700     .                                                                    
011800     EJECT                                                                
011900 IMS-GN-WDK7A1 SECTION.                                                   
012000     STRING 'WDK7A1  (WDK7A1KY=>' W-WDK7A1KY-MIN-X                        
012100                    '&WDK7A1KY<=' W-WDK7A1KY-MAX-X ')'                    
012200          DELIMITED BY SIZE INTO SSA1                                     
012300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
012400     CALL CBLTDLI USING GN  WDK7A-PCB DLI-IO-WDK7A1 SSA1                  
012500     MOVE WDK7A-STATUS-CODE TO STATUS-WS                                  
012600     PERFORM IMS-STATUSKONTROLL                                           
012700     .                                                                    
012800     SKIP3                                                                
012900 IMS-GU-WDK7A1 SECTION.                                                   
013000     STRING 'WDK7A1  (WDK7A1KY =' W-WDK7A1KY-MIN-X ')'                    
013100          DELIMITED BY SIZE INTO SSA1                                     
013200     MOVE '  GE' TO GODK-STATUSKODER                                      
013300     CALL CBLTDLI USING GU WDK7A-PCB DLI-IO-WDK7A1 SSA1                   
013400     MOVE WDK7A-STATUS-CODE TO STATUS-WS                                  
013500     PERFORM IMS-STATUSKONTROLL                                           
013600     .                                                                    
013700     SKIP3                                                                
013800 IMS-GHU-WDK711 SECTION.                                                  
013900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
014000          DELIMITED BY SIZE INTO SSA1                                     
014100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
014200          DELIMITED BY SIZE INTO SSA2                                     
014300     MOVE '  ' TO GODK-STATUSKODER                                        
014400     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
014500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
014600     PERFORM IMS-STATUSKONTROLL                                           
014700     .                                                                    
014800     SKIP3                                                                
014900 IMS-REPL-K711 SECTION.                                                   
015000                                                                          
015100     MOVE '  ' TO GODK-STATUSKODER                                        
015200     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
015300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
015400     PERFORM IMS-STATUSKONTROLL                                           
015500     .                                                                    
015600                                                                          
015700 IMS-RESTART SECTION.                                                     
015800                                                                          
015900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
016000     MOVE '  ' TO GODK-STATUSKODER                                        
016100     CALL CBLTDLI USING XRST MSG-PCB                                      
016200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
016300                        CHKP-AREA-LENGTH CHKP-AREA                        
016400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
016500     PERFORM IMS-STATUSKONTROLL                                           
016600     .                                                                    
016700                                                                          
016800 IMS-CHECKPOINT SECTION.                                                  
016900                                                                          
017000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
017100     MOVE '  XD' TO GODK-STATUSKODER                                      
017200     CALL CBLTDLI USING CHKP MSG-PCB                                      
017300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
017400                        CHKP-AREA-LENGTH CHKP-AREA                        
017500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
017600     PERFORM IMS-STATUSKONTROLL                                           
017700                                                                          
017800     IF IMS-NOT-OK                                                        
017900       MOVE ' IMS-CONTROL REGION NOT AVAILABLE '                          
018000                             TO FELTEXT                                   
018100       CALL FELLOG                                                        
018200     END-IF                                                               
018300     .                                                                    
018400 IMS-STATUSKONTROLL SECTION.                                              
018500                                                                          
018600     SET STATUS-IX TO 1                                                   
018700     SEARCH GODK-STATUS                                                   
018800       AT END                                                             
018900         CALL FELLOG                                                      
019000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
019100         CONTINUE                                                         
019200     END-SEARCH                                                           
019300     .                                                                    
