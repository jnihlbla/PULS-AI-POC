000100 ID DIVISION.                                                             
000200 PROGRAM-ID.        W1224000.                                             
000300 AUTHOR.            P DAHLÖF.                                             
000400 DATE-WRITTEN.      OKT 1987.                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*            LÄSER FIL W12225. ALLA B-MÄRKTA ARTIKLAR ÄR                  
000900*            SÅDANA SOM BLIVIT RENSADE. DESSA SKALL UPP-                  
001000*            DATERAS PÅ WDN6.                                             
001100                                                                          
001200*    MÄRKNING SKER:                                                       
001300*            I PROGRAM W12204                                             
001400                                                                          
001500                                                                          
001600     EJECT                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800 INPUT-OUTPUT SECTION.                                                    
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*- - - - - - - - - - - - - - INFIL:                                       
002200     SELECT W12225         ASSIGN TO     W12240D1.                        
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002700 FD   W12225                                                              
002800      RECORDING F                                                         
002900      BLOCK CONTAINS 0.                                                   
003000*01   POST -COPY W12225   -PRE W12225-    -L.                             
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(8)    VALUE 'W1224000'.            
003600 01  GENERELLA-KONSTANTER.                                                
003700   03  JA                        PIC X       VALUE 'J'.                   
003800   03  NEJ                       PIC X       VALUE 'N'.                   
003900   03  W12225-EOF                PIC X       VALUE 'N'.                   
004000   03  W-KDERS                   PIC S9(3).                               
004100   03  W-REPL-WDN601             PIC 9(7)    VALUE ZERO.                  
004200   03  W-REPL-WDN612             PIC 9(7)    VALUE ZERO.                  
004300   03  W-ISRT-WDN612             PIC 9(7)    VALUE ZERO.                  
004400     SKIP2                                                                
004500 01  DYNAMISKA-SUBPROGRAM.                                                
004600   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
004700   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
004800   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
004900   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
005000     SKIP3                                                                
005100*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
005200*01  -COPY W0005     -PRE  POSTSUM-.                                      
005300     EJECT                                                                
005400*- - - - - - - - - - - - - -  ARBETSAREAR FÖR INFIL                       
005500 01  FILLER                      PIC X(24)    VALUE 'INFIL'.              
005600                                                                          
005700*01  AREA    -COPY W12225   -PRE W-IN-.                                   
005800     EJECT                                                                
005900*- - - - - - - - -PARAMETER-AREOR TILL IMS-SUBPGM-SEKTIONER.              
006000 01  FILLER                      PIC X(16) VALUE 'IMS-WS'.                
006100*- - - - - - - - -STATUSKOD FRÅN IMS                                      
006200 01  STATUS-WS                   PIC X(2).                                
006300   88  SEGMENT-FINNS         VALUE '  '.                                  
006400   88  SEGMENT-SAKNAS        VALUE 'GE'.                                  
006500   88  SEGMENT-FINNS-REDAN   VALUE 'II'.                                  
006600   SKIP3                                                                  
006700 01  GODK-STATUSKODER.                                                    
006800    03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                 
006900 01  SSA1                        PIC X(32).                               
007000 01  SSA2                        PIC X(32).                               
007100 01  SSA3                        PIC X(32).                               
007200*- - - - - - - - - - - - - - NYCKLAR TILL DLI.                            
007300 01  NYCKLAR-TILL-DLI.                                                    
007400   03   W-IDARTNR-X.                                                      
007500     05 W-IDARTNR                PIC S9(9)  COMP-3.                       
007600     EJECT                                                                
007700*01   -COPY W0003                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)    VALUE                       
008000                                              'DLI-IO-AREA'.              
008100 01  DLI-IO-AREA.                                                         
008200   03  IO-AREA                   PIC X(130) VALUE SPACE.                  
008300*03  WLKATN01  -COPY WDN601  -PRE KATN01-  -RED IO-AREA.                  
008400     EJECT                                                                
008500*03  WLKATN12  -COPY WDN612  -PRE KATN12-  -RED IO-AREA.                  
008600     EJECT                                                                
008700 LINKAGE SECTION.                                                         
008800                                                                          
008900*01  -COPY W0008       -PRE KATN-.                                        
009000 05  FILLER             PIC X.                                            
009100     EJECT                                                                
009200 PROCEDURE DIVISION USING KATN-PCB.                                       
009300 MAIN SECTION.                                                            
009400     ENTRY 'DLITCBL' USING KATN-PCB.                                      
009500                                                                          
009600     PERFORM A-INIT                                                       
009700     PERFORM S11-LAES-INFIL                                               
009800     PERFORM UNTIL                                                        
009900      NOT ( W12225-EOF = NEJ )                                            
010000       IF W-IN-UTFIL-TYP = 'B'                                            
010100         MOVE W-IN-IDARTNR TO W-IDARTNR                                   
010200         PERFORM IMS-GHU-KATN01                                           
010300         IF SEGMENT-FINNS                                                 
010400           PERFORM B-UPPDATERA-WDN6                                       
010500         END-IF                                                           
010600       END-IF                                                             
010700       PERFORM S11-LAES-INFIL                                             
010800     END-PERFORM                                                          
010900                                                                          
011000     PERFORM Z-FINIT                                                      
011100     MOVE ZERO TO RETURN-CODE                                             
011200     GOBACK                                                               
011300     .                                                                    
011400     EJECT                                                                
011500 A-INIT SECTION.                                                          
011600                                                                          
011700     OPEN  INPUT W12225                                                   
011800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
011900     .                                                                    
012000     EJECT                                                                
012100 B-UPPDATERA-WDN6 SECTION.                                                
012200                                                                          
012300     MOVE 'NS' TO KATN01-MAST-KDMASTAT                                    
012400     PERFORM IMS-REPL-KATN                                                
012500     ADD 1 TO W-REPL-WDN601                                               
012600     PERFORM IMS-GNP-KATN12                                               
012700     IF SEGMENT-FINNS                                                     
012800       PERFORM IMS-GU-KATN01                                              
012900       PERFORM IMS-GHNP-KATN12                                            
013000       MOVE '1'              TO KATN12-BEN-KDSEGKEY                       
013100       MOVE W-IN-BEART       TO KATN12-BEN-BEART                          
013200       MOVE W-IN-KDHOMONYM   TO KATN12-BEN-KDHOMONYM                      
013300       PERFORM IMS-REPL-KATN                                              
013310       ADD 1 TO W-REPL-WDN612                                             
013400     ELSE                                                                 
013500       PERFORM IMS-GHU-KATN01                                             
013600       MOVE '1'              TO KATN12-BEN-KDSEGKEY                       
013700       MOVE W-IN-BEART       TO KATN12-BEN-BEART                          
013800       MOVE W-IN-KDHOMONYM   TO KATN12-BEN-KDHOMONYM                      
013900       PERFORM IMS-ISRT-KATN                                              
013910       ADD 1 TO W-ISRT-WDN612                                             
014000     END-IF                                                               
014100     .                                                                    
014200     EJECT                                                                
014300 S11-LAES-INFIL SECTION.                                                  
014400                                                                          
014500     READ W12225 INTO W-IN-AREA                                           
014600         AT END MOVE JA TO W12225-EOF                                     
014700     END-READ                                                             
014800     IF W12225-EOF = NEJ                                                  
014900       MOVE 'W12240D1'   TO POSTSUM-DDNAMN2                               
015000       MOVE 'W12225'     TO POSTSUM-FDNAMN                                
015100       CALL POSTSUM USING POSTSUM-PARM                                    
015200     END-IF                                                               
015300     .                                                                    
015400     EJECT                                                                
015500 Z-FINIT SECTION.                                                         
015600                                                                          
015610     DISPLAY ' ANTAL ÄNDRADE WDN601: ' W-REPL-WDN601                      
015620     DISPLAY ' ANTAL ÄNDRADE WDN612: ' W-REPL-WDN612                      
015630     DISPLAY ' ANTAL NYA WDN612: ' W-ISRT-WDN612                          
015700     CLOSE W12225                                                         
015800     MOVE 'S'      TO POSTSUM-OPKOD                                       
015900     CALL POSTSUM USING POSTSUM-PARM                                      
016000     .                                                                    
016100     EJECT                                                                
016200*- - - - - - - - - - - - - - - - - - - -IMS-SECTIONER.                    
016300 IMS-GU-KATN01 SECTION.                                                   
016400     STRING 'WLKATN01(IDARTNR  =' W-IDARTNR-X ')'                         
016500            DELIMITED BY SIZE INTO SSA1                                   
016600     MOVE '  GE' TO GODK-STATUSKODER                                      
016700     CALL CBLTDLI USING GU KATN-PCB DLI-IO-AREA SSA1                      
016800     MOVE KATN-STATUS-CODE  TO STATUS-WS                                  
016900     PERFORM IMS-STATUSKONTROLL                                           
017000     .                                                                    
017100     SKIP3                                                                
017200 IMS-GHU-KATN01 SECTION.                                                  
017300     STRING 'WLKATN01(IDARTNR  =' W-IDARTNR-X ')'                         
017400            DELIMITED BY SIZE INTO SSA1                                   
017500     MOVE '  GE' TO GODK-STATUSKODER                                      
017600     CALL CBLTDLI USING GHU KATN-PCB DLI-IO-AREA SSA1                     
017700     MOVE KATN-STATUS-CODE  TO STATUS-WS                                  
017800     PERFORM IMS-STATUSKONTROLL                                           
017900     .                                                                    
018000     EJECT                                                                
018100 IMS-GNP-KATN12 SECTION.                                                  
018200     MOVE 'WLKATN12' TO SSA1                                              
018300     MOVE '  GE' TO GODK-STATUSKODER                                      
018400     CALL CBLTDLI USING GNP   KATN-PCB DLI-IO-AREA SSA1                   
018500     MOVE KATN-STATUS-CODE  TO STATUS-WS                                  
018600     PERFORM IMS-STATUSKONTROLL                                           
018700     .                                                                    
018800     SKIP3                                                                
018900 IMS-GHNP-KATN12 SECTION.                                                 
019000     MOVE 'WLKATN12' TO SSA1                                              
019100     MOVE '  ' TO GODK-STATUSKODER                                        
019200     CALL CBLTDLI USING GHNP  KATN-PCB DLI-IO-AREA SSA1                   
019300     MOVE KATN-STATUS-CODE  TO STATUS-WS                                  
019400     PERFORM IMS-STATUSKONTROLL                                           
019500     .                                                                    
019600     EJECT                                                                
019700 IMS-REPL-KATN SECTION.                                                   
019800     MOVE '  ' TO GODK-STATUSKODER                                        
019900     CALL CBLTDLI USING REPL KATN-PCB DLI-IO-AREA                         
020000     MOVE KATN-STATUS-CODE  TO STATUS-WS                                  
020100     PERFORM IMS-STATUSKONTROLL                                           
020200     .                                                                    
020300     SKIP3                                                                
020400 IMS-ISRT-KATN SECTION.                                                   
020500     MOVE 'WLKATN12 ' TO SSA1                                             
020600     MOVE '  ' TO GODK-STATUSKODER                                        
020700     CALL CBLTDLI USING ISRT KATN-PCB DLI-IO-AREA SSA1                    
020800     MOVE KATN-STATUS-CODE  TO STATUS-WS                                  
020900     PERFORM IMS-STATUSKONTROLL                                           
021000     .                                                                    
021100     SKIP3                                                                
021200 IMS-STATUSKONTROLL SECTION.                                              
021300     SET STATUS-IX TO 1                                                   
021400     SEARCH GODK-STATUS                                                   
021500       AT END                                                             
021600         CALL FELLOG                                                      
021700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
021800         CONTINUE                                                         
021900     END-SEARCH                                                           
022000     .                                                                    
