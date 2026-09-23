000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W1150200.                                    
000300 AUTHOR.                     CONNY EGHOLT                                 
000400 DATE-WRITTEN.               SEP 92                                       
000500     SKIP2                                                                
000600*    REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*    (BMP MED CHECK-POINT)                                                
001000*    LÄSER WLARTG                                                         
001100*    UPPDATERAR WLARTG11                                                  
001200************************************************************              
001300*                                                          *              
001400*    O B S E R V E R A  DETTA ÄR ETT  F I X P R O G R A M  *              
001500*    FÖR ATT RÄTTA UPP ARTIKLAR SOM OAVSIKTLIGT            *              
001600*                                                          *              
001700*    TAGITS NED FRÅN KÖN FÖR MARKNAD  WS-BASLM-1           *              
001800*       "    "    "   "  FÖR MARKNAD  WS-BASLM-2           *              
001900*       "    "    "   "  FÖR MARKNAD  WS-BASLM-3           *              
002000*    OCH DESSUTOM TILLHÖR PROJEKT     WS-IDPROJ            *              
002100*                                                          *              
002200*    ÄNDRA I W-STORAGE INNAN KÖRNING !                     *              
002300*                                                          *              
002400************************************************************              
002500*                                                                         
002600*    OM ARTG11-TIBASLM = +2  OCH  DET ÄR OVANST MARKN/PROJ                
002700*    SKALL ARTG11-TIBASLM    ÅSÄTTAS +0                                   
002800*    OCH   ARTG11-TISTOMREG    -"-   +0                                   
002900*    OCH   ARTG11-FLBLMQ       -"-   N                                    
003000*                                                                         
003100*                                                                         
003200     EJECT                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600     SKIP2                                                                
003601                                                                          
003610*    -- CHECKED BY WY2000                                                 
003700 77  PROGRAM-NAMN            PIC X(8) VALUE 'W1150200'.                   
003800*- - - - - - - - - - - - - - - - - HJÄLP-FAELT                            
003900 77  WS-BASLM-1              PIC X(6)    VALUE '      '.                  
004000 77  WS-BASLM-2              PIC X(6)    VALUE '      '.                  
004100 77  WS-BASLM-3              PIC X(6)    VALUE '      '.                  
004200 77  WS-IDPROJ               PIC X(4)    VALUE '    '.                    
004300*- - - - - - - - - - - - - - - - - KONSTANTER.                            
004400 77  JA                      PIC X       VALUE 'J'.                       
004500 77  NEJ                     PIC X       VALUE 'N'.                       
004600 77  CHKP-RAK                PIC S9(9)   VALUE +0 COMP SYNC.              
004700*- - - - - - - - - - - - - - - - -CHECK-POINT.                            
004800 77  CHKP-ID                 PIC X(8)    VALUE 'W1150200'.                
004900 77  MSG-IO-AREA-LENGTH      PIC S9(9)   VALUE +32  COMP SYNC.            
005000 77  MSG-IO-AREA             PIC X(32)   VALUE SPACE.                     
005100 77  CHKP-AREA-1-LENGTH      PIC S9(9)   VALUE +32  COMP SYNC.            
005200 77  CHKP-AREA-1             PIC X(32)   VALUE SPACE.                     
005300     SKIP2                                                                
005400*- - - - - - - - - - - - - - - - - DYNAMISKA-SUB-PGM                      
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
005700     03  FELLOG              PIC X(8)    VALUE 'FELLOG'.                  
005800     EJECT                                                                
005900*- - - - - - - - - - - - - - - - - ARBETSAREOR  IMS-SEKTIONEN.            
006000*                                                                         
006100 01  IMS-WS.                                                              
006200   03  FILLER                PIC X(8)   VALUE 'IMS-WS  '.                 
006300*                                                                         
006400*- - - - - - - - - - - - - - - - - NYCKLAR TILL DLI                       
006500   03  W-KDBASLM-X.                                                       
006600     05  W-KDBASLM           PIC   X(6)       VALUE SPACE.                
006700                                                                          
006800*- - - - - - - - - - - - - - - - - STATUSKOD FRÅN IMS                     
006900   03    STATUS-WS           PIC XX.                                      
007000     88  SEGMENT-FINNS             VALUE '  '.                            
007100     88  SEGMENT-SAKNAS            VALUE 'GE'.                            
007200     88  BASEN-SLUT                VALUE 'GB'.                            
007300     88  IMS-EJ-OK                 VALUE 'XD'.                            
007400*                                                                         
007500*                                                                         
007600   03    SSA1                PIC X(96).                                   
007700   03    SSA2                PIC X(96).                                   
007800*                                                                         
007900   03    GODK-STATUSKODER.                                                
008000     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008100     EJECT                                                                
008200*- - - - - - - - - - - - - - - - - IMS-CALL FUNKTIONER                    
008300*01      -COPY W0003.                                                     
008400     EJECT                                                                
008500*- - - - - - - - - - - - - - - - - IMS - COMM-AREA                        
008600 01  FILLER                  PIC X(16) VALUE 'I-O-AREA2'.                 
008700 01  DLI-IO-AREA2.                                                        
008800     03 IO-AREA2            PIC X(550).                                   
008900*                                                                         
009000*    03 AREA  -COPY WDD201 -PRE   ARTG01- -RED IO-AREA2.                  
009100     EJECT                                                                
009200*    03 AREA  -COPY WDD211 -PRE   ARTG11- -RED IO-AREA2.                  
009300*                                                                         
009400     EJECT                                                                
009500 LINKAGE SECTION.                                                         
009600     SKIP2                                                                
009700*    -COPY W0008 -PRE MSG-.                                               
009800          05  FILLER         PIC XX.                                      
009900     SKIP2                                                                
010000     EJECT                                                                
010100*    -COPY W0008 -PRE ARTG-.                                              
010200          05  FILLER         PIC XX.                                      
010300     EJECT                                                                
010400 PROCEDURE DIVISION  USING MSG-PCB  ARTG-PCB.                             
010500     ENTRY 'DLITCBL' USING MSG-PCB  ARTG-PCB.                             
010600     SKIP2                                                                
010700     PERFORM  A-INIT                                                      
010800     MOVE +1              TO CHKP-RAK                                     
010900     PERFORM IMS-GN-ARTG01                                                
011000     PERFORM UNTIL BASEN-SLUT                                             
011100       IF SEGMENT-FINNS                                                   
011200         IF ARTG01-ART-IDPROJ = WS-IDPROJ                                 
011300* - - - - - - - - - - - - - - - - - - - - - -  FÖRSTA MARKNAD             
011400           MOVE WS-BASLM-1 TO W-KDBASLM                                   
011500                                                                          
011600           PERFORM IMS-GHNP-ARTG11                                        
011700           IF SEGMENT-FINNS                                               
011800             IF ARTG11-ART-TIBASLM = +2                                   
011900               MOVE ZERO  TO ARTG11-ART-TIBASLM                           
012000                             ARTG11-ART-TISTOMREG                         
012100               MOVE NEJ   TO ARTG11-ART-FLBLMQ                            
012200               PERFORM IMS-REPL-ARTG11                                    
012300               ADD +1     TO CHKP-RAK                                     
012400             END-IF                                                       
012500           END-IF                                                         
012600* - - - - - - - - - - - - - - - - - - - - - -  ANDRA  MARKNAD             
012700           IF WS-BASLM-2 NOT = SPACE                                      
012800                                                                          
012900             MOVE WS-BASLM-2 TO W-KDBASLM                                 
013000             PERFORM IMS-GHNP-ARTG11                                      
013100             IF SEGMENT-FINNS                                             
013200               IF ARTG11-ART-TIBASLM = +2                                 
013300                 MOVE ZERO   TO ARTG11-ART-TIBASLM                        
013400                                ARTG11-ART-TISTOMREG                      
013500                 MOVE NEJ    TO ARTG11-ART-FLBLMQ                         
013600                 PERFORM IMS-REPL-ARTG11                                  
013700                 ADD +1      TO CHKP-RAK                                  
013800               END-IF                                                     
013900             END-IF                                                       
014000           END-IF                                                         
014100* - - - - - - - - - - - - - - - - - - - - - -  TREDJE MARKNAD             
014200           IF WS-BASLM-3 NOT = SPACE                                      
014300                                                                          
014400             MOVE WS-BASLM-3 TO W-KDBASLM                                 
014500             PERFORM IMS-GHNP-ARTG11                                      
014600             IF SEGMENT-FINNS                                             
014700               IF ARTG11-ART-TIBASLM = +2                                 
014800                 MOVE ZERO   TO ARTG11-ART-TIBASLM                        
014900                                ARTG11-ART-TISTOMREG                      
015000                 MOVE NEJ    TO ARTG11-ART-FLBLMQ                         
015100                 PERFORM IMS-REPL-ARTG11                                  
015200                 ADD +1      TO CHKP-RAK                                  
015300               END-IF                                                     
015400             END-IF                                                       
015500           END-IF                                                         
015600* - - - - - - - - - - - - - - - - - - - - - -                             
015700         END-IF                                                           
015800       END-IF                                                             
015900                                                                          
016000       IF CHKP-RAK > 500                                                  
016100         PERFORM IMS-CHECKPOINT                                           
016200         MOVE +1 TO CHKP-RAK                                              
016300       END-IF                                                             
016400                                                                          
016500       PERFORM IMS-GN-ARTG01                                              
016600     END-PERFORM                                                          
016700     MOVE ZERO               TO RETURN-CODE                               
016800     GOBACK                                                               
016900     EJECT                                                                
017000     .                                                                    
017100 A-INIT SECTION.                                                          
017200     SKIP3                                                                
017300     PERFORM IMS-RESTART                                                  
017400     EJECT                                                                
017500     .                                                                    
017600*- - -  - - - IMS SEKTION                                                 
017700 IMS-RESTART SECTION.                                                     
017800     SKIP2                                                                
017900     MOVE SPACE TO MSG-IO-AREA                                            
018000     MOVE '  ' TO GODK-STATUSKODER                                        
018100     CALL CBLTDLI USING XRST MSG-PCB                                      
018200                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
018300                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
018400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
018500     PERFORM IMS-STATUSKONTROLL                                           
018600     SKIP3                                                                
018700     .                                                                    
018800 IMS-CHECKPOINT SECTION.                                                  
018900     SKIP2                                                                
019000     MOVE CHKP-ID TO MSG-IO-AREA                                          
019100     MOVE '  XD' TO GODK-STATUSKODER                                      
019200     CALL CBLTDLI USING CHKP MSG-PCB                                      
019300                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
019400                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
019500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019600     PERFORM IMS-STATUSKONTROLL                                           
019700     IF IMS-EJ-OK                                                         
019800       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
019900       CALL FELLOG                                                        
020000     END-IF                                                               
020100     .                                                                    
020200     EJECT                                                                
020300 IMS-GN-ARTG01  SECTION.                                                  
020400*                                                                         
020500     MOVE   'WLARTG01 ' TO SSA1                                           
020600     MOVE '  GB' TO GODK-STATUSKODER                                      
020700     CALL CBLTDLI USING GN   ARTG-PCB DLI-IO-AREA2 SSA1                   
020800     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
020900     PERFORM IMS-STATUSKONTROLL                                           
021000     SKIP2                                                                
021100     EJECT                                                                
021200     .                                                                    
021300 IMS-GHNP-ARTG11 SECTION.                                                 
021400*                                                                         
021500     STRING 'WLARTG11(KDBASLM  =' W-KDBASLM   ')'                         
021600             DELIMITED BY SIZE INTO SSA1                                  
021700     MOVE '  GE' TO GODK-STATUSKODER                                      
021800     CALL CBLTDLI USING GHNP ARTG-PCB DLI-IO-AREA2 SSA1                   
021900     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
022000     PERFORM IMS-STATUSKONTROLL                                           
022100     SKIP2                                                                
022200     .                                                                    
022300 IMS-REPL-ARTG11 SECTION.                                                 
022400*                                                                         
022500     MOVE '  '   TO GODK-STATUSKODER                                      
022600     CALL  CBLTDLI USING REPL ARTG-PCB DLI-IO-AREA2                       
022700     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
022800     PERFORM IMS-STATUSKONTROLL                                           
022900     SKIP2                                                                
023000     .                                                                    
023100 IMS-STATUSKONTROLL SECTION.                                              
023200*                                                                         
023300     SET STATUS-IX TO 1                                                   
023400     SEARCH GODK-STATUS AT END CALL FELLOG                                
023500         WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                         
023600         CONTINUE                                                         
023700     END-SEARCH                                                           
023800     .                                                                    
