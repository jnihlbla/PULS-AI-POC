000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W2220100.                                                
000800*AUTHOR.         TOMAS.                                                   
000900*DATE-WRITTEN.   JAN  1979.                                               
001000                                                                          
001100*        FUNKTION.                                                        
001200*                PROGRAMMET SKRIVER KONTROLLERADE                         
001300*                OCH REDIGERADE TRANSAKTIONER PÅ                          
001400*                TRANSFILEN W09257.                                       
001500*        INDATA.                                                          
001600*                TRANSAKTIONSTYPER   RP1 - RP7                            
001700*        UTDATA.                                                          
001800*                TRANSFIL W09257.    DDNAMN  W09206DJ.                    
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100 INPUT-OUTPUT SECTION.                                                    
002200 FILE-CONTROL.                                                            
002300     SELECT  W09257  ASSIGN TO UT-S-W09206DJ.                             
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 FILE SECTION.                                                            
002700     SKIP2                                                                
002800 FD  W09257                                                               
002900     RECORDING V                                                          
003000     BLOCK CONTAINS 0 RECORDS.                                            
003100     SKIP2                                                                
003200*01  RP1-UTPOST      -COPY W222RP1  -L.                                   
003400     SKIP2                                                                
003500*01  RP2-UTPOST      -COPY W222RP2  -L.                                   
003700     SKIP2                                                                
003800*01  RP3-UTPOST      -COPY W222RP3  -L.                                   
004000     SKIP2                                                                
004100*01  RP4-UTPOST      -COPY W222RP4  -L.                                   
004300     SKIP2                                                                
004400*01  RP5-UTPOST      -COPY W222RP5  -L.                                   
004600     SKIP2                                                                
004700*01  RP6-UTPOST      -COPY W222RP6  -L.                                   
004900     SKIP2                                                                
005000*01  RP7-UTPOST      -COPY W222RP7  -L.                                   
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005310                                                                          
005400*    -- CHECKED BY WY2000                                                 
005500 77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.           
005600 77  IDPGM                   PIC X(8)  VALUE 'W2220100'.                  
005700 77  FL-FIRST                PIC X     VALUE 'J'.                         
006000     SKIP3                                                                
006100 01  KONSTANTER.                                                          
006200     03  JA                  PIC X(1)  VALUE 'J'.                         
006300     03  NEJ                 PIC X(1)  VALUE 'N'.                         
006400     SKIP3                                                                
006500 01  W-INDEX.                                                             
006600     03  IX                  PIC S9(9) VALUE ZERO COMP SYNC.              
006700     EJECT                                                                
006800 LINKAGE SECTION.                                                         
006900     SKIP2                                                                
007000 01  TRANS-PARM.                                                          
007100     03  KOD                 PIC X(3).                                    
007200     03  FELTAB OCCURS 100.                                               
007300         05  FELKOD          PIC X(3).                                    
007400         05  FELTEXT         PIC X(15).                                   
007500     03  FILLER              PIC X(3).                                    
007600     03  INPOST              PIC X(100).                                  
007700     03  UTAREA.                                                          
007800*        05  FILLER -COPY W092W001.                                       
008000         05  UTPOST          PIC X(214).                                  
008100     SKIP2                                                                
008200*        05  FILLER -COPY W222RP1 -PRE RP1- -RED UTPOST.                  
008400     SKIP2                                                                
008500*        05  FILLER -COPY W222RP2 -PRE RP2- -RED UTPOST.                  
008700     SKIP2                                                                
008800*        05  FILLER -COPY W222RP3 -PRE RP3- -RED UTPOST.                  
009000     SKIP2                                                                
009100*        05  FILLER -COPY W222RP4 -PRE RP4- -RED UTPOST.                  
009300     SKIP2                                                                
009400*        05  FILLER -COPY W222RP5 -PRE RP5- -RED UTPOST.                  
009600     SKIP2                                                                
009700*        05  FILLER -COPY W222RP6 -PRE RP6- -RED UTPOST.                  
009900     SKIP2                                                                
010000*        05  FILLER -COPY W222RP7 -PRE RP7- -RED UTPOST.                  
010200     EJECT                                                                
010300 PROCEDURE DIVISION USING TRANS-PARM.                                     
010500     SKIP2                                                                
010600******************************************************************        
010700*    PROGRAMMET ANROPAS DYNAMISKT AV W09206.                     *        
010800******************************************************************        
010900     SKIP2                                                                
011000 STYR SECTION.                                                            
011100     SKIP2                                                                
011200     IF FL-FIRST = JA                                                     
011300         MOVE NEJ TO FL-FIRST                                             
011400         OPEN OUTPUT W09257                                               
011410     END-IF                                                               
011500     IF KOD = 'EOF'                                                       
011600       CLOSE W09257                                                       
011700       GOBACK                                                             
011800     END-IF                                                               
011900     SKIP2                                                                
012000     IF  IDPTYP = 'RP1'                                                   
012100       WRITE RP1-UTPOST FROM RP1-W222RP1                                  
012200     ELSE                                                                 
012300       IF  IDPTYP = 'RP2'                                                 
012400         PERFORM A-TREND-KONTROLL                                         
012500       ELSE                                                               
012600         IF  IDPTYP = 'RP3'                                               
012700           WRITE RP3-UTPOST FROM RP3-W222RP3                              
012800         ELSE                                                             
012900           IF  IDPTYP = 'RP4'                                             
013000             WRITE RP4-UTPOST FROM RP4-W222RP4                            
013100           ELSE                                                           
013200             IF  IDPTYP = 'RP5'                                           
013300               WRITE RP5-UTPOST FROM RP5-W222RP5                          
013400             ELSE                                                         
013500               IF  IDPTYP = 'RP6'                                         
013600                 WRITE RP6-UTPOST FROM RP6-W222RP6                        
013700               ELSE                                                       
013800                 IF  IDPTYP = 'RP7'                                       
013900                   WRITE RP7-UTPOST FROM RP7-W222RP7                      
014000                 ELSE                                                     
014100                   CONTINUE                                               
014200                 END-IF                                                   
014300               END-IF                                                     
014400             END-IF                                                       
014500           END-IF                                                         
014600         END-IF                                                           
014700       END-IF                                                             
014800     END-IF                                                               
014900     GOBACK                                                               
015000     .                                                                    
015100     EJECT                                                                
015200 A-TREND-KONTROLL SECTION.                                                
015300     SKIP3                                                                
015400     MOVE 1 TO IX                                                         
015500                                                                          
015600     IF RP2-FLABORT-TREND = NEJ                                           
015700                                                                          
015800       IF RP2-KVTREND NOT > ZERO                                          
015900         MOVE '1D3' TO FELKOD (IX)                                        
016000         MOVE SPACE TO FELTEXT (IX)                                       
016100         ADD 1 TO IX                                                      
016200       END-IF                                                             
016300       IF RP2-TITREND NOT > ZERO                                          
016400         MOVE '1D5' TO FELKOD (IX)                                        
016500         MOVE SPACE TO FELTEXT (IX)                                       
016600         ADD 1 TO IX                                                      
016700       END-IF                                                             
016800       IF RP2-RVTREND NOT > ZERO                                          
016900         MOVE '1D6' TO FELKOD (IX)                                        
017000         MOVE SPACE TO FELTEXT (IX)                                       
017100         ADD 1 TO IX                                                      
017200       END-IF                                                             
017300     END-IF                                                               
017400     MOVE HIGH-VALUE TO FELKOD (IX)                                       
017500                                                                          
017600     IF FELKOD (1) = HIGH-VALUE                                           
017700       WRITE RP2-UTPOST FROM RP2-W222RP2                                  
017800     END-IF                                                               
017900     .                                                                    
