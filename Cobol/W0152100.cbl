000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0152100.                                                
000300 AUTHOR.         RICHARD THÖRNGREN.                                       
000400 DATE-WRITTEN.   01/10/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAM SOM KOLLAR ATT DISPATCHERN (W00693 DB=WDP8)              
000900*        HAR KÖRT ALLA TRANSAR I DE TRANSGRUPPER (BUNTAR)                 
001000*        SOM FINNS PÅ INPUT-STYRFILEN.                                    
001100*                                                                         
001200                                                                          
001300                                                                          
001400                                                                          
001500 ENVIRONMENT DIVISION.                                                    
001600                                                                          
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000                                                                          
002100     SELECT W01521     ASSIGN TO W01521D1.                                
002200                                                                          
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500                                                                          
002600 FILE SECTION.                                                            
002700                                                                          
002800 FD  W01521                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200 01  -COPY WMSGKOM    -L                                                  
003300                                                                          
003400                                                                          
003500                                                                          
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W0152100'.            
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 77  EOF-W01521                  PIC X       VALUE 'N'.                   
004300 77  TRANS-KLAR                  PIC X       VALUE 'N'.                   
004400 77  INDX                        PIC S9(9)   VALUE ZERO COMP SYNC.        
004500 77  ANT-W01521                  PIC S9(7)   VALUE ZERO COMP-3.           
004600 77  ANT-CALL                    PIC S9(7)   VALUE ZERO COMP-3.           
004700 77  ANT-WAIT                    PIC S9(7)   VALUE ZERO COMP-3.           
004800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16  COMP SYNC.        
004900 77  WS-TIME-DELAY               PIC S9(9)   VALUE +500 COMP SYNC.        
005000                                                                          
005100                                                                          
005200 01  DYNAMISKA-SUBPROGRAM.                                                
005300   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
005400   03  W009WAIT                  PIC X(8)    VALUE 'W009WAIT'.            
005500   03  W006KOM                   PIC X(8)    VALUE 'W006KOM '.            
005600                                                                          
005700                                                                          
005800     EJECT                                                                
005900 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
006000*01  -COPY WMSGKOM                                                        
006100                                                                          
006200                                                                          
006300     EJECT                                                                
006400 LINKAGE SECTION.                                                         
006500                                                                          
006600*01  -COPY W0009   -PRE MSG-                                              
006700     EJECT                                                                
006800*01  -COPY W0008  -PRE DISP-                                              
006900     05  FILLER                  PIC X.                                   
007000                                                                          
007100*01  -COPY W0008  -PRE WDP8-                                              
007200     05  FILLER                  PIC X.                                   
007300                                                                          
007400     EJECT                                                                
007500 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB WDP8-PCB.                     
007600 MAIN SECTION.                                                            
007700     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB WDP8-PCB.                     
007800                                                                          
007900     PERFORM A-INIT                                                       
008000     PERFORM S01-LAES-W01521                                              
008100     PERFORM UNTIL EOF-W01521 = JA                                        
008200       PERFORM UNTIL TRANS-KLAR = JA                                      
008300         PERFORM B-KOLLA-TRANS-GRUPP                                      
008400         IF TRANS-KLAR = NEJ                                              
008500           CALL W009WAIT USING WS-TIME-DELAY                              
008600           ADD +1 TO ANT-WAIT                                             
008700         END-IF                                                           
008800       END-PERFORM                                                        
008900       PERFORM S01-LAES-W01521                                            
009000     END-PERFORM                                                          
009100                                                                          
009200     PERFORM Z-FINIT                                                      
009300                                                                          
009400     MOVE ZERO TO RETURN-CODE                                             
009500     GOBACK                                                               
009600     .                                                                    
009700                                                                          
009800     EJECT                                                                
009900 A-INIT SECTION.                                                          
010000                                                                          
010100     OPEN INPUT  W01521                                                   
010200     .                                                                    
010300                                                                          
010400     EJECT                                                                
010500 B-KOLLA-TRANS-GRUPP SECTION.                                             
010600                                                                          
010700     MOVE '000' TO MSG-KOM-IDMFSMED                                       
010800                                                                          
010900     CALL W006KOM USING MSG-PCB                                           
011000                        DISP-PCB                                          
011100                        WDP8-PCB                                          
011200                        MSG-KOM-WMSGKOM                                   
011300                        MSG-KOM-WMSGKOM                                   
011400                                                                          
011500     IF MSG-KOM-IDMFSMED = '078' OR '101'                                 
011600*                          SAKNAS ELLER KLAR                              
011700       MOVE JA TO TRANS-KLAR                                              
011800     ELSE                                                                 
011900       IF MSG-KOM-IDMFSMED = '130' OR '034'                               
012000*                          STARTAD ELLER VÄNTAR PÅ START                  
012100         MOVE NEJ TO TRANS-KLAR                                           
012200         IF ANT-WAIT > +500                                               
012300*                       500 GÅNGER 5 SEKUNDER = TOTALT 41MIN 40SEK        
012400           MOVE JA TO TRANS-KLAR                                          
012500           DISPLAY 'VÄNTETIDEN FÖR LÅNG'                                  
012600           MOVE ZERO TO ANT-WAIT                                          
012700         END-IF                                                           
012800       ELSE                                                               
012900         MOVE NEJ TO TRANS-KLAR                                           
013000         DISPLAY 'FELKOD FRÅN W006KOM ' MSG-KOM-IDMFSMED                  
013100         IF ANT-WAIT > +0                                                 
013200*                       TOTALT 5 SEKUNDER                                 
013300           MOVE JA TO TRANS-KLAR                                          
013400           DISPLAY 'VÄNTETIDEN FÖR LÅNG'                                  
013500           MOVE ZERO TO ANT-WAIT                                          
013600         END-IF                                                           
013700****     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
013800       END-IF                                                             
013900     END-IF                                                               
014000                                                                          
014100     DISPLAY 'MSG-KOM-IDMFSMED    ' MSG-KOM-IDMFSMED                      
014200     ADD +1 TO ANT-CALL                                                   
014300     .                                                                    
014400                                                                          
014500     EJECT                                                                
014600 S01-LAES-W01521  SECTION.                                                
014700                                                                          
014800     READ W01521 INTO MSG-KOM-WMSGKOM                                     
014900     AT END                                                               
015000       MOVE JA TO EOF-W01521                                              
015100                                                                          
015200     NOT AT END                                                           
015300       DISPLAY MSG-KOM-WMSGKOM                                            
015400       ADD +1 TO ANT-W01521                                               
015500       MOVE NEJ TO TRANS-KLAR                                             
015600     END-READ                                                             
015700     .                                                                    
015800                                                                          
015900     EJECT                                                                
016000 Z-FINIT SECTION.                                                         
016100                                                                          
016200     CLOSE W01521                                                         
016300                                                                          
016400     DISPLAY 'ANTAL POSTER IN = ' ANT-W01521                              
016500     DISPLAY 'ANTAL CALL      = ' ANT-CALL                                
016600     .                                                                    
