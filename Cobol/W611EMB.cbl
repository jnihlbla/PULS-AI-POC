000010*COMPOPT STDSUB=YES                                                       
000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W611EMB0.                                                
000400*AUTHOR.         MÅNS SAMUELSSON.                                         
000500*DATE-WRITTEN.   95/04/12.                                                
000600                                                                          
000800*    FUNKTION:                                                            
000900*        KONVERTERAR KDEMBISO TILL KDLAGEMB                               
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003201                                                                          
003210*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(8)    VALUE 'W611EMB0'.            
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003510 77  OK                          PIC X       VALUE SPACE.                 
003520 77  FEL                         PIC X       VALUE 'F'.                   
003600 77  EMB-IX                      PIC S9(9)  VALUE ZERO COMP SYNC.         
003800     EJECT                                                                
005500 01  FELTEXT.                                                             
005600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006100     EJECT                                                                
006200*      --- EMBALLAGE-KODER MED ÖVERSÄTTNINGAR                             
006220*01    -COPY W611EMB3                                                     
006230       EJECT                                                              
006250 LINKAGE SECTION.                                                         
006251                                                                          
006252 01  -COPY W611EMB                                                        
006260     EJECT                                                                
006300 PROCEDURE DIVISION USING EMB-W611EMB.                                    
006400     SKIP2                                                                
006500                                                                          
006600     MOVE OK                TO EMB-KDSVAR                                 
006951     MOVE 1                     TO EMB-IX                                 
006952     PERFORM UNTIL EMB-IX       >  TAB-EMBQ3-MAX OR                       
006953          TAB-KOD (EMB-IX)      = EMB-KDEMBISO                            
006954       ADD 1                    TO EMB-IX                                 
006955     END-PERFORM                                                          
006956                                                                          
006957     IF EMB-IX                  > TAB-EMBQ3-MAX                           
006958         MOVE SPACE TO          EMB-KDLAGEMB                              
006959      ELSE                                                                
006960         IF TAB-KOD (EMB-IX)        =  EMB-KDEMBISO                       
006961             MOVE TAB-TEXT (EMB-IX) TO EMB-KDLAGEMB                       
006962         ELSE                                                             
006963             MOVE SPACE             TO EMB-KDLAGEMB                       
006964             MOVE FEL               TO EMB-KDSVAR                         
006965         END-IF                                                           
006966     END-IF                                                               
006970                                                                          
008100     MOVE ZERO TO RETURN-CODE                                             
008200     GOBACK                                                               
008300     .                                                                    
008400     EJECT                                                                
