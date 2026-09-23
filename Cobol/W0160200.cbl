000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0160200.                                                
000300 AUTHOR.         RICHARD THÖRNGREN.                                       
000400 DATE-WRITTEN.   92/04/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAM SOM TAR EMOT RECEIPT (KVITTO) FRÅN VCOM.                 
000900*                                                                         
001000*                                                                         
001100                                                                          
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500                                                                          
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900                                                                          
002000 DATA DIVISION.                                                           
002100                                                                          
002200 FILE SECTION.                                                            
002300                                                                          
002400     SKIP3                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800                                                                          
002900 77  IDPGM                       PIC X(8)    VALUE 'W0160200'.            
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003200 77  INDX                        PIC S9(9)   VALUE ZERO COMP SYNC.        
003300                                                                          
003400                                                                          
003500 01  FELTEXT.                                                             
003600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
003700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
003800                                                                          
003900                                                                          
004000                                                                          
004100 01  DYNAMISKA-SUBPROGRAM.                                                
004200*                                                                         
004300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004400     03  DSCONR                  PIC X(8)    VALUE 'DSCONR  '.            
004500     03  DSRECV                  PIC X(8)    VALUE 'DSRECV  '.            
004600     03  DSRLSE                  PIC X(8)    VALUE 'DSRLSE  '.            
004700                                                                          
004800                                                                          
004900*    --- PARAMETRAR TILL ABEND                                            
005000                                                                          
005100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005200     EJECT                                                                
005300 01  VCOM-AREA-START             PIC X(16)   VALUE                        
005400                                 'VCOM-AREA-START '.                      
005500*01  -COPY W0028 -PRE VCOM-                                               
005600     EJECT                                                                
005700 PROCEDURE DIVISION.                                                      
005800 MAIN SECTION.                                                            
005900                                                                          
006000     PERFORM A-INIT                                                       
006100     PERFORM B-VCOM-START                                                 
006200     PERFORM UNTIL VCOM-RC > ZERO                                         
006300                                                                          
006400       CALL DSRECV USING VCOM-RC                                          
006500                         VCOM-DISTID                                      
006600                         VCOM-MAXLENGTH                                   
006700                         VCOM-ACTLENGTH                                   
006800                         VCOM-DATA                                        
006900                                                                          
007000       IF VCOM-RC = ZERO                                                  
007100         PERFORM C-WRITE                                                  
007200       ELSE                                                               
007300         IF VCOM-RC NOT = 45                                              
007400           DISPLAY 'DSRECV RC         = ' VCOM-RC                         
007500           DISPLAY 'DSRECV DISTID     = ' VCOM-DISTID                     
007600           DISPLAY 'DSRECV MAXLENGTH  = ' VCOM-MAXLENGTH                  
007700           DISPLAY 'DSRECV ACTLENGTH  = ' VCOM-ACTLENGTH                  
007800           DISPLAY 'DSRECV DATA       = ' VCOM-DATA                       
007900           PERFORM S99-ABEND                                              
008000         END-IF                                                           
008100       END-IF                                                             
008200                                                                          
008300     END-PERFORM                                                          
008400                                                                          
008500     PERFORM D-VCOM-SLUT                                                  
008600                                                                          
008700     PERFORM Z-FINIT                                                      
008800                                                                          
008900     MOVE ZERO TO RETURN-CODE                                             
009000     GOBACK                                                               
009100     .                                                                    
009200                                                                          
009300     EJECT                                                                
009400 A-INIT SECTION.                                                          
009500                                                                          
009600     MOVE +512  TO VCOM-MAXLENGTH                                         
009700                                                                          
009800     .                                                                    
009900                                                                          
010000                                                                          
010100                                                                          
010200 B-VCOM-START SECTION.                                                    
010300                                                                          
010400     MOVE 'W016X1SE' TO VCOM-EXPEDITER                                    
010500     CALL DSCONR USING VCOM-RC                                            
010600                       VCOM-DISTID                                        
010700                       VCOM-SECUR                                         
010800                       VCOM-TIMEOUT                                       
010900                       VCOM-SENDERTAG                                     
011000                       VCOM-EXPEDITER                                     
011100                       VCOM-RECTYPE                                       
011200     DISPLAY 'DSCONR RC         = ' VCOM-RC                               
011300     DISPLAY 'DSCONR DISTID     = ' VCOM-DISTID                           
011400     DISPLAY 'DSCONR SECUR      = ' VCOM-SECUR                            
011500     DISPLAY 'DSCONR TIMEOUT    = ' VCOM-TIMEOUT                          
011600     DISPLAY 'DSCONR SENDERTAG  = ' VCOM-SENDERTAG                        
011700     DISPLAY 'DSCONR EXPEDITER  = ' VCOM-EXPEDITER                        
011800     DISPLAY 'DSCONR RECTYPE    = ' VCOM-RECTYPE                          
011900     .                                                                    
012000                                                                          
012100                                                                          
012200                                                                          
012300 C-WRITE SECTION.                                                         
012400                                                                          
012500     DISPLAY VCOM-DATA                                                    
012600     .                                                                    
012700                                                                          
012800     EJECT                                                                
012900 D-VCOM-SLUT SECTION.                                                     
013000                                                                          
013100     MOVE ZERO TO VCOM-RC                                                 
013200     CALL DSRLSE USING VCOM-RC                                            
013300                       VCOM-DISTID                                        
013400                       VCOM-RVALUE                                        
013500     DISPLAY 'DSRLSE RC         = ' VCOM-RC                               
013600     DISPLAY 'DSRLSE DISTID     = ' VCOM-DISTID                           
013700     DISPLAY 'DSRLSE RVALUE     = ' VCOM-RVALUE                           
013800     .                                                                    
013900                                                                          
014000                                                                          
014100                                                                          
014200 Z-FINIT SECTION.                                                         
014300     DISPLAY 'SLUT'                                                       
014400     .                                                                    
014500                                                                          
014600                                                                          
014700                                                                          
014800 S99-ABEND SECTION.                                                       
014900                                                                          
015000     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
015100     .                                                                    
