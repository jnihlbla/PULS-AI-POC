000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0162200.                                                
000300 AUTHOR.         RICHARD THÖRNGREN.                                       
000400 DATE-WRITTEN.   92/04/23                                                 
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAM SOM SÄNDER EN FIL                                        
000900*        MED HJÄLP AV VCOM.                                               
001000*                                                                         
001100                                                                          
001200 ENVIRONMENT DIVISION.                                                    
001300                                                                          
001400                                                                          
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800                                                                          
001900     SELECT W01622D1                   ASSIGN TO W01622D1.                
002000                                                                          
002100                                                                          
002200 DATA DIVISION.                                                           
002300 FILE SECTION.                                                            
002400 FD  W01622D1                                                             
002500     RECORDING       F                                                    
002600     BLOCK CONTAINS  0.                                                   
002700                                                                          
002800*01  -COPY W016221A  -L                                                   
002900                                                                          
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400                                                                          
003500 77  IDPGM                       PIC X(8)    VALUE 'W0162200'.            
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800 77  W-EOF                       PIC X(1)    VALUE 'N'.                   
003900 77  W-POSTANT-IN                PIC S9(7)   VALUE ZERO  COMP-3.          
004000                                                                          
004100                                                                          
004200                                                                          
004300 01  DYNAMISKA-SUBPROGRAM.                                                
004400   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
004500   03  W016READ                  PIC X(8)    VALUE 'W016READ'.            
004600   03  DSCONS                    PIC X(8)    VALUE 'DSCONS  '.            
004700   03  DSSEND                    PIC X(8)    VALUE 'DSSEND  '.            
004800   03  DSRLSE                    PIC X(8)    VALUE 'DSRLSE  '.            
004900                                                                          
005000                                                                          
005100*    --- PARAMETRAR TILL ABEND                                            
005200                                                                          
005300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005400     EJECT                                                                
005500 01  VCOM-AREA-START             PIC X(16)   VALUE                        
005600                                 'VCOM-AREA-START '.                      
005700                                                                          
005800*01  -COPY W0028 -PRE VCOM-                                               
005900     EJECT                                                                
006000 01  VCOM-STYR-INFO              PIC X(16)   VALUE                        
006100                                 'VCOM-STYR-INFO  '.                      
006200                                                                          
006300*01  -COPY W016221A                                                       
006400     EJECT                                                                
006500 01  FILLER                      PIC X(16)   VALUE                        
006600                                 'W016READ INFO   '.                      
006700                                                                          
006800 01  READ-W016READ.                                                       
006900   03  READ-KVLL                 PIC S9(4)   VALUE ZERO  COMP.            
007000   03  READ-KVLRECL              PIC S9(4)   VALUE ZERO  COMP.            
007100   03  READ-KDRECFM              PIC X(3)    VALUE SPACE.                 
007200   03  READ-IDDSN                PIC X(44)   VALUE SPACE.                 
007300   03  READ-KDSVAR               PIC X(1)    VALUE SPACE.                 
007400     EJECT                                                                
007500 LINKAGE SECTION.                                                         
007600                                                                          
007700 01  EXEC-PARM.                                                           
007800     03  PARM-LENGD              PIC S9(4) COMP.                          
007900     03  PARM-KORTYP             PIC X(4).                                
008000     EJECT                                                                
008100 PROCEDURE DIVISION USING EXEC-PARM.                                      
008200 MAIN SECTION.                                                            
008300                                                                          
008400     PERFORM A-INIT                                                       
008500     PERFORM B-VCOM-START                                                 
008600     PERFORM C-READ                                                       
008700                                                                          
008800     PERFORM UNTIL READ-KDSVAR = 'E' OR 'S'                               
008900                                                                          
009000       MOVE READ-KVLL TO VCOM-ACTLENGTH                                   
009100       CALL DSSEND USING VCOM-RC                                          
009200                         VCOM-DISTID                                      
009300                         VCOM-ACTLENGTH                                   
009400                         VCOM-DATA                                        
009500                                                                          
009600       IF VCOM-RC NOT = ZERO                                              
009700         DISPLAY 'DSSEND RC         = ' VCOM-RC                           
009800         DISPLAY 'DSSEND DISTID     = ' VCOM-DISTID                       
009900         DISPLAY 'DSSEND ACTLENGTH  = ' VCOM-ACTLENGTH                    
010000         DISPLAY 'DSSEND DATA       = ' VCOM-DATA                         
010100         DISPLAY '* * * * *  FEL RETURKOD FRÅN DSSEND  * * * * *'         
010200         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
010300       END-IF                                                             
010400                                                                          
010500       IF READ-KDSVAR = 'S'                                               
010600         CONTINUE                                                         
010700       ELSE                                                               
010800         PERFORM C-READ                                                   
010900         IF VCOM-DATA = LOW-VALUE                                         
011000           MOVE 'E' TO READ-KDSVAR                                        
011100         END-IF                                                           
011200       END-IF                                                             
011300     END-PERFORM                                                          
011400                                                                          
011500     PERFORM D-VCOM-SLUT                                                  
011600                                                                          
011700     PERFORM Z-FINIT                                                      
011800                                                                          
011900     MOVE ZERO TO RETURN-CODE                                             
012000     GOBACK                                                               
012100     .                                                                    
012200                                                                          
012300     EJECT                                                                
012400 A-INIT SECTION.                                                          
012500                                                                          
012600     OPEN INPUT  W01622D1                                                 
012700                                                                          
012800     READ W01622D1 INTO 221A-W016221A                                     
012900       AT END                                                             
013000         DISPLAY 'IDVCOM SAKNAS '                                         
013100         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
013200     END-READ                                                             
013300                                                                          
013400     DISPLAY 'START IDVCOM      = ' 221A-W016221A                         
013500                                                                          
013600     MOVE 221A-KDRECFM TO READ-KDRECFM                                    
013700     MOVE 221A-KVLRECL TO READ-KVLL                                       
013800                          VCOM-ACTLENGTH                                  
013900     .                                                                    
014000     EJECT                                                                
014100 B-VCOM-START SECTION.                                                    
014200                                                                          
014300     MOVE 221A-IDVCOM    TO VCOM-PARTNER                                  
014400     IF PARM-KORTYP = 'QASE'                                              
014500       MOVE 'W016X1SE'     TO VCOM-RECEIPT-PARTNER                        
014600       MOVE +1             TO VCOM-RECEIPT-RC                             
014700       MOVE +62            TO VCOM-RECEIPT-LENGTH                         
014800       MOVE 221A-W016221A  TO VCOM-RECEIPT-DATA                           
014900     ELSE                                                                 
015000       MOVE SPACE          TO VCOM-RECEIPT-PARTNER                        
015100       MOVE +0             TO VCOM-RECEIPT-RC                             
015200       MOVE +0             TO VCOM-RECEIPT-LENGTH                         
015300       MOVE SPACE          TO VCOM-RECEIPT-DATA                           
015400     END-IF                                                               
015500*    MOVE +1             TO VCOM-RECEIPT-RC                               
015600*    MOVE +62            TO VCOM-RECEIPT-LENGTH                           
015700*    MOVE 221A-W016221A  TO VCOM-RECEIPT-DATA                             
015800     MOVE 221A-TEVCOMST  TO VCOM-SENDERTAG                                
015900     MOVE 221A-IDVCINIT  TO VCOM-INITIATOR                                
016000     MOVE 'N'            TO VCOM-PRIO                                     
016100                                                                          
016200     CALL DSCONS USING VCOM-RC                                            
016300                       VCOM-DISTID                                        
016400                       VCOM-SECUR                                         
016500                       VCOM-TIMEOUT                                       
016600                       VCOM-SENDERTAG                                     
016700                       VCOM-PARTNER                                       
016800                       VCOM-RECEIPT                                       
016900                       VCOM-PRIO                                          
017000                       VCOM-INITIATOR                                     
017100                                                                          
017200     DISPLAY 'DSCONS RC         = ' VCOM-RC                               
017300     DISPLAY 'DSCONS DISTID     = ' VCOM-DISTID                           
017400     DISPLAY 'DSCONS SECUR      = ' VCOM-SECUR-DATA                       
017500     DISPLAY 'DSCONS TIMEOUT    = ' VCOM-TIMEOUT                          
017600     DISPLAY 'DSCONS SENDERTAG  = ' VCOM-SENDERTAG                        
017700     DISPLAY 'DSCONS INITIATOR  = ' VCOM-INITIATOR                        
017800     DISPLAY 'DSCONS PARTNER    = ' VCOM-PARTNER                          
017900     DISPLAY 'DSCONS PRIO       = ' VCOM-PRIO                             
018000     DISPLAY 'DSCONS RECEIPT    = ' VCOM-RECEIPT-PARTNER                  
018100                                ' ' VCOM-RECEIPT-DATA                     
018200                                                                          
018300     IF VCOM-RC NOT = ZERO                                                
018400       DISPLAY 'FEL RETURKOD FRÅN DSCONS '                                
018500       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
018600     END-IF                                                               
018700     .                                                                    
018800     EJECT                                                                
018900 C-READ SECTION.                                                          
019000                                                                          
019100     MOVE LOW-VALUE TO VCOM-DATA                                          
019200     CALL W016READ USING READ-W016READ VCOM-DATA                          
019300     IF READ-KDSVAR = SPACE                                               
019400       ADD +1 TO W-POSTANT-IN                                             
019500     END-IF                                                               
019600     .                                                                    
019700                                                                          
019800     EJECT                                                                
019900 D-VCOM-SLUT SECTION.                                                     
020000                                                                          
020100     DISPLAY ' '                                                          
020200     DISPLAY 'SLUT  KVLL = ' READ-KVLL                                    
020300             '  LRECL = ' READ-KVLRECL                                    
020400             '  RECFM = ' READ-KDRECFM                                    
020500             '   ANTAL POSTER = ' W-POSTANT-IN                            
020600     DISPLAY '      LÄST FIL = ' READ-IDDSN                               
020700             ' STATUS = ' READ-KDSVAR ' (E=LÄST TILL EOF, S=FEL)'         
020800                                                                          
020900     MOVE ZERO TO VCOM-RC                                                 
021000     MOVE +1   TO VCOM-RVALUE                                             
021100     CALL DSRLSE USING VCOM-RC                                            
021200                       VCOM-DISTID                                        
021300                       VCOM-RVALUE                                        
021400     DISPLAY ' '                                                          
021500     DISPLAY 'DSRLSE RC         = ' VCOM-RC                               
021600     DISPLAY 'DSRLSE DISTID     = ' VCOM-DISTID                           
021700     DISPLAY 'DSRLSE RVALUE     = ' VCOM-RVALUE                           
021800     IF VCOM-RC NOT = ZERO                                                
021900       DISPLAY 'FEL RETURKOD FRÅN DSRLSE '                                
022000       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
022100     END-IF                                                               
022200     .                                                                    
022300                                                                          
022400                                                                          
022500                                                                          
022600 Z-FINIT SECTION.                                                         
022700                                                                          
022800     CLOSE W01622D1                                                       
022900     .                                                                    
