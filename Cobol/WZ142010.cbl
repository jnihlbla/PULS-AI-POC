000100*COMPOPT XPOSIX=YES                                                       
000200                                                                          
000300 ID  DIVISION.                                                            
000400                                                                          
000500 PROGRAM-ID.    WZ142010.                                                 
000600                                                                          
000700 AUTHOR.        CLAES OLANDER.                                            
000800                                                                          
000900 DATE-WRITTEN.  NOV 15, 2005.                                             
001000                                                                          
001100 DATE-COMPILED.                                                           
001200                                                                          
001300*                                                                         
001400****************************************************************          
001500*                                                                         
001600*  DESCRIPTION                                                            
001700*  ***********                                                            
001800*                                                                         
001900*  This program is called by WZ142000 with the following                  
002000*  2 arguments:                                                           
002100                                                                          
002200*  01. A-AREA ( _COPY RZ1420NN )                                          
002300*                                                                         
002400*  02. B-AREA.                                                            
002500*                                                                         
002600*  A-AREA contains information about a mail in a specific                 
002700*  folder,                                                                
002800*                                                                         
002900*  The following 3 fields are of interest:                                
003000*                                                                         
003100*    A-RETCODE Numeric field to convay information from and               
003200*              to the invoking program, WZ142000.                         
003300*                                                                         
003400*              If the value at entry to this program is +99               
003500*              this program returns immediately to its                    
003600*              invoker.                                                   
003700*                                                                         
003800*    A-AGE     Numeric field, containing the age of the mail,             
003900*              expressed in days.                                         
004000*                                                                         
004100*    A-DATA    a 50 characters long field that contains all               
004200*              blanks with exeption for a maximum 9 digits                
004300*              long, unsigned number.                                     
004400*                                                                         
004500*              This number contains the minimum age of a                  
004600*              mail to be deleted. The age is expressed in                
004700*              days.                                                      
004800*                                                                         
004900*                                                                         
005000*  B-AREA contains 10 flags. If a flag = 'D', the corresponding           
005100*  intermediate file should be read and used as input to                  
005200*  mail-delete-commands.                                                  
005300*                                                                         
005400*                                                                         
005500*  If the number in A-DATA > A-AGE, the mail should neither be            
005600*  saved, nor deleted. We tell the invoking program this, by              
005700*  setting A-RETCODE to +96.                                              
005800*                                                                         
005900*  If the number in A-DATA <= A-AGE, the mail should be                   
006000*  saved on file-01 and deleted. We tell the invoking program             
006100*  this, by setting A-RETCODE to +01 and by setting the first             
006200*  flag-byte in B-AREA to 'D'.                                            
006300*                                                                         
006400*  IF the number in A-DATA is invalid, we demand the invoking             
006500*  program to abend. We tell the invoking program this by                 
006600*  setting A-RETCODE to +98.                                              
006700*                                                                         
006800****************************************************************          
006900*                                                                         
007000 DATA DIVISION.                                                           
007100                                                                          
007200 WORKING-STORAGE SECTION.                                                 
007300                                                                          
007400 01     W-AREA.                                                           
007500   03   W-MINAGE         PIC S9(09)    VALUE ZEROES.                      
007600   03   FILLER           PIC 9(09)     VALUE ZEROES.                      
007700 01     FILLER REDEFINES W-AREA.                                          
007800   03   W-AREA-CH OCCURS 18 TIMES                                         
007900            INDEXED BY W-INX1                                             
008000            PIC X(01).                                                    
008100                                                                          
008200 01     A-AGE-EDITED     PIC ZZZ9.                                        
008300 01     W-MINAGE-EDITED  PIC ZZZ9.                                        
008400*                                                                         
008500*****************************************************************         
008600*                                                                         
008700 LINKAGE SECTION.                                                         
008800*                                                                         
008900*       -COPY WZ1420NN  -PRE A-                                           
009000*                                                                         
009100 01     B-AREA.                                                           
009200   03   B-AREA-CH OCCURS 10 TIMES                                         
009300            INDEXED BY B-INX1                                             
009400            PIC X(01).                                                    
009500*                                                                         
009600*****************************************************************         
009700*                                                                         
009800 PROCEDURE DIVISION              USING A-AREA                             
009900                                       B-AREA.                            
010000 MAIN SECTION.                                                            
010100                                                                          
010200     IF  A-RETCODE NOT = 99                                               
010300         SET A-INX1                  TO  +1                               
010400         SET A-INX2                  TO  +40                              
010500         PERFORM UNTIL A-INX1 > A-INX2                                    
010600             OR A-DATA-CH (A-INX1) NOT = SPACE                            
010700             SET A-INX1                  UP  BY +1                        
010800         END-PERFORM                                                      
010900         IF  A-INX1 > A-INX2                                              
011000             OR A-DATA-CH (A-INX1) < '0'                                  
011100             OR A-DATA-CH (A-INX1) > '9'                                  
011200             MOVE +98                    TO  A-RETCODE                    
011300         ELSE                                                             
011400             SET A-INX3                  TO  A-INX1                       
011500             SET A-INX3                  UP  BY +8                        
011600             PERFORM UNTIL A-INX1 > A-INX3                                
011700                 OR  A-DATA-CH (A-INX1) < '0'                             
011800                 OR  A-DATA-CH (A-INX1) > '9'                             
011900                 SET A-INX1                  UP  BY +1                    
012000             END-PERFORM                                                  
012100             IF  A-DATA-CH (A-INX1) NOT = SPACE                           
012200                 MOVE +98                    TO  A-RETCODE                
012300             ELSE                                                         
012400                 MOVE ZEROES                 TO  W-MINAGE                 
012500                 MOVE +1                     TO  A-RETCODE                
012600                 SET W-INX1                  TO  +10                      
012700                 SET A-INX1                  DOWN BY +1                   
012800                 PERFORM UNTIL W-INX1   < +2                              
012900                     OR  A-DATA-CH (A-INX1) < '0'                         
013000                     MOVE A-DATA-CH (A-INX1)                              
013100                         TO  W-AREA-CH (W-INX1 - 1)                       
013200                     IF  A-INX1 = +1                                      
013300                         SET W-INX1                  TO  +1               
013400                     ELSE                                                 
013500                         SET A-INX1                  DOWN BY +1           
013600                         SET W-INX1                  DOWN BY +1           
013700                     END-IF                                               
013800                 END-PERFORM                                              
013900                 IF  A-AGE < W-MINAGE                                     
014000*                    -- ACTUAL AGE < SPECIFIED MINIMUM                    
014100                     MOVE +96                    TO  A-RETCODE            
014200                 ELSE                                                     
014300*                    -- ACTUAL AGE >= SPECIFIED MINIMUM                   
014400                     MOVE 'D'                    TO  B-AREA-CH(1)         
014500                     MOVE +01                    TO  A-RETCODE            
014600                                                                          
014700                     DISPLAY ' '                                          
014800                     DISPLAY 'SUBJECT   = ' A-SUBJECT (1 : 85)            
014900                     DISPLAY 'MAIL-ADDR = ' A-MAIL-ADDR (1 : 85)          
015000                     DISPLAY 'DATE      = ' A-DATE                        
015100                     MOVE A-AGE TO A-AGE-EDITED                           
015200                     MOVE W-MINAGE TO W-MINAGE-EDITED                     
015300                     DISPLAY 'MAIL WILL BE DELETED SINCE '                
015400                           'AGE ' A-AGE-EDITED ' IS >= '                  
015500                           'SPECIFIED ' W-MINAGE-EDITED ' DAY(S).'        
015600                                                                          
015700                 END-IF                                                   
015800             END-IF                                                       
015900         END-IF                                                           
016000     END-IF                                                               
016100                                                                          
016200     MOVE ZEROES                 TO  RETURN-CODE                          
016300                                                                          
016400     GOBACK                                                               
016500     .                                                                    
