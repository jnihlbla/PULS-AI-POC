000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W005MCHK.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   17/09/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
001000*        CHECK NOT VALID CHARACTERS IN A MAIL ADDRESS                     
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700 ENVIRONMENT DIVISION.                                                    
001800                                                                          
001900 DATA DIVISION.                                                           
002000                                                                          
002100 WORKING-STORAGE SECTION.                                                 
002200                                                                          
002300 77  IDPGM                       PIC X(8)    VALUE 'W005MCHK'.            
002400 77  YES                         PIC X       VALUE 'J'.                   
002500 77  NOO                         PIC X       VALUE 'N'.                   
002600 77  INDX                        PIC S9(5)   VALUE +0   COMP SYNC.        
002700 77  INDX-MAX                    PIC S9(5)   VALUE +50  COMP SYNC.        
002800 77  W-KDFEL                     PIC S9(3) COMP-3.                        
002801 77  W-ANT-A                     PIC S9(03)   VALUE +0.                   
002802 77  W-ANT-B                     PIC S9(03)   VALUE +0.                   
002803 77  W-TAB-MAIL                  PIC X(01)    VALUE SPACE.                
002804                                                                          
002811 01  TEST-IDMAIL                 PIC X.                                   
002812 01  FILLLER REDEFINES TEST-IDMAIL.                                       
002813     03 MAIL-IDMAIL              PIC X.                                   
002821        88 MAIL-NOT-VALID-CHARACTERS VALUE                                
002830        '<' '>' '!' '#' '$' '%' '&' '*' '+' '/' '=' '?'                   
002900        '^' '`' '{' '|' '}' '~' ';' ':'.                                  
002910                                                                          
003000 01  TABLE-IDMAIL.                                                        
003200     03 TAB-IDMAIL OCCURS 50     PIC X.                                   
003300                                                                          
003400 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
003500 01  FILLER REDEFINES TODAYS-DATE.                                        
003600     03  TODAYS-DATE-YEAR        PIC 9(2).                                
003700     03  TODAYS-DATE-MONTH       PIC 9(2).                                
003800     03  TODAYS-DATE-DAY         PIC 9(2).                                
003900     EJECT                                                                
004000 01  GENERAL-SUBPROGRAMS.                                                 
004100*                                                                         
004200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004300     SKIP2                                                                
004400*    --- PARAMETERS TO ABEND                                              
004500                                                                          
004600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
004700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
004800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
004900     SKIP2                                                                
005000 01  ERROR-TEXT.                                                          
005100     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
005200     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005300     EJECT                                                                
005400 LINKAGE  SECTION.                                                        
005500                                                                          
005600*   -COPY W005MCHK                                                        
005700                                                                          
005800 PROCEDURE DIVISION USING MCHK-W005MCHK.                                  
005900                                                                          
006000 MAIN SECTION.                                                            
006100                                                                          
006200     PERFORM B-CHECK-MAIL-ADDRESS                                         
006300                                                                          
006400     MOVE ZERO TO RETURN-CODE                                             
006500     GOBACK                                                               
006600     .                                                                    
006700     EJECT                                                                
006800 B-CHECK-MAIL-ADDRESS SECTION.                                            
006900                                                                          
007011     MOVE +0                     TO W-ANT-A                               
007012     MOVE +0                     TO W-ANT-B                               
007013     MOVE SPACE                  TO MCHK-BETEXT                           
007014     MOVE +0                     TO MCHK-KDFEL                            
007020                                                                          
007100     MOVE MCHK-IDMAIL            TO TABLE-IDMAIL                          
007200     MOVE +1 TO INDX                                                      
007300     PERFORM UNTIL INDX > INDX-MAX                                        
007400                OR MCHK-KDFEL > +0                                        
007410        MOVE TAB-IDMAIL(INDX)    TO TEST-IDMAIL                           
007500        IF MAIL-NOT-VALID-CHARACTERS                                      
007600           MOVE TAB-IDMAIL(INDX) TO MCHK-BETEXT                           
007670           MOVE +1               TO MCHK-KDFEL                            
007720        ELSE                                                              
007722           IF INDX > +1                                                   
007725              IF TAB-IDMAIL(INDX - 1) = SPACE                             
007726             AND TAB-IDMAIL(INDX)     > SPACE                             
007728                 MOVE +1         TO MCHK-KDFEL                            
007730              END-IF                                                      
007731           END-IF                                                         
007732                                                                          
007734           IF TAB-IDMAIL(INDX) = '@'                                      
007735              ADD +1             TO W-ANT-A                               
007736              MOVE TAB-IDMAIL(INDX) TO W-TAB-MAIL                         
007737           END-IF                                                         
007738                                                                          
007739           IF W-TAB-MAIL  = '@'                                           
007740              IF TAB-IDMAIL(INDX) = '.'                                   
007741                 ADD +1          TO W-ANT-B                               
007742              END-IF                                                      
007743           END-IF                                                         
007750        END-IF                                                            
007800        ADD +1                   TO INDX                                  
007900     END-PERFORM                                                          
008000                                                                          
008010     IF MCHK-KDFEL = +0                                                   
008020        IF W-ANT-A = +0                                                   
008022           MOVE '@'              TO MCHK-BETEXT                           
008023           MOVE +2               TO MCHK-KDFEL                            
008031        ELSE                                                              
008032           IF W-ANT-A > +1                                                
008033              MOVE '@'           TO MCHK-BETEXT                           
008034              MOVE +1            TO MCHK-KDFEL                            
008036           ELSE                                                           
008037              IF W-ANT-B = +0                                             
008038                 MOVE '.DOMAIN'  TO MCHK-BETEXT                           
008039                 MOVE +2         TO MCHK-KDFEL                            
008040              END-IF                                                      
008041           END-IF                                                         
008042        END-IF                                                            
008050     END-IF                                                               
008200     .                                                                    
008300     EJECT                                                                
