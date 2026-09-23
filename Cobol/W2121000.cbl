000001                                                                          
000002                                                                          
000010                                                                          
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2121000.                                                
000400 AUTHOR.         KIHLBERG STEFAN.                                         
000500 DATE-WRITTEN.   12/09/26.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        SPLIT AGREEMENTS FROM SI+/EPIC BETWEEN SE AND CN/USA             
001100*                                                                         
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002401     SKIP2                                                                
002402*          --- AGREEMENTS FROM SI+                                        
002403     SELECT EPIC                       ASSIGN TO W21210D1.                
002404     SKIP2                                                                
002405*          --- AGREEMENTS FROM SI+ SE                                     
002406     SELECT EPICSE                     ASSIGN TO W21210D2.                
002407     SKIP2                                                                
002408*          --- AGREEMENTS FROM SI+ CN USA                                 
002410     SELECT EPICCN                     ASSIGN TO W21210D3.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  EPIC                                                                 
003003     RECORDING       V                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003006 01   FILLER            PIC X(82).                                        
003007                                                                          
003008*01  -COPY IS061BPA      -L.                                              
003009                                                                          
003010*01  -COPY IS061BUY      -L.                                              
003011                                                                          
003012*01  -COPY IS061OOP      -L.                                              
003013                                                                          
003014*01  -COPY IS061POB      -L.                                              
003015                                                                          
003016*01  -COPY IS061SSK      -L.                                              
003017     SKIP3                                                                
003018 FD  EPICSE                                                               
003019     RECORDING       V                                                    
003020     BLOCK CONTAINS  0.                                                   
003021                                                                          
003022 01   FILLER            PIC X(82).                                        
003023                                                                          
003024*01  RECORD -COPY IS061BPA -PRE  SE-BPA-  -L.                             
003025                                                                          
003026*01  RECORD -COPY IS061BUY -PRE  SE-BUY-  -L.                             
003027                                                                          
003028*01  RECORD -COPY IS061OOP -PRE  SE-OOP-  -L.                             
003029                                                                          
003030*01  RECORD -COPY IS061POB -PRE  SE-POB-  -L.                             
003031                                                                          
003032*01  RECORD -COPY IS061SSK -PRE  SE-SSK-  -L.                             
003033     SKIP3                                                                
003034 FD  EPICCN                                                               
003035     RECORDING       V                                                    
003036     BLOCK CONTAINS  0.                                                   
003037                                                                          
003038 01   FILLER            PIC X(82).                                        
003039                                                                          
003040*01  RECORD -COPY IS061BPA -PRE  CN-BPA-  -L.                             
003041                                                                          
003042*01  RECORD -COPY IS061BUY -PRE  CN-BUY-  -L.                             
003043                                                                          
003044*01  RECORD -COPY IS061OOP -PRE  CN-OOP-  -L.                             
003045                                                                          
003046*01  RECORD -COPY IS061POB -PRE  CN-POB-  -L.                             
003047                                                                          
003050*01  RECORD -COPY IS061SSK -PRE  CN-SSK-  -L.                             
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(8)    VALUE 'W2121000'.            
003500 77  YES                         PIC X       VALUE 'J'.                   
003600 77  NOO                         PIC X       VALUE 'N'.                   
003801                                                                          
003802 77  EPIC-EOF-SW                 PIC X       VALUE 'N'.                   
003810     88  END-OF-EPIC                         VALUE 'J'.                   
003900     EJECT                                                                
004000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004100 01  FILLER REDEFINES TODAYS-DATE.                                        
004200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004400     03  TODAYS-DATE-DAY         PIC 9(2).                                
004500     EJECT                                                                
004600 01  GENERAL-SUBPROGRAMS.                                                 
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005000     SKIP2                                                                
005100*    --- PARAMETERS TO ABEND                                              
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005600     SKIP2                                                                
005700 01  ERROR-TEXT.                                                          
005800     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
005900     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006001     EJECT                                                                
006002*    --- PARAMETRAR TILL POSTSUM                                          
006003*                                                                         
006010*01  -COPY W0005   -PRE  POSTSUM-                                         
006201     EJECT                                                                
006202 01  IN-AREA-START               PIC X(24)   VALUE                        
006203                                 'IN-AREA-START  '.                       
006204     SKIP2                                                                
006205 01  IN-AREA.                                                             
006206     03  IN-AREA-0.                                                       
006207       05  IN-IDPTYP             PIC X(3).                                
006208       05  IN-PLANT              PIC X(5).                                
006209       05  FILLER                PIC X(75).                               
006210*   03  FILLER -COPY IS061BPA  -PRE IN-BPA-  -RED  IN-AREA-0              
006211*   03  FILLER -COPY IS061BUY  -PRE IN-BUY-  -RED  IN-AREA-0              
006212*   03  FILLER -COPY IS061OOP  -PRE IN-OOP-  -RED  IN-AREA-0              
006213*   03  FILLER -COPY IS061POB  -PRE IN-POB-  -RED  IN-AREA-0              
006214*   03  FILLER -COPY IS061SSK  -PRE IN-SSK-  -RED  IN-AREA-0              
006215     EJECT                                                                
006216 01  SE-AREA-START               PIC X(24)   VALUE                        
006217                                 'SE-AREA-START  '.                       
006218     SKIP2                                                                
006219 01  SE-AREA.                                                             
006220     03  SE-AREA-0.                                                       
006221       05  SE-IDPTYP             PIC X(3).                                
006222       05  SE-PLANT              PIC X(5).                                
006223       05  FILLER                PIC X(75).                               
006224*   03  FILLER -COPY IS061BPA  -PRE SE-  -RED  SE-AREA-0                  
006225*   03  FILLER -COPY IS061BUY  -PRE SE-  -RED  SE-AREA-0                  
006226*   03  FILLER -COPY IS061OOP  -PRE SE-  -RED  SE-AREA-0                  
006227*   03  FILLER -COPY IS061POB  -PRE SE-  -RED  SE-AREA-0                  
006228*   03  FILLER -COPY IS061SSK  -PRE SE-  -RED  SE-AREA-0                  
006229     EJECT                                                                
006230 01  CN-AREA-START               PIC X(24)   VALUE                        
006231                                 'CN-AREA-START  '.                       
006232     SKIP2                                                                
006233 01  CN-AREA.                                                             
006234     03  CN-AREA-0.                                                       
006235       05  CN-IDPTYP             PIC X(3).                                
006236       05  CN-PLANT              PIC X(5).                                
006237       05  FILLER                PIC X(75).                               
006238*   03  FILLER -COPY IS661BPA  -PRE CN-  -RED  CN-AREA-0                  
006239*   03  FILLER -COPY IS061BUY  -PRE CN-  -RED  CN-AREA-0                  
006240*   03  FILLER -COPY IS061OOP  -PRE CN-  -RED  CN-AREA-0                  
006241*   03  FILLER -COPY IS061POB  -PRE CN-  -RED  CN-AREA-0                  
006250*   03  FILLER -COPY IS061SSK  -PRE CN-  -RED  CN-AREA-0                  
006300     EJECT                                                                
006400 PROCEDURE DIVISION.                                                      
006500 MAIN SECTION.                                                            
006700     SKIP2                                                                
006800                                                                          
006900     PERFORM A-INIT                                                       
007010     PERFORM S01-READ-EPIC                                                
007100     PERFORM UNTIL END-OF-EPIC                                            
007200       IF IN-PLANT = 'BP2TW' OR '1441 '                                   
007210         PERFORM S12-WRITE-EPICSE                                         
007300       ELSE                                                               
007310         PERFORM S13-WRITE-EPICCN                                         
007400       END-IF                                                             
007500                                                                          
007600                                                                          
007700                                                                          
007810       PERFORM S01-READ-EPIC                                              
007900     END-PERFORM                                                          
008000                                                                          
008100                                                                          
008200     PERFORM Z-FINIT                                                      
008300                                                                          
008400     MOVE ZERO TO RETURN-CODE                                             
008500     GOBACK                                                               
008600     .                                                                    
008700     EJECT                                                                
008800 A-INIT SECTION.                                                          
008901                                                                          
008910     OPEN INPUT  EPIC                                                     
009001                                                                          
009002     OPEN OUTPUT EPICSE                                                   
009010                 EPICCN                                                   
009100     SKIP2                                                                
009200     ACCEPT TODAYS-DATE  FROM DATE                                        
009310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009400     .                                                                    
009500     EJECT                                                                
009501                                                                          
009600 Z-FINIT SECTION.                                                         
009701     CLOSE EPIC                                                           
009702           EPICSE                                                         
009710           EPICCN                                                         
009801     SKIP2                                                                
009802     MOVE 'S' TO POSTSUM-OPKOD                                            
009810     CALL POSTSUM USING POSTSUM-PARM                                      
009900     .                                                                    
010001     EJECT                                                                
010002 S01-READ-EPIC  SECTION.                                                  
010003     READ EPIC INTO IN-AREA                                               
010004     AT END                                                               
010005        MOVE HIGH-VALUE TO IN-AREA                                        
010006        SET END-OF-EPIC TO TRUE                                           
010007                                                                          
010008     NOT AT END                                                           
010009        MOVE 'EPIC'  TO POSTSUM-FDNAMN                                    
010010        MOVE 'W21210D1' TO POSTSUM-DDNAMN2                                
010012        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
010013        CALL POSTSUM USING POSTSUM-PARM                                   
010014     END-READ                                                             
010020     .                                                                    
010101     EJECT                                                                
010102 S12-WRITE-EPICSE SECTION.                                                
010103                                                                          
010105     EVALUATE IN-IDPTYP                                                   
010106        WHEN 'BPA'                                                        
010107          WRITE SE-BPA-RECORD FROM IN-BPA-IS061BPA                        
010108        WHEN 'BUY'                                                        
010109          WRITE SE-BUY-RECORD FROM IN-BUY-IS061BUY                        
010110        WHEN 'OOP'                                                        
010111          WRITE SE-OOP-RECORD FROM IN-OOP-IS061OOP                        
010112        WHEN 'POB'                                                        
010113          WRITE SE-POB-RECORD FROM IN-POB-IS061POB                        
010114        WHEN 'SSK'                                                        
010115          WRITE SE-SSK-RECORD FROM IN-SSK-IS061SSK                        
010116     END-EVALUATE                                                         
010118                                                                          
010119     MOVE SE-IDPTYP TO POSTSUM-TRANSTYP                                   
010120     MOVE 'EPICSE' TO POSTSUM-FDNAMN                                      
010121     MOVE 'W21210D2' TO POSTSUM-DDNAMN2                                   
010122     CALL POSTSUM USING POSTSUM-PARM                                      
010123     .                                                                    
010124     EJECT                                                                
010125 S13-WRITE-EPICCN SECTION.                                                
010126                                                                          
010128     EVALUATE IN-IDPTYP                                                   
010129        WHEN 'BPA'                                                        
010130          WRITE CN-BPA-RECORD FROM IN-BPA-IS061BPA                        
010131        WHEN 'BUY'                                                        
010132          WRITE CN-BUY-RECORD FROM IN-BUY-IS061BUY                        
010133        WHEN 'OOP'                                                        
010134          WRITE CN-OOP-RECORD FROM IN-OOP-IS061OOP                        
010135        WHEN 'POB'                                                        
010136          WRITE CN-POB-RECORD FROM IN-POB-IS061POB                        
010137        WHEN 'SSK'                                                        
010138          WRITE CN-SSK-RECORD FROM IN-SSK-IS061SSK                        
010139     END-EVALUATE                                                         
010140                                                                          
010141                                                                          
010142     MOVE CN-IDPTYP TO POSTSUM-TRANSTYP                                   
010143     MOVE 'EPICCN' TO POSTSUM-FDNAMN                                      
010144     MOVE 'W21210D3' TO POSTSUM-DDNAMN2                                   
010145     CALL POSTSUM USING POSTSUM-PARM                                      
010150     .                                                                    
010300     EJECT                                                                
010400 S99-ABEND SECTION.                                                       
010500                                                                          
010601     SKIP2                                                                
010602     MOVE 'S' TO POSTSUM-OPKOD                                            
010610     CALL POSTSUM USING POSTSUM-PARM                                      
010700     CALL ABEND USING RKOD-ABEND                                          
010800     .                                                                    
