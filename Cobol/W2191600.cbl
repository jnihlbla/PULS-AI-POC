000100 PROCESS DYNAM                                                            
000020*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000030*                                                                         
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2191600.                                                
000300 AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000400 DATE-WRITTEN.   FEB 2006.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*                LÄSER KAMPANJDATA FRÅN QW90.                             
001500*                                                                         
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002810     SELECT W21916UT                   ASSIGN TO W21916D1.                
004000     SKIP2                                                                
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP3                                                                
004400 FILE SECTION.                                                            
004410     SKIP3                                                                
004430                                                                          
004440 FD  W21916UT                                                             
004450     RECORDING F                                                          
004470     BLOCK CONTAINS  0.                                                   
004480                                                                          
004481                                                                          
004482*01  POST -COPY W21916  -PRE  UT-  -L.                                    
004491                                                                          
004493                                                                          
009100*                                                                         
009200     EJECT                                                                
009300 WORKING-STORAGE SECTION.                                                 
009400*    -- CHECKED BY WY2000                                                 
009500 77  IDPGM                       PIC X(8)    VALUE 'W2191600'.            
010500 77  JA                          PIC X       VALUE 'J'.                   
010600 77  NEJ                         PIC X       VALUE 'N'.                   
010700     SKIP2                                                                
010800 01  WS.                                                                  
010847     03  WS-ANTAL-UTDATA         PIC 9(9)    VALUE ZERO.                  
010848     03  WS-ANTAL-INSERT         PIC 9(9)    VALUE ZERO.                  
010849     03  WS-ANTAL-UPDATE         PIC 9(9)    VALUE ZERO.                  
010851     03 FILLER                   PIC X(16)   VALUE                        
010852                                             'WS-DB2-SEKTION'.            
010860     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
012700     EJECT                                                                
014100 01  POST-ANT                    PIC S9(7)   VALUE ZERO COMP-3.           
014110     EJECT                                                                
014120*    --- PARAMETRAR TILL POSTSUM                                          
014130*                                                                         
014140*01  -COPY W0005   -PRE  POSTSUM-                                         
014200                                                                          
014300 01  FILLER          PIC X(16)   VALUE 'SPAR-FAELT START'.                
015800                                                                          
015900 77  RKOD-ABEND-UTAN-DUMP       PIC S9(4)   VALUE +16 COMP SYNC.          
016000 77  RKOD-ABEND-MED-DUMP        PIC S9(4)   VALUE +1000 COMP SYNC.        
016010*                                                                         
016020 01   ABEND-TX.                                                           
016030  02  ABEND-RUBRIK.                                                       
016040   03 FILLER              PIC X(16)   VALUE '--------->     *'.           
016050   03 FILLER              PIC X(16)   VALUE '*** A B E N D **'.           
016060   03 FILLER              PIC X(16)   VALUE '**     <--------'.           
016070*                                                                         
016080  02  ABEND-RAD1.                                                         
016090   03 FILLER              PIC X(120)  VALUE ' '.                          
016091*                                                                         
016092  02  ABEND-RAD2.                                                         
016093   03 FILLER              PIC X(120)  VALUE ' '.                          
016094*                                                                         
016095 01      PRM.                                                             
016096  02     PARM-MODE           PIC X(4)    VALUE ' '.                       
016097  02     PARM-REST           PIC X(50)   VALUE ' '.                       
016100                                                                          
018100 01  FELTEXT.                                                             
018200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
018300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
018400                                                                          
018500 77  W21916-EOF-SW               PIC X       VALUE 'N'.                   
018600     88  END-OF-W21916                       VALUE 'J'.                   
018900     EJECT                                                                
019000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
019100 01  FILLER REDEFINES DAGENS-DATUM.                                       
019200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
019300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
019400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
019500     SKIP3                                                                
019920                                                                          
020100 01  DYNAMISKA-SUBPROGRAM.                                                
020500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
020600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
020800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG '.             
021000     EJECT                                                                
021100 01  UT-AREA-START               PIC X(24)   VALUE                        
021200                                             'UT-AREA-START'.             
021300     SKIP2                                                                
021400*01  AREA -COPY W21916     -PRE UT-                                       
022800                                                                          
022900     EJECT                                                                
023201*                                                                         
023202 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
023203       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
023204                                                                          
023205 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
023206 01  DB2-WS.                                                              
023207     03  SQLCODE-WS              PIC  9(3)   VALUE ZERO.                  
023208         88  CURSOR-OK                       VALUE  000.                  
023209         88  ROW-FOUND                       VALUE  000.                  
023210         88  ROW-NOTFOUND                    VALUE  100.                  
023211         88  ROW-DUPLICATE                   VALUE  803.                  
023212         88  ROW-SEVERAL                     VALUE  811.                  
023213         88  RESOURCE-WRONG                  VALUE  904.                  
023214     03  GOOD-SQLCODECODES.                                               
023215         05  GOOD-SQLCODE OCCURS 5                                        
023216             INDEXED BY SQLCODE-IX PIC 9(3).                              
023217*                                                                         
023218*                                                                         
023219*    ---  DB2 HOST-COPYTEXTER                                             
023220     EJECT                                                                
023230 01  FILLER                      PIC X(16)   VALUE 'TP1KAMP-AREA'.        
023231*01  AREA -COPY TP1KAMP  -PRE TP1KAMP-                                    
023232     EJECT                                                                
023233 01  FILLER                      PIC X(16)   VALUE 'TP1KAMP-DCL '.        
023234       EXEC SQL INCLUDE TP1KAMP  END-EXEC.                                
023235     EJECT                                                                
023236 01  FILLER                      PIC X(16)   VALUE 'TP1ARTK-AREA'.        
023237*01  AREA -COPY TP1ARTK  -PRE TP1ARTK-                                    
023238     EJECT                                                                
023239 01  FILLER                      PIC X(16)   VALUE 'TP1ARTK-DCL '.        
023240       EXEC SQL INCLUDE TP1ARTK  END-EXEC.                                
023241                                                                          
023242     EJECT                                                                
023243 PROCEDURE DIVISION.                                                      
023244 MAIN SECTION.                                                            
023245     ENTRY 'DLITCBL'.                                                     
023600                                                                          
023700     PERFORM A-INIT                                                       
023800                                                                          
023900     PERFORM DB2-DCL-OPN-TP1ARTK-CRS                                      
024100                                                                          
024101     IF ROW-FOUND                                                         
024110       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
024111     END-IF                                                               
024120                                                                          
024200     PERFORM UNTIL ROW-NOTFOUND                                           
024435                                                                          
024436       IF TP1KAMP-KDKAMP = 'W'                                            
024437       OR TP1KAMP-KDKAMP = 'Q'                                            
024438         MOVE TP1ARTK-IDARTNR TO UT-IDARTNR                               
024439         MOVE TP1KAMP-IDKAMP TO UT-IDKAMP                                 
024440         MOVE TP1KAMP-IDKAMP-GRP                                          
024441                             TO UT-IDKAMP-GRP                             
024442         MOVE TP1KAMP-TISTADAT-KAMP                                       
024443                             TO UT-TISTADAT-KAMP                          
024444         MOVE TP1KAMP-TISTODAT-KAMP                                       
024445                             TO UT-TISTODAT-KAMP                          
024446         MOVE TP1KAMP-KDKAMP TO UT-KDKAMP                                 
024447         PERFORM S01-SKRIV-UTFIL                                          
024448       END-IF                                                             
024540                                                                          
024550       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
026092                                                                          
026100     END-PERFORM                                                          
026102                                                                          
026103     PERFORM DB2-CLOSE-TP1ARTK-CRS                                        
026104                                                                          
030100     PERFORM Z-FINIT                                                      
030200                                                                          
030300     MOVE ZERO TO RETURN-CODE                                             
030400     GOBACK                                                               
030500     .                                                                    
030600     EJECT                                                                
030700 A-INIT SECTION.                                                          
030800     SKIP2                                                                
030900                                                                          
031000     OPEN OUTPUT  W21916UT                                                
031300     ACCEPT DAGENS-DATUM  FROM DATE                                       
031400                                                                          
031600     INITIALIZE GOOD-SQLCODECODES                                         
031601     .                                                                    
031602     EJECT                                                                
031800                                                                          
031900 Z-FINIT SECTION.                                                         
032000                                                                          
032200     DISPLAY 'ANTAL UTDATA : ' WS-ANTAL-UTDATA                            
032300     CLOSE W21916UT                                                       
032600     .                                                                    
033600     EJECT                                                                
033700 S01-SKRIV-UTFIL SECTION.                                                 
033800     SKIP2                                                                
033900     WRITE UT-POST           FROM UT-AREA                                 
034000     ADD 1                   TO WS-ANTAL-UTDATA                           
034100     .                                                                    
034200     EJECT                                                                
048930 DB2-DCL-OPN-TP1ARTK-CRS  SECTION.                                        
048940     MOVE 'DB2-DCL-OPN-TP1ARTK   ' TO  WS-DB2-SEKTION                     
048950                                                                          
048960     MOVE 000100  TO GOOD-SQLCODECODES                                    
048970                                                                          
048980     EXEC SQL                                                             
048990         DECLARE TP1ARTK-CRS CURSOR FOR                                   
048992           SELECT  A.IDARTNR                                              
048993                  ,B.IDKAMP                                               
048994                  ,B.IDKAMP_GRP                                           
048995                  ,B.TISTADAT_KAMP                                        
048996                  ,B.TISTODAT_KAMP                                        
048997                  ,B.KDKAMP                                               
048998                                                                          
048999           FROM    TP1ARTK A                                              
049000                  ,TP1KAMP B                                              
049001                                                                          
049002           WHERE   A.IDKAMP  =  B.IDKAMP                                  
049003                                                                          
049004           ORDER BY B.IDKAMP                                              
049005     END-EXEC                                                             
049006                                                                          
049007     MOVE 000100  TO GOOD-SQLCODECODES                                    
049008     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
049009     .                                                                    
049010     SKIP3                                                                
049011 DB2-FETCH-TP1ARTK-CRS  SECTION.                                          
049012     MOVE 'DB2-FETCH-TP1ARTK   ' TO  WS-DB2-SEKTION                       
049013     SKIP2                                                                
049014     MOVE 000100  TO GOOD-SQLCODECODES                                    
049015     EXEC SQL                                                             
049016         FETCH TP1ARTK-CRS INTO                                           
049018                    :TP1ARTK-IDARTNR                                      
049019                   ,:TP1KAMP-IDKAMP                                       
049020                   ,:TP1KAMP-IDKAMP-GRP                                   
049023                   ,:TP1KAMP-TISTADAT-KAMP                                
049024                   ,:TP1KAMP-TISTODAT-KAMP                                
049025                   ,:TP1KAMP-KDKAMP                                       
049026     END-EXEC                                                             
049027                                                                          
049028     MOVE SQLCODE TO SQLCODE-WS                                           
049029     PERFORM DB2-STATUS-CHECK                                             
049030     .                                                                    
049031     SKIP3                                                                
049032 DB2-CLOSE-TP1ARTK-CRS  SECTION.                                          
049033     MOVE 'DB2-CLOSE-TP1ARTK   ' TO  WS-DB2-SEKTION                       
049034                                                                          
049035     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
049036     .                                                                    
049037     EJECT                                                                
049038                                                                          
049039 DB2-STATUS-CHECK  SECTION.                                               
049040                                                                          
049070     SET SQLCODE-IX TO 1                                                  
049080     SEARCH GOOD-SQLCODE                                                  
049091       AT END CALL FELLOG                                                 
049095       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
049096     END-SEARCH                                                           
049097     .                                                                    
049100                                                                          
