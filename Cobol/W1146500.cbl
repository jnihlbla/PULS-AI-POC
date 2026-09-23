000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W1146500.                                                
000400 AUTHOR.         KIHLBERG STEFAN.                                         
000500 DATE-WRITTEN.   12/09/25.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        SPLITS RECIEPTS FROM S1+ BETWEEN SE AND CN/USA                   
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
002402*          --- RECIEPTS FROM SI+                                          
002403     SELECT T335R309                   ASSIGN TO W11465D1.                
002404     SKIP2                                                                
002405*          --- RECIEPTS FROM SI+ SE                                       
002406     SELECT W11460                     ASSIGN TO W11465D2.                
002407     SKIP2                                                                
002408*          --- RECIEPTS FROM SI+ CN USA                                   
002410     SELECT W11465                     ASSIGN TO W11465D3.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  T335R309                                                             
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003006*01  -COPY T335R309      -L.                                              
003007     SKIP3                                                                
003008 FD  W11460                                                               
003009     RECORDING       F                                                    
003010     BLOCK CONTAINS  0.                                                   
003011                                                                          
003012*01  RECORD -COPY T335R309 -PRE  SE-  -L.                                 
003013     SKIP3                                                                
003014 FD  W11465                                                               
003015     RECORDING       F                                                    
003016     BLOCK CONTAINS  0.                                                   
003017                                                                          
003020*01  RECORD -COPY T335R309 -PRE  CN-  -L.                                 
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(8)    VALUE 'W1146500'.            
003500 77  YES                         PIC X       VALUE 'J'.                   
003600 77  NOO                         PIC X       VALUE 'N'.                   
003801                                                                          
003802 77  T335R309-EOF-SW             PIC X       VALUE 'N'.                   
003810     88  END-OF-T335R309                     VALUE 'J'.                   
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
006205                                                                          
006206*01  AREA -COPY T335R309     -PRE IN-                                     
006207     EJECT                                                                
006208 01  SE-AREA-START               PIC X(24)   VALUE                        
006209                                 'SE-AREA-START  '.                       
006210     SKIP2                                                                
006211                                                                          
006212*01  AREA -COPY T335R309     -PRE SE-                                     
006213     EJECT                                                                
006214 01  CN-AREA-START               PIC X(24)   VALUE                        
006215                                 'CN-AREA-START  '.                       
006216     SKIP2                                                                
006217                                                                          
006220*01  AREA -COPY T335R309     -PRE CN-                                     
006300     EJECT                                                                
006400 PROCEDURE DIVISION.                                                      
006500 MAIN SECTION.                                                            
006700     SKIP2                                                                
006800                                                                          
006900     PERFORM A-INIT                                                       
007010     PERFORM S01-READ-T335R309                                            
007100     PERFORM UNTIL END-OF-T335R309                                        
007110       IF IN-CDTYPE-REQ ='NP'                                             
007121         IF IN-NP-IDUSER = 'BP2TW' OR '1441 '                             
007122           MOVE IN-AREA TO SE-AREA                                        
007123           PERFORM S11-WRITE-W11460                                       
007140         ELSE                                                             
007143           MOVE IN-AREA TO CN-AREA                                        
007144           PERFORM S12-WRITE-W11465                                       
007150         END-IF                                                           
007160                                                                          
007170       ELSE                                                               
007171         IF IN-CDTYPE-REQ ='CC'                                           
007173           IF IN-CC-IDUSER = 'BP2TW' OR '1441 '                           
007174             MOVE IN-AREA TO SE-AREA                                      
007175             PERFORM S11-WRITE-W11460                                     
007176           ELSE                                                           
007179             MOVE IN-AREA TO CN-AREA                                      
007180             PERFORM S12-WRITE-W11465                                     
007181           END-IF                                                         
007182         END-IF                                                           
007183       END-IF                                                             
007192                                                                          
007810       PERFORM S01-READ-T335R309                                          
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
008910     OPEN INPUT  T335R309                                                 
009001                                                                          
009002     OPEN OUTPUT W11460                                                   
009010                 W11465                                                   
009100     SKIP2                                                                
009200     ACCEPT TODAYS-DATE  FROM DATE                                        
009310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009400     .                                                                    
009500     EJECT                                                                
009600 Z-FINIT SECTION.                                                         
009701     CLOSE T335R309                                                       
009702           W11460                                                         
009710           W11465                                                         
009801     SKIP2                                                                
009802     MOVE 'S' TO POSTSUM-OPKOD                                            
009810     CALL POSTSUM USING POSTSUM-PARM                                      
009900     .                                                                    
010001     EJECT                                                                
010002 S01-READ-T335R309  SECTION.                                              
010003     READ T335R309 INTO IN-AREA                                           
010004     AT END                                                               
010005        MOVE HIGH-VALUE TO IN-AREA                                        
010006        SET END-OF-T335R309 TO TRUE                                       
010007                                                                          
010008     NOT AT END                                                           
010009        MOVE 'T335R309' TO POSTSUM-FDNAMN                                 
010010        MOVE 'W11465D1' TO POSTSUM-DDNAMN2                                
010013        CALL POSTSUM USING POSTSUM-PARM                                   
010014     END-READ                                                             
010020     .                                                                    
010101     EJECT                                                                
010102 S11-WRITE-W11460 SECTION.                                                
010103                                                                          
010104     WRITE SE-RECORD FROM SE-AREA                                         
010105                                                                          
010106     MOVE SE-IDRT TO POSTSUM-TRANSTYP                                     
010107     MOVE 'W11460' TO POSTSUM-FDNAMN                                      
010108     MOVE 'W11465D2' TO POSTSUM-DDNAMN2                                   
010109     CALL POSTSUM USING POSTSUM-PARM                                      
010110     .                                                                    
010111     EJECT                                                                
010112 S12-WRITE-W11465 SECTION.                                                
010113                                                                          
010114     WRITE CN-RECORD FROM CN-AREA                                         
010115                                                                          
010116     MOVE CN-IDRT   TO POSTSUM-TRANSTYP                                   
010117     MOVE 'W11465' TO POSTSUM-FDNAMN                                      
010118     MOVE 'W11465D3' TO POSTSUM-DDNAMN2                                   
010119     CALL POSTSUM USING POSTSUM-PARM                                      
010120     .                                                                    
010300     EJECT                                                                
010400 S99-ABEND SECTION.                                                       
010500                                                                          
010601     SKIP2                                                                
010602     MOVE 'S' TO POSTSUM-OPKOD                                            
010610     CALL POSTSUM USING POSTSUM-PARM                                      
010700     CALL ABEND USING RKOD-ABEND                                          
010800     .                                                                    
