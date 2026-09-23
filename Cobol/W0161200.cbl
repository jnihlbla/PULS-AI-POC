000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0161200.                                                
000300 AUTHOR.         RICHARD THÖRNGREN.                                       
000400 DATE-WRITTEN.   92/02/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAM SOM TAR EMOT DATA                                        
000900*        MED HJÄLP AV VCOM                                                
001000*        OCH SKAPAR EN FILE.                                              
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
002100     SELECT W01612VC                   ASSIGN TO W01612D1.                
002200     SELECT W01612ST                   ASSIGN TO W01612D2.                
002300     SELECT W01612UT                   ASSIGN TO W01612D3.                
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600                                                                          
002700                                                                          
002800 FILE SECTION.                                                            
002900                                                                          
003000                                                                          
003100                                                                          
003200 FD  W01612VC                                                             
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600                                                                          
003700 01  IN-VCOM.                                                             
003800   03  IN-EXPEDITER              PIC X(8).                                
003900   03  FILLER                    PIC X(72).                               
004000                                                                          
004100                                                                          
004200                                                                          
004300 FD  W01612ST                                                             
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700                                                                          
004800 01  IN-STYR                     PIC X(80).                               
004900                                                                          
005000                                                                          
005100                                                                          
005200 FD  W01612UT                                                             
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS  0.                                                   
005500                                                                          
005600                                                                          
005700 01  UT-INFO                     PIC X(80).                               
005800                                                                          
005900     EJECT                                                                
006000 WORKING-STORAGE SECTION.                                                 
006100                                                                          
006200*    -- CHECKED BY WY2000                                                 
006300                                                                          
006400 77  IDPGM                       PIC X(8)    VALUE 'W0161200'.            
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  NEJ                         PIC X       VALUE 'N'.                   
006700 77  INDX                        PIC S9(9)   VALUE ZERO COMP SYNC.        
006800 77  W-POSTANT-IN                PIC S9(7)   VALUE ZERO COMP-3.           
006900 77  W-EXPEDITER                 PIC X(8)    VALUE SPACE.                 
007000                                                                          
007100                                                                          
007200 01  W-SENDERTAG.                                                         
007300     03  W-IDCPYTXT              PIC X(8)    VALUE SPACE.                 
007400     03  W-EOF                   PIC X(3)    VALUE SPACE.                 
007500     03  FILLER                  PIC X(9)    VALUE SPACE.                 
007600                                                                          
007700                                                                          
007800 01  FELTEXT.                                                             
007900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008100                                                                          
008200                                                                          
008300                                                                          
008400 01  DYNAMISKA-SUBPROGRAM.                                                
008500                                                                          
008600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008700     03  DSCONR                  PIC X(8)    VALUE 'DSCONR  '.            
008800     03  DSRECV                  PIC X(8)    VALUE 'DSRECV  '.            
008900     03  DSRLSE                  PIC X(8)    VALUE 'DSRLSE  '.            
009000     03  W016WRT                 PIC X(8)    VALUE 'W016WRT '.            
009100                                                                          
009200                                                                          
009300*    --- PARAMETRAR TILL ABEND                                            
009400                                                                          
009500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009600     EJECT                                                                
009700 01  VCOM-AREA-START             PIC X(16)   VALUE                        
009800                                 'VCOM-AREA-START '.                      
009900                                                                          
010000*01  -COPY W0028  -PRE VCOM-                                              
010100     EJECT                                                                
010200 01  W-IN-STYR-START             PIC X(16)   VALUE                        
010300                                 'W-IN-STYR-START '.                      
010400                                                                          
010500*01  -COPY W016121A                                                       
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE                        
010800                                 'W016WRT-DATA   '.                       
010900 01  WRT-W016WRT.                                                         
011000   03  WRT-KVLL                  PIC S9(4)   VALUE +1    COMP.            
011100   03  WRT-KVTRANS               PIC S9(4)   VALUE +1    COMP.            
011200   03  WRT-KDRECFM               PIC X(3)    VALUE SPACE.                 
011300   03  WRT-KVPOST                PIC S9(7)   VALUE ZERO  COMP-3.          
011400   03  WRT-IDDSN                 PIC X(44)   VALUE SPACE.                 
011500   03  WRT-KDSVAR                PIC X(1)    VALUE SPACE.                 
011600   03  WRT-STYRPOST.                                                      
011700     05  WRT-STYRLL              PIC S9(4)   VALUE +80   COMP.            
011800     05  WRT-STYRZZ              PIC X(2)    VALUE LOW-VALUE.             
011900     05  WRT-STYRDATA            PIC X(80)   VALUE SPACE.                 
012000     EJECT                                                                
012100 01  FILLER                      PIC X(16)   VALUE                        
012200                                 'UT-INFO        '.                       
012300*01  -COPY W016001A                                                       
012400                                                                          
012500     EJECT                                                                
012600*01  -COPY W016011A                                                       
012700                                                                          
012800     EJECT                                                                
012900 PROCEDURE DIVISION.                                                      
013000 MAIN SECTION.                                                            
013100                                                                          
013200     PERFORM A-INIT                                                       
013300     PERFORM B-VCOM-START                                                 
013400     PERFORM C-LAES-STYRINFO                                              
013500     PERFORM D-HDR-RECORD                                                 
013600     PERFORM UNTIL VCOM-RC > ZERO                                         
013700                                                                          
013800       CALL DSRECV USING VCOM-RC                                          
013900                         VCOM-DISTID                                      
014000                         VCOM-MAXLENGTH                                   
014100                         VCOM-ACTLENGTH                                   
014200                         VCOM-DATA                                        
014300                                                                          
014400       IF VCOM-RC = ZERO OR 45                                            
014500         PERFORM E-WRITE                                                  
014600       ELSE                                                               
014700         DISPLAY 'DSRECV RC        = ' VCOM-RC                            
014800         DISPLAY 'DSRECV DISTID    = ' VCOM-DISTID                        
014900         DISPLAY 'DSRECV MAXLENGTH = ' VCOM-MAXLENGTH                     
015000         DISPLAY 'DSRECV ACTLENGTH = ' VCOM-ACTLENGTH                     
015100         DISPLAY 'DSRECV DATA      = ' VCOM-DATA                          
015200         DISPLAY 'FEL RETURKOD FRÅN DSRECV '                              
015300         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
015400       END-IF                                                             
015500                                                                          
015600       MOVE SPACE TO VCOM-DATA                                            
015700     END-PERFORM                                                          
015800                                                                          
015900     PERFORM Z-FINIT                                                      
016000                                                                          
016100     PERFORM ZZ-VCOM-SLUT                                                 
016200                                                                          
016300     MOVE ZERO TO RETURN-CODE                                             
016400     GOBACK                                                               
016500     .                                                                    
016600                                                                          
016700     EJECT                                                                
016800 A-INIT SECTION.                                                          
016900                                                                          
017000     OPEN INPUT  W01612VC                                                 
017100                 W01612ST                                                 
017200                                                                          
017300     MOVE +0 TO W-POSTANT-IN                                              
017400     MOVE ALL '+' TO 121A-W016121A                                        
017500                                                                          
017600     READ W01612VC                                                        
017700       AT END                                                             
017800         DISPLAY 'EXPEDITER SAKNAS '                                      
017900         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
018000       NOT AT END                                                         
018100         MOVE IN-EXPEDITER TO W-EXPEDITER                                 
018200         DISPLAY 'START EXPEDITER = ' W-EXPEDITER                         
018300     END-READ                                                             
018400     .                                                                    
018500                                                                          
018600     EJECT                                                                
018700 B-VCOM-START SECTION.                                                    
018800                                                                          
018900     MOVE +9999 TO VCOM-MAXLENGTH                                         
019000     MOVE W-EXPEDITER TO VCOM-EXPEDITER                                   
019100     CALL DSCONR USING VCOM-RC                                            
019200                       VCOM-DISTID                                        
019300                       VCOM-SECUR                                         
019400                       VCOM-TIMEOUT                                       
019500                       VCOM-SENDERTAG                                     
019600                       VCOM-EXPEDITER                                     
019700                       VCOM-RECTYPE                                       
019800     MOVE VCOM-SENDERTAG TO W-SENDERTAG                                   
019900     DISPLAY 'DSCONR RC        = ' VCOM-RC                                
020000     DISPLAY 'DSCONR DISTID    = ' VCOM-DISTID                            
020100     DISPLAY 'DSCONR SECUR     = ' VCOM-SECUR                             
020200     DISPLAY 'DSCONR TIMEOUT   = ' VCOM-TIMEOUT                           
020300     DISPLAY 'DSCONR SENDERTAG = ' VCOM-SENDERTAG                         
020400     DISPLAY 'DSCONR EXPEDITER = ' VCOM-EXPEDITER                         
020500     DISPLAY 'DSCONR RECTYPE   = ' VCOM-RECTYPE                           
020600     IF VCOM-RC NOT = ZERO                                                
020700       DISPLAY 'FEL RETURKOD FRÅN DSCONR '                                
020800       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
020900     END-IF                                                               
021000     .                                                                    
021100                                                                          
021200     EJECT                                                                
021300 C-LAES-STYRINFO SECTION.                                                 
021400                                                                          
021500     PERFORM UNTIL 121A-IDCPYTXT = W-IDCPYTXT                             
021600       READ W01612ST INTO 121A-W016121A                                   
021700         AT END                                                           
021800           DISPLAY 'STYR-COPYTEXT SAKNAS PÅ VCOMPARM '                    
021900           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
022000       END-READ                                                           
022100     END-PERFORM                                                          
022200     DISPLAY 121A-W016121A                                                
022300     .                                                                    
022400                                                                          
022500     EJECT                                                                
022600 D-HDR-RECORD SECTION.                                                    
022700                                                                          
022800     MOVE SPACE TO 001A-W016001A                                          
022900                   011A-W016011A                                          
023000     MOVE W-EXPEDITER TO 001A-IDVCOM                                      
023100                         011A-IDVCOM                                      
023200     MOVE '001' TO 001A-IDPTYP                                            
023300     MOVE '011' TO 011A-IDPTYP                                            
023400     MOVE 'A' TO 001A-IDVTYP                                              
023500                 011A-IDVTYP                                              
023600     MOVE '57' TO 011A-IDFTG                                              
023700     MOVE 001A-VC-IDLANDX2 TO 011A-IDLANDX2                               
023800     MOVE W-IDCPYTXT TO 001A-IDCPYTXT                                     
023900     ACCEPT 001A-TIREGDAT FROM DATE                                       
024000     MOVE ZERO TO 001A-KVPOST                                             
024100     MOVE VCOM-DISTID TO 001A-IDVCOMEX                                    
024200                                                                          
024300     IF 121A-KDCALL = '002'                                               
024400       MOVE 001A-W016001A TO WRT-STYRDATA                                 
024500     END-IF                                                               
024600     MOVE 121A-KVLRECL TO WRT-KVLL                                        
024700     MOVE 121A-KVTRANS TO WRT-KVTRANS                                     
024800                                                                          
024900*    IF 121A-KVLRECL > 9999                                               
025000*      DISPLAY ' FÖR STORT RECORD ' 121A-KVLRECL                          
025100*      CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
025200*    ELSE                                                                 
025300       IF 121A-KVLRECL > 200                                              
025400          AND 121A-KVTRANS > 1                                            
025500         DISPLAY ' KVLRECL OCH KVTRANS EJ OK '                            
025600                 121A-KVLRECL '  ' 121A-KVTRANS                           
025700         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
025800       END-IF                                                             
025900*    END-IF                                                               
026000     .                                                                    
026100     EJECT                                                                
026200 E-WRITE SECTION.                                                         
026300                                                                          
026400     IF VCOM-RC = 45                                                      
026500       MOVE SPACE TO VCOM-DATA                                            
026600     ELSE                                                                 
026700       IF VCOM-DATA = SPACE                                               
026800         MOVE '<THIS IS AN EMPTY LINE FROM VCOM>' TO VCOM-DATA            
026900       END-IF                                                             
027000       ADD +1 TO W-POSTANT-IN                                             
027100       IF W-EOF = 'EOF'                                                   
027200         MOVE SPACE TO VCOM-DATA                                          
027300       END-IF                                                             
027400     END-IF                                                               
027500     IF 121A-KVLRECL = ZERO                                               
027600*      -- DYNAMISK POSTLÄNGD GÄLLER - ANVÄND VERKLIGT VÄRDE               
027700       MOVE '0'            TO WRT-KDSVAR                                  
027800       MOVE VCOM-ACTLENGTH TO WRT-KVLL                                    
027900       DISPLAY VCOM-ACTLENGTH                                             
028000     END-IF                                                               
028100*    DISPLAY WRT-W016WRT ' ' VCOM-DATA                                    
028200     CALL W016WRT USING WRT-W016WRT VCOM-DATA                             
028300     .                                                                    
028400                                                                          
028500     EJECT                                                                
028600 Z-FINIT SECTION.                                                         
028700     CLOSE W01612VC                                                       
028800           W01612ST                                                       
028900                                                                          
029000     DISPLAY 'ANTAL POSTER IN = ' W-POSTANT-IN                            
029100     DISPLAY 'ANTAL POSTER UT = ' WRT-KVPOST                              
029200                                                                          
029300     MOVE WRT-IDDSN TO 011A-IDDSN                                         
029400     MOVE WRT-KVPOST TO 001A-KVPOST                                       
029500                                                                          
029600     IF 121A-KDCALL = '003'                                               
029700       OPEN OUTPUT W01612UT                                               
029800       WRITE UT-INFO FROM 001A-W016001A                                   
029900       WRITE UT-INFO FROM 011A-W016011A                                   
030000       CLOSE W01612UT                                                     
030100       DISPLAY 001A-W016001A                                              
030200       DISPLAY 011A-W016011A                                              
030300     END-IF                                                               
030400     .                                                                    
030500                                                                          
030600                                                                          
030700                                                                          
030800 ZZ-VCOM-SLUT SECTION.                                                    
030900                                                                          
031000     MOVE ZERO TO VCOM-RC                                                 
031100     CALL DSRLSE USING VCOM-RC                                            
031200                       VCOM-DISTID                                        
031300                       VCOM-RVALUE                                        
031400     DISPLAY 'DSRLSE RC     = ' VCOM-RC                                   
031500     DISPLAY 'DSRLSE DISTID = ' VCOM-DISTID                               
031600     DISPLAY 'DSRLSE RVALUE = ' VCOM-RVALUE                               
031700     .                                                                    
