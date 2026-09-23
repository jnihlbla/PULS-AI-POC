000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3350600.                                                
000300 AUTHOR.         INGVAR SKJELBRED.                                        
000400 DATE-WRITTEN.   99/09/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄGGER TILL TIDEN FÖR ALLA PRISFILER FRÅN ALLA                   
001000*        MARKNADSBOLAG                                                    
001100*                                                                         
001200*                                                                         
001300*    ABENDKODER:                                                          
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
002402*          --- FIL MED NYA PRISER FRÅN MARKNADSBOLAG                      
002403     SELECT W33520                     ASSIGN TO W33506D1.                
002404     SKIP2                                                                
002405*          --- FIL MED NYA PRISER SAMT TID NÄR MAN SKICKAT PRISET         
002410     SELECT W33506                     ASSIGN TO W33506D2.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003007 FD  W33520                                                               
003008     RECORDING       V                                                    
003009     BLOCK CONTAINS  0.                                                   
003010*01  -COPY W335201A      -L.                                              
003012     SKIP3                                                                
003013 FD  W33506                                                               
003016     RECORDING       V                                                    
003017     BLOCK CONTAINS  0.                                                   
003018                                                                          
003020*01  POST -COPY W335201B -PRE  UT-  -L.                                   
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003301                                                                          
003310*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(8)    VALUE 'W3350600'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003801                                                                          
003802 77  W33520-EOF-SW               PIC X       VALUE 'N'.                   
003810     88  END-OF-W33520                       VALUE 'J'.                   
003900     EJECT                                                                
003901                                                                          
003910 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
003920                                                                          
004000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004100 01  FILLER REDEFINES DAGENS-DATUM.                                       
004200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004500     EJECT                                                                
004600 01  DYNAMISKA-SUBPROGRAM.                                                
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004901     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004910     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
005000     SKIP2                                                                
005100*    --- PARAMETRAR TILL ABEND                                            
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005600     SKIP2                                                                
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006001     EJECT                                                                
006002*    --- PARAMETRAR TILL POSTSUM                                          
006003*                                                                         
006010*01  -COPY W0005   -PRE  POSTSUM-                                         
006101     EJECT                                                                
006110*01  -COPY WDATAREA                                                       
006201     EJECT                                                                
006202 01  IN-AREA-START               PIC X(24)   VALUE                        
006203                                 'IN-AREA-START  '.                       
006204     SKIP2                                                                
006205                                                                          
006206*01  AREA -COPY W335201A     -PRE IN-                                     
006207     EJECT                                                                
006208 01  UT-AREA-START               PIC X(24)   VALUE                        
006209                                 'UT-AREA-START  '.                       
006210     SKIP2                                                                
006211                                                                          
006220*01  AREA -COPY W335201B     -PRE UT-                                     
006300     EJECT                                                                
006400 PROCEDURE DIVISION.                                                      
006500 MAIN SECTION.                                                            
006620*------------------------                                                 
006700     SKIP2                                                                
006800                                                                          
006900     PERFORM A-INIT                                                       
007010     PERFORM S01-LAES-W33520                                              
007100     PERFORM UNTIL END-OF-W33520                                          
007210       PERFORM C-BEARBETA                                                 
007810       PERFORM S01-LAES-W33520                                            
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
008910     OPEN INPUT  W33520                                                   
009001                                                                          
009010     OPEN OUTPUT W33506                                                   
009100     SKIP2                                                                
009200     ACCEPT DAGENS-DATUM  FROM DATE                                       
009300     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
009310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009400     .                                                                    
009500     EJECT                                                                
009510 C-BEARBETA SECTION.                                                      
009512                                                                          
009539     MOVE IN-IDVTYP                   TO UT-IDVTYP                        
009540     MOVE IN-IDPROMR                  TO UT-IDPROMR                       
009541     MOVE IN-TISTADAT                 TO UT-TISTADAT                      
009542     MOVE IN-TISTODAT                 TO UT-TISTODAT                      
009543     MOVE IN-KDARTKAM                 TO UT-KDARTKAM                      
009545     MOVE IN-REARTRAB-DO              TO UT-REARTRAB-DO                   
009546     MOVE IN-REARTRAB-BULK            TO UT-REARTRAB-BULK                 
009548     MOVE WS-TTMMSSTH                 TO UT-TIKLOCK                       
009549     PERFORM S11-SKRIV-W33506                                             
009550     .                                                                    
009560     EJECT                                                                
009600 Z-FINIT SECTION.                                                         
009701     CLOSE W33520                                                         
009710           W33506                                                         
009801     SKIP2                                                                
009802     MOVE 'S' TO POSTSUM-OPKOD                                            
009810     CALL POSTSUM USING POSTSUM-PARM                                      
009900     .                                                                    
010001     EJECT                                                                
010002 S01-LAES-W33520  SECTION.                                                
010003     READ W33520 INTO IN-AREA                                             
010004     AT END                                                               
010005        MOVE HIGH-VALUE TO IN-AREA                                        
010006        SET END-OF-W33520 TO TRUE                                         
010007                                                                          
010008     NOT AT END                                                           
010009        MOVE 'W33520' TO POSTSUM-FDNAMN                                   
010010        MOVE 'W33506D1' TO POSTSUM-DDNAMN2                                
010013        MOVE IN-IDVTYP TO POSTSUM-TRANSTYP                                
010014        CALL POSTSUM USING POSTSUM-PARM                                   
010015     END-READ                                                             
010020     .                                                                    
010101     EJECT                                                                
010102 S11-SKRIV-W33506 SECTION.                                                
010103                                                                          
010104     WRITE UT-POST FROM UT-AREA                                           
010105                                                                          
010106     MOVE UT-IDVTYP TO POSTSUM-TRANSTYP                                   
010107     MOVE 'W33506' TO POSTSUM-FDNAMN                                      
010108     MOVE 'W33506D2' TO POSTSUM-DDNAMN2                                   
010109     CALL POSTSUM USING POSTSUM-PARM                                      
010110     .                                                                    
010300     EJECT                                                                
010400 S99-ABEND SECTION.                                                       
010500                                                                          
010601     SKIP2                                                                
010602     MOVE 'S' TO POSTSUM-OPKOD                                            
010610     CALL POSTSUM USING POSTSUM-PARM                                      
010700     CALL ABEND USING RKOD-ABEND                                          
010800     .                                                                    
