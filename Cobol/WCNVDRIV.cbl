000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WCNVDRIV.                                                
000400 AUTHOR.         KJELL ANDRE.                                             
000500 DATE-WRITTEN.   97/04/21.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        DS                                                               
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
002402*          --- TEXT                                                       
002403     SELECT INFIL                      ASSIGN TO WCNVDRD1.                
002404     SKIP2                                                                
002405*          --- OCKSÅ TEXT                                                 
002410     SELECT UTFIL                      ASSIGN TO WCNVDRD2.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  INFIL                                                                
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003006 01  FILLER          PIC X(500).                                          
003007     SKIP3                                                                
003008 FD  UTFIL                                                                
003009     RECORDING       F                                                    
003010     BLOCK CONTAINS  0.                                                   
003011                                                                          
003020 01  O-POST          PIC X(500).                                          
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003301                                                                          
003310*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(8)    VALUE 'WCNVDRIV'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003801                                                                          
003802 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
003810     88  END-OF-INFIL                        VALUE 'J'.                   
003900     EJECT                                                                
004000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004100 01  FILLER REDEFINES DAGENS-DATUM.                                       
004200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004500     EJECT                                                                
004600 01  DYNAMISKA-SUBPROGRAM.                                                
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004920     03  WCNVUNJP                PIC X(8)    VALUE 'WCNVUNJP'.            
004930     03  WCNVJPUN                PIC X(8)    VALUE 'WCNVJPUN'.            
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
006020     EJECT                                                                
006030*    --- PARAMETRAR TILL WCNVUNJP                                         
006040*                                                                         
006050*01  -COPY WCNVAREA                                                       
006201     EJECT                                                                
006202 01  I-AREA-START                PIC X(24)   VALUE                        
006203                                 'I-AREA-START  '.                        
006204     SKIP2                                                                
006205                                                                          
006206 01  I-AREA                      PIC X(500).                              
006207     EJECT                                                                
006208 01  O-AREA-START                PIC X(24)   VALUE                        
006209                                 'O-AREA-START  '.                        
006210     SKIP2                                                                
006211                                                                          
006220 01  O-AREA                      PIC X(500).                              
006300     EJECT                                                                
006400 PROCEDURE DIVISION.                                                      
006500 MAIN SECTION.                                                            
006700     SKIP2                                                                
006800                                                                          
006900     PERFORM A-INIT                                                       
007010     PERFORM S01-LAES-INFIL                                               
007100     PERFORM UNTIL END-OF-INFIL                                           
007200                                                                          
007300       MOVE I-AREA TO TECONV-FROM                                         
007400       MOVE NEJ    TO FLTXTENT                                            
007401       MOVE 400    TO KVMAXTL                                             
007410       CALL WCNVJPUN  USING WCNVAREA                                      
007600       MOVE TECONV-TO TO O-AREA                                           
007601       DISPLAY KDSVAR                                                     
007610       PERFORM S11-SKRIV-UTFIL                                            
007810       PERFORM S01-LAES-INFIL                                             
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
008910     OPEN INPUT  INFIL                                                    
009001                                                                          
009010     OPEN OUTPUT UTFIL                                                    
009100     SKIP2                                                                
009200     ACCEPT DAGENS-DATUM  FROM DATE                                       
009310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009400     .                                                                    
009500     EJECT                                                                
009600 Z-FINIT SECTION.                                                         
009701     CLOSE INFIL                                                          
009710           UTFIL                                                          
009801     SKIP2                                                                
009802     MOVE 'S' TO POSTSUM-OPKOD                                            
009810     CALL POSTSUM USING POSTSUM-PARM                                      
009900     .                                                                    
010001     EJECT                                                                
010002 S01-LAES-INFIL  SECTION.                                                 
010003     READ INFIL INTO I-AREA                                               
010004     AT END                                                               
010005        MOVE HIGH-VALUE TO I-AREA                                         
010006        SET END-OF-INFIL TO TRUE                                          
010007                                                                          
010008     NOT AT END                                                           
010009        MOVE 'INFIL' TO POSTSUM-FDNAMN                                    
010010        MOVE 'WCNVDRD1' TO POSTSUM-DDNAMN2                                
010013        MOVE SPACE    TO POSTSUM-TRANSTYP                                 
010014        CALL POSTSUM USING POSTSUM-PARM                                   
010015     END-READ                                                             
010020     .                                                                    
010101     EJECT                                                                
010102 S11-SKRIV-UTFIL SECTION.                                                 
010103                                                                          
010104     WRITE O-POST FROM O-AREA                                             
010105                                                                          
010106     MOVE SPACE    TO POSTSUM-TRANSTYP                                    
010107     MOVE 'UTFIL' TO POSTSUM-FDNAMN                                       
010108     MOVE 'WCNVDRD2' TO POSTSUM-DDNAMN2                                   
010109     CALL POSTSUM USING POSTSUM-PARM                                      
010110     .                                                                    
