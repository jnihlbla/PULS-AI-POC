000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W9805200.                                                 
000400 AUTHOR.        KARIN OLSSON.                                             
000500     DATE-WRITTEN.  SEPTEMBER 1991.                                       
000600                                                                          
000700     REMARKS.                                                             
000800                                                                          
000900*    FUNKTION:                                                            
001000*        PROGRAMMET LÄGGER TILL EN RANDOMNYCKEL FÖRE                      
001100*        VARJE UNLOADPOST FRÅN SOP.                                       
001200*                                                                         
001300*    SUBPROGRAM:                                                          
001400*        W9805210 - RÄKNAR FRAM EN RANDOMNYCKEL                           
001500*                                                                         
001600     EJECT                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000 FILE-CONTROL.                                                            
002100                                                                          
002200* --- UNLOADFIL FRÅN SOP                                                  
002300     SELECT INFIL           ASSIGN TO W98052D1.                           
002400                                                                          
002500* --- UTFIL MED SORTERINGSNYCKEL                                          
002600     SELECT UTFIL           ASSIGN TO W98052D2.                           
002700                                                                          
002800*                                                                         
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 FILE SECTION.                                                            
003200                                                                          
003300 FD  INFIL                                                                
003400     RECORDING MODE F                                                     
003500     BLOCK CONTAINS 0.                                                    
003600 01  FILLER                      PIC X(83).                               
003700     SKIP3                                                                
003800 FD  UTFIL                                                                
003900     RECORDING MODE F                                                     
004000     BLOCK CONTAINS 0.                                                    
004100 01  UT-POST                     PIC X(87).                               
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400*                                                                         
004500 77  PROGRAM-NAMN                PIC X(8) VALUE 'W9805200'.               
004600                                                                          
004700 01  KONSTANTER.                                                          
004800     03  JA                      PIC X      VALUE 'J'.                    
004900     03  NEJ                     PIC X      VALUE 'N'.                    
005000     03  NYCKEL                  PIC X      VALUE 'K'.                    
005100     03  ATTR                    PIC X      VALUE 'P'.                    
005200     03  DATAT                   PIC X      VALUE 'D'.                    
005300*                                                                         
005400 01  DYNAMISKA-SUBPROGRAM.                                                
005500     03  ABEND                   PIC X(8)   VALUE 'ABEND'.                
005600     03  W9805210                PIC X(8)   VALUE 'W9805210'.             
005700                                                                          
005800*-----  SWITCHAR                                                          
005900 01  EOF-INFIL                   PIC X      VALUE 'N'.                    
006000                                                                          
006100 01  RKOD                        PIC S9(4) COMP  VALUE ZERO.              
006200 01  RKOD-ABEND-UTAN-DUMP        PIC S9(4) COMP  VALUE +16.               
006300                                                                          
006400 01  RANDOM-INDATA               PIC X(12).                               
006500 01  RANDOM-NYCKEL               PIC X(4).                                
006600 01  SOP-DATABAS                 PIC X(4)  VALUE 'SOP '.                  
006700     EJECT                                                                
006800 01  IN-RAD.                                                              
006900     03  IN-TYP                  PIC X.                                   
007000     03  IN-VAERDE               PIC X(80).                               
007100     SKIP3                                                                
007200 01  UT-RAD.                                                              
007300     03  UT-TYP                  PIC X.                                   
007400     03  UT-NYCKEL               PIC X(4).                                
007500     03  UT-VAERDE               PIC X(80).                               
007600     SKIP3                                                                
007700 01  ANTAL-NYCKLAR               PIC S9(7) COMP-3  VALUE 0.               
007800 01  ANTAL-DATAPOSTER            PIC S9(7) COMP-3  VALUE 0.               
007900     EJECT                                                                
008000 PROCEDURE DIVISION.                                                      
008100     SKIP2                                                                
008200     PERFORM A-INIT                                                       
008300     PERFORM S1-LAES-INFIL                                                
008400     IF EOF-INFIL = NEJ                                                   
008500*                                                                         
008600       PERFORM UNTIL EOF-INFIL = JA                                       
008700         IF IN-TYP = NYCKEL                                               
008800           ADD +1 TO ANTAL-NYCKLAR                                        
008900           MOVE SPACE TO RANDOM-NYCKEL                                    
009000           MOVE IN-VAERDE TO RANDOM-INDATA                                
009100                                                                          
009200           CALL W9805210 USING RANDOM-INDATA RANDOM-NYCKEL                
009300                            SOP-DATABAS                                   
009400                                                                          
009500           MOVE IN-TYP        TO UT-TYP                                   
009600           MOVE RANDOM-NYCKEL TO UT-NYCKEL                                
009700           MOVE IN-VAERDE     TO UT-VAERDE                                
009800                                                                          
009900           PERFORM S2-SKRIV-UTFIL                                         
010000*                                                                         
010100           PERFORM S1-LAES-INFIL                                          
010200*                                                                         
010300           IF IN-TYP = ATTR                                               
010500             MOVE IN-TYP        TO UT-TYP                                 
010600             MOVE RANDOM-NYCKEL TO UT-NYCKEL                              
010700             MOVE IN-VAERDE     TO UT-VAERDE                              
010800                                                                          
010900             PERFORM S2-SKRIV-UTFIL                                       
011000                                                                          
011100             PERFORM S1-LAES-INFIL                                        
011200             PERFORM UNTIL EOF-INFIL = JA                                 
011300                 OR IN-TYP = NYCKEL                                       
011400               IF IN-TYP = DATAT                                          
011500                 ADD +1 TO ANTAL-DATAPOSTER                               
011600                 MOVE IN-TYP        TO UT-TYP                             
011700                 MOVE RANDOM-NYCKEL TO UT-NYCKEL                          
011800                 MOVE IN-VAERDE     TO UT-VAERDE                          
011900                                                                          
012000                 PERFORM S2-SKRIV-UTFIL                                   
012100                                                                          
012200               ELSE                                                       
012300          DISPLAY 'OKÄND DATATYP ' IN-TYP ' EFTER ' ANTAL-NYCKLAR         
012400                   ' ANTAL NYCKLAR'                                       
012500                 PERFORM S99-ABEND                                        
012600               END-IF                                                     
012700               PERFORM S1-LAES-INFIL                                      
012800             END-PERFORM                                                  
012900           END-IF                                                         
013000*                                                                         
013100         ELSE                                                             
013200          DISPLAY 'OKÄND DATATYP ' IN-TYP ' EFTER ' ANTAL-NYCKLAR         
013300                   ' ANTAL NYCKLAR'                                       
013400           PERFORM S99-ABEND                                              
013500         END-IF                                                           
013600       END-PERFORM                                                        
013700*                                                                         
013800       DISPLAY ANTAL-NYCKLAR ' ANTAL NYCKLAR FUNNA PÅ INFILEN'            
013900     ELSE                                                                 
014000       DISPLAY 'INGA DATAPOSTER PÅ INFILEN'                               
014100     END-IF                                                               
014200                                                                          
014300     PERFORM Z-FINIT                                                      
014400     MOVE RKOD TO RETURN-CODE                                             
014500     GOBACK                                                               
014600     .                                                                    
014700     EJECT                                                                
014800 A-INIT  SECTION.                                                         
014900     SKIP2                                                                
015000     OPEN INPUT INFIL                                                     
015100     OPEN OUTPUT UTFIL                                                    
015200     .                                                                    
015300     EJECT                                                                
015400 Z-FINIT     SECTION.                                                     
015500     SKIP2                                                                
015600     CLOSE INFIL                                                          
015700           UTFIL                                                          
015800     .                                                                    
015900     EJECT                                                                
016000 S1-LAES-INFIL SECTION.                                                   
016100     SKIP2                                                                
016200     READ INFIL INTO IN-RAD                                               
016300       AT END MOVE JA TO EOF-INFIL                                        
016400     END-READ                                                             
016500     .                                                                    
016600 S2-SKRIV-UTFIL SECTION.                                                  
016700     SKIP2                                                                
016800     WRITE UT-POST FROM UT-RAD                                            
016900     .                                                                    
017000 S99-ABEND     SECTION.                                                   
017100     SKIP2                                                                
017200     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
017300     .                                                                    
017400     SKIP3                                                                
