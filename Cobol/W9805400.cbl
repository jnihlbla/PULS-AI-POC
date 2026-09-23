000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W9805400.                                                 
000400 AUTHOR.        KARIN OLSSON.                                             
000500     DATE-WRITTEN.  AUGUSTI 1991.                                         
000600                                                                          
000700     REMARKS.                                                             
000800                                                                          
000900*    FUNKTION:                                                            
001000*        PROGRAMMET LADDAR EN DATABAS MED DATA. DATAT LÄSES               
001100*        IN FRÅN EN SEKVENSIELL FIL DÄR NYCKLAR FÖLJS AV                  
001200*        DATARADER. LADDAR ENDAST NYCKLAR MED DATA. TOMMA                 
001300*        NYCKLAR IGNORERAS.                                               
001310*                                                                         
001400*    SUBPROGRAM:                                                          
001500*        W9805410 -  ACCESSER MOT DATABASEN (VARIANT AV WSPACE)           
001600*                                                                         
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200                                                                          
002300     SELECT INFIL           ASSIGN TO W98054D1.                           
002400                                                                          
002500*    DDNAMN  SOPDD1  ANVÄNDS AV SUBPROGRAM W9805410                       
002700*                                                                         
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 FILE SECTION.                                                            
003100                                                                          
003200 FD  INFIL                                                                
003300     RECORDING MODE F                                                     
003400     BLOCK CONTAINS 0.                                                    
003500 01  FILLER                      PIC X(87).                               
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003701                                                                          
003710*    -- CHECKED BY WY2000                                                 
003800*                                                                         
003900 77  PROGRAM-NAMN                PIC X(8) VALUE 'W9805400'.               
004000                                                                          
004100 01  KONSTANTER.                                                          
004200     03  JA                      PIC X      VALUE 'J'.                    
004300     03  NEJ                     PIC X      VALUE 'N'.                    
004400     03  NYCKEL                  PIC X      VALUE 'K'.                    
004410     03  ATTR                    PIC X      VALUE 'P'.                    
004500     03  DATAT                   PIC X      VALUE 'D'.                    
004600*                                                                         
004700 01  DYNAMISKA-SUBPROGRAM.                                                
004800     03  ABEND                   PIC X(8)   VALUE 'ABEND'.                
004900     03  W9805410                PIC X(8)   VALUE 'W9805410'.             
005000                                                                          
005100*-----  SWITCHAR                                                          
005200 01  EOF-INFIL                   PIC X      VALUE 'N'.                    
005400                                                                          
005500 01  RKOD                        PIC S9(4) COMP  VALUE ZERO.              
005600 01  RKOD-ABEND-UTAN-DUMP        PIC S9(4) COMP  VALUE +16.               
005700     EJECT                                                                
005800*-----  PARAMETRAR TILL W9805410 (HANTERING AV SOP-REGISTRET)             
005900 01  FUNKTIONSKODER.                                                      
006100     03  FOPEN                   PIC X(4)   VALUE 'OPEN'.                 
006200     03  FCLSE                   PIC X(4)   VALUE 'CLSE'.                 
006300     03  FQUIT                   PIC X(4)   VALUE 'QUIT'.                 
006400     03  FSAVE                   PIC X(4)   VALUE 'SAVE'.                 
006510     03  FADD                    PIC X(4)   VALUE 'ADD'.                  
007100                                                                          
007200 01  WSRKOD                      PIC X.                                   
007300                                                                          
007400*--------  SPECIELLA ARBETS-PARAMETRAR                                    
007500 01  W9805410-DD-PARM.                                                    
007600     03  W9805410-DD-LENGD       PIC S9(4)  COMP   VALUE +10.             
007700     03  W9805410-DD-NAMN        PIC X(8)   VALUE 'SOPDD1  '.             
007800                                                                          
007900 01  W9805410-DD-PARM2.                                                   
008000     03  W9805410-DD-LENGD2      PIC S9(4)  COMP   VALUE +10.             
008100     03  W9805410-DD-NAMN2       PIC X(8)   VALUE 'SOPDD2  '.             
008200                                                                          
008300 01  NYCKEL1-PARM.                                                        
008400     03  FILLER                  PIC S9(4)  COMP.                         
008500     03  FILLER                  PIC X(20).                               
008600                                                                          
008700 01  NYCKEL2-PARM.                                                        
008800     03  FILLER                  PIC S9(4)  COMP.                         
008900     03  FILLER                  PIC X(20).                               
009000                                                                          
009100 01  DATA-PARM.                                                           
009200     03  FILLER                  PIC S9(4)  COMP.                         
009300     03  FILLER                  PIC X(80).                               
009400     EJECT                                                                
009500                                                                          
009600 01  DATA-RAD.                                                            
009700     03  DATA-TYP                PIC X.                                   
009800     03  FILLER                  PIC X(4).                                
009801     03  DATA-VAERDE             PIC X(80).                               
009810     SKIP3                                                                
009820 01  ANTAL-NYCKLAR               PIC S9(7) COMP-3  VALUE 0.               
009830 01  ANTAL-DATAPOSTER            PIC S9(7) COMP-3  VALUE 0.               
009900     EJECT                                                                
010000 PROCEDURE DIVISION.                                                      
010100     SKIP2                                                                
010200     PERFORM A-INIT                                                       
010300     PERFORM S1-LAES-INFIL                                                
010400     IF EOF-INFIL = NEJ                                                   
010500       PERFORM X-OPEN-SOP                                                 
010510*                                                                         
010600       PERFORM UNTIL EOF-INFIL = JA                                       
010700         IF DATA-TYP = NYCKEL                                             
010800           MOVE DATA-VAERDE TO NYCKEL1-PARM                               
011100           PERFORM S1-LAES-INFIL                                          
011110*                                                                         
011111           IF DATA-TYP = ATTR AND EOF-INFIL = NEJ                         
011112             MOVE DATA-VAERDE TO NYCKEL2-PARM                             
011120             ADD +1 TO ANTAL-NYCKLAR                                      
011130             PERFORM S1-LAES-INFIL                                        
011200             PERFORM UNTIL EOF-INFIL = JA                                 
011300                 OR DATA-TYP = NYCKEL                                     
011400               IF DATA-TYP = DATAT                                        
011410                 ADD +1 TO ANTAL-DATAPOSTER                               
011500                 MOVE DATA-VAERDE TO DATA-PARM                            
011510                 PERFORM B-LADDA-DATABAS                                  
011600               ELSE                                                       
011700                 DISPLAY 'OKÄND DATATYP ' DATA-TYP                        
011800               END-IF                                                     
011900               PERFORM S1-LAES-INFIL                                      
012000             END-PERFORM                                                  
012001           END-IF                                                         
012010*                                                                         
012100         ELSE                                                             
012200           DISPLAY 'OKÄND DATATYP ' DATA-TYP                              
012300         END-IF                                                           
012500       END-PERFORM                                                        
012510*                                                                         
012520       CALL W9805410 USING FSAVE WSRKOD                                   
012530*                                                                         
012600       PERFORM Y-CLOSE-SOP                                                
012610       DISPLAY ANTAL-NYCKLAR ' ANTAL NYCKLAR FUNNA PÅ INFILEN'            
012620       DISPLAY ANTAL-DATAPOSTER ' ANTAL POSTER LADDADE I BASEN'           
012700     ELSE                                                                 
012800       DISPLAY 'INGA DATAPOSTER PÅ INFILEN'                               
012900     END-IF                                                               
013000                                                                          
013100     PERFORM Z-FINIT                                                      
013200     MOVE RKOD TO RETURN-CODE                                             
013300     GOBACK                                                               
013400     .                                                                    
013500     EJECT                                                                
013600 A-INIT  SECTION.                                                         
013700     SKIP2                                                                
013900     OPEN INPUT INFIL                                                     
014000     .                                                                    
014100     EJECT                                                                
014200 B-LADDA-DATABAS    SECTION.                                              
014300     SKIP2                                                                
014400     CALL W9805410 USING FADD WSRKOD NYCKEL1-PARM                         
014500               NYCKEL2-PARM DATA-PARM                                     
014600     IF WSRKOD NOT = SPACE                                                
014700       DISPLAY 'ERROR FRÅN W9805410 ' WSRKOD                              
014800       DISPLAY 'NYCKEL1 ' NYCKEL1-PARM                                    
014810       DISPLAY 'NYCKEL2 ' NYCKEL2-PARM                                    
014820       DISPLAY 'DATA    ' DATA-PARM                                       
014821                                                                          
014830       CALL W9805410 USING FQUIT WSRKOD                                   
014840       PERFORM S99-ABEND                                                  
014900     END-IF                                                               
033600     .                                                                    
033700     EJECT                                                                
033800 X-OPEN-SOP  SECTION.                                                     
033900     SKIP2                                                                
034100     CALL W9805410 USING FOPEN WSRKOD W9805410-DD-PARM                    
034110                                                                          
034120     IF WSRKOD NOT = SPACE                                                
034130       DISPLAY 'KAN EJ ÖPPNA SOPDATABAS'                                  
034140       PERFORM S99-ABEND                                                  
034150     END-IF                                                               
034200     .                                                                    
034300     SKIP3                                                                
034400 Y-CLOSE-SOP SECTION.                                                     
034500     SKIP2                                                                
034710     CALL W9805410 USING FCLSE WSRKOD                                     
034800     .                                                                    
034900     SKIP3                                                                
035000 Z-FINIT     SECTION.                                                     
035100     SKIP2                                                                
035200     CLOSE INFIL                                                          
035300     .                                                                    
035400     EJECT                                                                
035500 S1-LAES-INFIL SECTION.                                                   
035600     SKIP2                                                                
035700     READ INFIL INTO DATA-RAD                                             
035800       AT END MOVE JA TO EOF-INFIL                                        
035900     END-READ                                                             
036000     .                                                                    
036010 S99-ABEND     SECTION.                                                   
036020     SKIP2                                                                
036030     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
036060     .                                                                    
036100     SKIP3                                                                
