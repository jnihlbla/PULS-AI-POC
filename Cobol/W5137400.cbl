000100 ID  DIVISION.                                                            
000200                                                                          
000300 PROGRAM-ID.    W5137400.                                                 
000400 AUTHOR.        BOO HAMMARIN, GDC GROUP.                                  
000500 DATE-WRITTEN.  JUNI 1997.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*    FUNKTION: SÅLLAR BORT POSTER FRÅN URVALSFIL SOM                      
000900*              REDAN FINNS PÅ INVENTERINGSKÖN.                            
001450     EJECT                                                                
001460 ENVIRONMENT DIVISION.                                                    
001470     SKIP2                                                                
001480 INPUT-OUTPUT SECTION.                                                    
001490                                                                          
001500 FILE-CONTROL.                                                            
001600     SKIP2                                                                
001700*--- URVALSFIL :                                                          
001800                                                                          
001900     SELECT W51371                       ASSIGN TO UT-S-W51374D1.         
002000                                                                          
002110     SELECT W51377                       ASSIGN TO UT-S-W51374D2.         
002120                                                                          
002130     SELECT W51374                       ASSIGN TO UT-S-W51374D3.         
002140                                                                          
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400     SKIP2                                                                
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002601 FD  W51371                                                               
002602     RECORDING  F                                                         
002603     BLOCK CONTAINS 0.                                                    
002604*01  INPOST1 -COPY W51371     -L.                                         
002605     EJECT                                                                
002606                                                                          
002610 FD  W51377                                                               
002620     RECORDING  F                                                         
002630     BLOCK CONTAINS 0.                                                    
002650*01  INPOST2 -COPY W51377     -L.                                         
002660     EJECT                                                                
002670                                                                          
002680 FD  W51374                                                               
002690     RECORDING  F                                                         
002700     BLOCK CONTAINS 0.                                                    
002800*01  UTPOST  -COPY W51371     -L.                                         
002900     EJECT                                                                
003000                                                                          
003900 WORKING-STORAGE SECTION.                                                 
004001                                                                          
004100 77  IDPGM                       PIC X(8)  VALUE 'W5137400'.              
004200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(3) VALUE +16.                     
004300                                                                          
004310 77  EOF-W51371-SW               PIC X       VALUE 'N'.                   
004320     88  EOF-W51371                          VALUE 'J'.                   
004330 77  EOF-W51377-SW               PIC X       VALUE 'N'.                   
004340     88  EOF-W51377                          VALUE 'J'.                   
004700                                                                          
004800 01  DYNAMISKA-SUBPROGRAM.                                                
004900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
006300     EJECT                                                                
006400*01  -COPY W0005      -PRE POSTSUM-                                       
007200     EJECT                                                                
007300 01  FILLER                      PIC X(8)    VALUE 'UT-AREOR'.            
007400                                                                          
007500*01  INPOST1 -COPY W51371 -PRE IN1-                                       
007600     EJECT                                                                
007610*01  INPOST2 -COPY W51377 -PRE IN2-                                       
007620     EJECT                                                                
007630*01  UTPOST  -COPY W51371 -PRE UT-                                        
010600     EJECT                                                                
010700 PROCEDURE DIVISION.                                                      
010710 MAIN SECTION.                                                            
010900                                                                          
011000     PERFORM A-INIT                                                       
012911                                                                          
012912     PERFORM S50-LAES-W51371                                              
012913     PERFORM S51-LAES-W51377                                              
012915     PERFORM UNTIL EOF-W51371                                             
012916       IF EOF-W51377                                                      
012917         PERFORM S52-SKRIV-W51374                                         
012919         PERFORM S50-LAES-W51371                                          
012920       ELSE                                                               
012921         IF (IN1-IDDC    < IN2-IDDC)  OR                                  
012922            (IN1-IDDC    = IN2-IDDC   AND                                 
012923             IN1-IDARTNR < IN2-IDARTNR)                                   
012924           PERFORM S52-SKRIV-W51374                                       
012925           PERFORM S50-LAES-W51371                                        
012926         ELSE                                                             
012927           IF (IN1-IDDC    > IN2-IDDC)  OR                                
012928              (IN1-IDDC    = IN2-IDDC   AND                               
012929               IN1-IDARTNR > IN2-IDARTNR)                                 
012930             PERFORM S51-LAES-W51377                                      
012931           ELSE                                                           
012936             PERFORM S50-LAES-W51371                                      
012937             PERFORM S51-LAES-W51377                                      
012938           END-IF                                                         
012939         END-IF                                                           
012940       END-IF                                                             
012941     END-PERFORM                                                          
012942                                                                          
012943     PERFORM Z-FINIT                                                      
012944     MOVE ZERO TO RETURN-CODE                                             
012950     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200 A-INIT SECTION.                                                          
013300                                                                          
013400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015001                                                                          
015010     OPEN INPUT  W51371 W51377                                            
015011     OPEN OUTPUT W51374                                                   
015020     .                                                                    
015030     EJECT                                                                
020910 Z-FINIT SECTION.                                                         
020941                                                                          
020942     CLOSE W51371                                                         
020943           W51377                                                         
020944           W51374                                                         
020945                                                                          
020949     MOVE 'S' TO POSTSUM-OPKOD                                            
020950     CALL POSTSUM USING POSTSUM-PARM                                      
020951     .                                                                    
020960     SKIP2                                                                
021401 S50-LAES-W51371  SECTION.                                                
021402                                                                          
021403     READ W51371  INTO IN1-W51371                                         
021404     AT END                                                               
021405        MOVE +99999999  TO IN1-IDARTNR                                    
021406        MOVE 'J'        TO EOF-W51371-SW                                  
021407                                                                          
021408     NOT AT END                                                           
021409        MOVE 'URIN'     TO POSTSUM-TRANSTYP                               
021410        MOVE 'W51371'   TO POSTSUM-FDNAMN                                 
021411        MOVE 'W51374D1' TO POSTSUM-DDNAMN2                                
021412        CALL POSTSUM USING POSTSUM-PARM                                   
021413     END-READ                                                             
021414     .                                                                    
021415     EJECT                                                                
021416 S51-LAES-W51377  SECTION.                                                
021417                                                                          
021418     READ W51377 INTO IN2-INVPOST                                         
021419     AT END                                                               
021420        MOVE +99999999  TO IN2-IDARTNR                                    
021421        MOVE 'J'        TO EOF-W51377-SW                                  
021422                                                                          
021423     NOT AT END                                                           
021424        MOVE 'URG'      TO POSTSUM-TRANSTYP                               
021425        MOVE 'W51377'   TO POSTSUM-FDNAMN                                 
021426        MOVE 'W51374D2' TO POSTSUM-DDNAMN2                                
021427        CALL POSTSUM USING POSTSUM-PARM                                   
021428     END-READ                                                             
021429     .                                                                    
021430     EJECT                                                                
021431 S52-SKRIV-W51374     SECTION.                                            
021432                                                                          
021440     WRITE UTPOST FROM IN1-W51371                                         
021450     MOVE 'URUT'        TO POSTSUM-TRANSID                                
021451     MOVE 'W51374'      TO POSTSUM-FDNAMN                                 
021452     MOVE 'W51374D3'    TO POSTSUM-DDNAMN2                                
021460     CALL POSTSUM USING POSTSUM-PARM                                      
021470     .                                                                    
