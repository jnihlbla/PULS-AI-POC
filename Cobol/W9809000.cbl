       ID DIVISION.                                                             
       PROGRAM-ID.     W9809000.                                                
       AUTHOR.         SURESH GUDIVADA.                                         
       DATE-WRITTEN.   24/01/07.                                                
       DATE-COMPILED.                                                           
                                                                                
      *                                                                         
      *      FUNCTION:                                                          
      *          THIS PROGRAM IS USED TO CREATE CSV FILE WITH ABEND             
      *          DURING THE WEEK. IT USES THE EXISTING ROUTINE (W980V1)         
      *          TO PRODUCE THE NEW CSV FILES.                                  
      *                                                                         
      *    INPUT FILE: W980.W980V1.W98032                                       
      *   OUTPUT FILE: W980.W980V1.W98032A                                      
      *                W980.W980V1.W98032B                                      
      *                W980.W980V1.W98032C                                      
      *                                                                         
                                                                                
       SKIP3                                                                    
       ENVIRONMENT DIVISION.                                                    
       SKIP2                                                                    
       INPUT-OUTPUT SECTION.                                                    
                                                                                
       FILE-CONTROL.                                                            
           SKIP2                                                                
      *    --- INDATA                                                           
           SELECT W98090                     ASSIGN TO W98090D1.                
           SKIP2                                                                
      *    --- UTDATA-1                                                         
           SELECT W98091                     ASSIGN TO W98090D2.                
           SKIP2                                                                
      *    --- UTDATA-2                                                         
           SELECT W98092                     ASSIGN TO W98090D3.                
           SKIP2                                                                
      *    --- UTDATA-3                                                         
           SELECT W98093                     ASSIGN TO W98090D4.                
           EJECT                                                                
       DATA DIVISION.                                                           
           SKIP3                                                                
       FILE SECTION.                                                            
           SKIP3                                                                
       FD  W98090                                                               
           RECORDING       F                                                    
           BLOCK CONTAINS  0.                                                   
                                                                                
      *01  -COPY W98090      -L.                                                
           SKIP3                                                                
       FD  W98091                                                               
           RECORDING       V                                                    
           BLOCK CONTAINS  0.                                                   
                                                                                
       01  UT-HEAD-LINE1.                                                       
           03  FILLER       PIC X(80) VALUE SPACE.                              
                                                                                
      *01  RECORD -COPY W98091 -PRE  UT1-  -L.                                  
           SKIP3                                                                
       FD  W98092                                                               
           RECORDING       V                                                    
           BLOCK CONTAINS  0.                                                   
                                                                                
       01  UT-HEAD-LINE2.                                                       
           03  FILLER       PIC X(80) VALUE SPACE.                              
                                                                                
      *01  RECORD -COPY W98092 -PRE  UT2-  -L.                                  
           SKIP3                                                                
       FD  W98093                                                               
           RECORDING       V                                                    
           BLOCK CONTAINS  0.                                                   
                                                                                
       01  UT-HEAD-LINE3.                                                       
           03  FILLER       PIC X(80) VALUE SPACE.                              
                                                                                
      *01  RECORD -COPY W98093 -PRE  UT3-  -L.                                  
           EJECT                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
       77  IDPGM                       PIC X(8)    VALUE 'W9809000'.            
       77  YES                         PIC X       VALUE 'J'.                   
       77  NOO                         PIC X       VALUE 'N'.                   
       77  W98090-EOF-SW               PIC X       VALUE 'N'.                   
           88  END-OF-W98090                       VALUE 'J'.                   
           EJECT                                                                
      *                                                                         
       01  TAB                         PIC X       VALUE X'05'.                 
      *                                                                         
       01  HEADLINE1-AREA.                                                      
           03   HRAD-DATE              PIC X(6)  VALUE 'DATE  '.                
           03   FILLER                 PIC X(1)  VALUE X'05'.                   
           03   HRAD-TIME              PIC X(5)  VALUE 'TIME '.                 
           03   FILLER                 PIC X(1)  VALUE X'05'.                   
           03   HRAD-PSB               PIC X(6)  VALUE 'PSB   '.                
           03   FILLER                 PIC X(1)  VALUE X'05'.                   
           03   HRAD-JOB               PIC X(8)  VALUE 'JOBNAME '.              
           03   FILLER                 PIC X(1)  VALUE X'05'.                   
           03   HRAD-ABEND             PIC X(7)  VALUE 'FELTEXT'.               
           EJECT                                                                
      *                                                                         
       01  HEADLINE2-AREA.                                                      
           03   HRAD-DATE              PIC X(6)  VALUE 'DATE  '.                
           03   FILLER                 PIC X(1)  VALUE X'05'.                   
           03   HRAD-TIME              PIC X(5)  VALUE 'TIME '.                 
           03   FILLER                 PIC X(1)  VALUE X'05'.                   
           03   HRAD-PSB               PIC X(6)  VALUE 'PSB   '.                
           03   FILLER                 PIC X(1)  VALUE X'05'.                   
           03   HRAD-TRAN              PIC X(8)  VALUE 'TRANNAME'.              
           03   FILLER                 PIC X(1)  VALUE X'05'.                   
           03   HRAD-SABEND            PIC X(5)  VALUE 'SCODE'.                 
           03   FILLER                 PIC X(1)  VALUE X'05'.                   
           03   HRAD-UABEND            PIC X(5)  VALUE 'UCODE'.                 
           03   FILLER                 PIC X(1)  VALUE X'05'.                   
           03   HRAD-REGION            PIC X(8)  VALUE 'USER    '.              
           03   FILLER                 PIC X(1)  VALUE X'05'.                   
           03   HRAD-ACTION            PIC X(6)  VALUE 'ACTION'.                
           03   FILLER                 PIC X(1)  VALUE X'05'.                   
           03   HRAD-TYPE              PIC X(3)  VALUE 'TYP'.                   
           03   FILLER                 PIC X(1)  VALUE X'05'.                   
           03   HRAD-CLASS             PIC X(3)  VALUE 'CLS'.                   
           03   FILLER                 PIC X(1)  VALUE X'05'.                   
           03   HRAD-CONTRACT          PIC X(8)  VALUE 'CONTRACT'.              
           EJECT                                                                
      *                                                                         
       01  GENERAL-SUBPROGRAMS.                                                 
      *                                                                         
           03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
           03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
           SKIP2                                                                
      *    --- PARAMETRAR TILL POSTSUM                                          
                                                                                
      *01  -COPY W0005   -PRE  POSTSUM-                                         
           EJECT                                                                
       01  IN-AREA-START               PIC X(24)   VALUE                        
                                 'IN-AREA-START  '.                             
           SKIP2                                                                
                                                                                
      *01  AREA -COPY W98090     -PRE IN-                                       
           EJECT                                                                
                                                                                
       01  UT1-AREA-START              PIC X(24)   VALUE                        
                                 'UT1-AREA-START  '.                            
           SKIP2                                                                
                                                                                
      *01  AREA -COPY W98091     -PRE UT1-                                      
           EJECT                                                                
       01  UT2-AREA-START              PIC X(24)   VALUE                        
                                 'UT2-AREA-START  '.                            
           SKIP2                                                                
                                                                                
      *01  AREA -COPY W98092     -PRE UT2-                                      
           EJECT                                                                
       01  UT3-AREA-START              PIC X(24)   VALUE                        
                                 'UT3-AREA-START  '.                            
           SKIP2                                                                
                                                                                
      *01  AREA -COPY W98093     -PRE UT3-                                      
           EJECT                                                                
       PROCEDURE DIVISION.                                                      
       MAIN SECTION.                                                            
           SKIP2                                                                
                                                                                
           PERFORM A-INIT                                                       
           PERFORM B-WRITE-HEAD-LINE                                            
           PERFORM S01-READ-W98090                                              
           PERFORM UNTIL END-OF-W98090                                          
                IF IN-DATUM IS NUMERIC                                          
                   PERFORM C-GET-ABEND-INFO                                     
                END-IF                                                          
                PERFORM S01-READ-W98090                                         
           END-PERFORM                                                          
                                                                                
           PERFORM Z-FINIT                                                      
                                                                                
           MOVE ZERO TO RETURN-CODE                                             
           GOBACK                                                               
           .                                                                    
           EJECT                                                                
                                                                                
       A-INIT SECTION.                                                          
                                                                                
           OPEN INPUT  W98090                                                   
                                                                                
           OPEN OUTPUT W98091                                                   
                       W98092                                                   
                       W98093                                                   
           SKIP2                                                                
      *    ACCEPT TODAYS-DATE  FROM DATE                                        
           MOVE IDPGM TO POSTSUM-PROGNAMN                                       
           INITIALIZE IN-W98090                                                 
                      UT1-W98091                                                
                      UT2-W98092                                                
                      UT3-W98093                                                
           .                                                                    
           EJECT                                                                
                                                                                
       B-WRITE-HEAD-LINE SECTION.                                               
                                                                                
      *    -- WRITE HEADER DETAILS IN W98091                                    
           WRITE UT-HEAD-LINE1 FROM HEADLINE1-AREA                              
           MOVE 'W98091'   TO POSTSUM-FDNAMN                                    
           MOVE 'W98090D2' TO POSTSUM-DDNAMN2                                   
           MOVE SPACE      TO POSTSUM-TRANSTYP                                  
           CALL POSTSUM USING POSTSUM-PARM                                      
           .                                                                    
           EJECT                                                                
                                                                                
      *    -- WRITE HEADER DETAILS IN W98092                                    
           WRITE UT-HEAD-LINE2 FROM HEADLINE1-AREA                              
           MOVE 'W98092'   TO POSTSUM-FDNAMN                                    
           MOVE 'W98090D3' TO POSTSUM-DDNAMN2                                   
           MOVE SPACE      TO POSTSUM-TRANSTYP                                  
           CALL POSTSUM USING POSTSUM-PARM                                      
           .                                                                    
           EJECT                                                                
                                                                                
      *    -- WRITE HEADER DETAILS IN W98093                                    
           WRITE UT-HEAD-LINE3 FROM HEADLINE2-AREA                              
           MOVE 'W98093'   TO POSTSUM-FDNAMN                                    
           MOVE 'W98090D4' TO POSTSUM-DDNAMN2                                   
           MOVE SPACE      TO POSTSUM-TRANSTYP                                  
           CALL POSTSUM USING POSTSUM-PARM                                      
           .                                                                    
           EJECT                                                                
                                                                                
       C-GET-ABEND-INFO SECTION.                                                
                                                                                
           IF (IN-IDFELMSG(01:2) = ', '         AND                             
               IN-IDFELMSG(12:7) = 'ABENDED')                                   
               MOVE IN-DATUM          TO UT1-DATUM                              
               MOVE IN-TIMINUT        TO UT1-TIMINUT                            
               MOVE IN-IDPSB          TO UT1-IDPSB                              
               MOVE IN-IDFELMSG(03:8) TO UT1-IDJOB                              
               MOVE IN-IDFELMSG(12:7) TO UT1-IDFELMSG                           
               MOVE TAB               TO UT1-TAB-1                              
                                         UT1-TAB-2                              
                                         UT1-TAB-3                              
                                         UT1-TAB-4                              
               PERFORM S11-WRITE-W98091                                         
           END-IF                                                               
                                                                                
           IF (IN-IDFELMSG(01:2)  NOT = ', '  AND                               
               IN-IDFELMSG(13:1)  NOT = 'S'   AND                               
               IN-IDFELMSG(18:1)  NOT = 'U'   AND                               
               IN-IDFELMSG(21:17) NOT = SPACE)                                  
               MOVE IN-DATUM           TO UT2-DATUM                             
               MOVE IN-TIMINUT         TO UT2-TIMINUT                           
               MOVE IN-IDPSB           TO UT2-IDPSB                             
               MOVE IN-IDFELMSG(05:8)  TO UT2-IDJOB                             
               MOVE IN-IDFELMSG(21:35) TO UT2-IDFELMSG                          
               MOVE TAB                TO UT2-TAB-1                             
                                          UT2-TAB-2                             
                                          UT2-TAB-3                             
                                          UT2-TAB-4                             
               PERFORM S12-WRITE-W98092                                         
           END-IF                                                               
                                                                                
           IF (IN-IDFELMSG(13:1) = 'S'    AND                                   
               IN-IDFELMSG(18:1) = 'U')                                         
               MOVE IN-DATUM          TO UT3-DATUM                              
               MOVE IN-TIMINUT        TO UT3-TIMINUT                            
               MOVE IN-IDPSB          TO UT3-IDPSB                              
               MOVE IN-IDFELMSG(04:8) TO UT3-IDPGM                              
               MOVE IN-IDFELMSG(13:4) TO UT3-SCODE                              
               MOVE IN-IDFELMSG(18:5) TO UT3-UCODE                              
               MOVE IN-IDFELMSG(24:8) TO UT3-IDUSER                             
               MOVE IN-IDFELMSG(33:4) TO UT3-IDACTION                           
               MOVE IN-IDFELMSG(38:3) TO UT3-IDTTYP                             
               MOVE IN-IDFELMSG(42:3) TO UT3-IDCLASS                            
               MOVE IN-IDFELMSG(46:8) TO UT3-IDCONTRACT                         
               MOVE TAB               TO UT3-TAB-1                              
                                         UT3-TAB-2                              
                                         UT3-TAB-3                              
                                         UT3-TAB-4                              
                                         UT3-TAB-5                              
                                         UT3-TAB-6                              
                                         UT3-TAB-7                              
                                         UT3-TAB-8                              
                                         UT3-TAB-9                              
                                         UT3-TAB-10                             
               PERFORM S13-WRITE-W98093                                         
           END-IF                                                               
           .                                                                    
           EJECT                                                                
                                                                                
       Z-FINIT SECTION.                                                         
           CLOSE W98090                                                         
                 W98091                                                         
                 W98092                                                         
                 W98093                                                         
           SKIP2                                                                
           MOVE 'S' TO POSTSUM-OPKOD                                            
           CALL POSTSUM USING POSTSUM-PARM                                      
           .                                                                    
           EJECT                                                                
                                                                                
       S01-READ-W98090  SECTION.                                                
           READ W98090 INTO IN-AREA                                             
           AT END                                                               
              MOVE HIGH-VALUE   TO IN-AREA                                      
              SET END-OF-W98090 TO TRUE                                         
                                                                                
           NOT AT END                                                           
           MOVE 'W98090'   TO POSTSUM-FDNAMN                                    
           MOVE 'W98090D1' TO POSTSUM-DDNAMN2                                   
      *    -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES               
           MOVE SPACE      TO POSTSUM-TRANSTYP                                  
           CALL POSTSUM USING POSTSUM-PARM                                      
           END-READ                                                             
           .                                                                    
           EJECT                                                                
                                                                                
       S11-WRITE-W98091 SECTION.                                                
           WRITE UT1-RECORD FROM UT1-AREA                                       
                                                                                
           MOVE 'W98091'   TO POSTSUM-FDNAMN                                    
           MOVE 'W98090D2' TO POSTSUM-DDNAMN2                                   
           MOVE SPACE      TO POSTSUM-TRANSTYP                                  
           CALL POSTSUM USING POSTSUM-PARM                                      
           .                                                                    
           EJECT                                                                
                                                                                
       S12-WRITE-W98092 SECTION.                                                
           WRITE UT2-RECORD FROM UT2-AREA                                       
                                                                                
           MOVE 'W98092'   TO POSTSUM-FDNAMN                                    
           MOVE 'W98090D3' TO POSTSUM-DDNAMN2                                   
           MOVE SPACE      TO POSTSUM-TRANSTYP                                  
           CALL POSTSUM USING POSTSUM-PARM                                      
           .                                                                    
           EJECT                                                                
                                                                                
       S13-WRITE-W98093 SECTION.                                                
           WRITE UT3-RECORD FROM UT3-AREA                                       
                                                                                
           MOVE 'W98093'   TO POSTSUM-FDNAMN                                    
           MOVE 'W98090D4' TO POSTSUM-DDNAMN2                                   
           MOVE SPACE      TO POSTSUM-TRANSTYP                                  
           CALL POSTSUM USING POSTSUM-PARM                                      
           .                                                                    
           EJECT                                                                
                                                                                
