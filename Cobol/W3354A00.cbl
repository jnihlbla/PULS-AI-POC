010065 ID DIVISION.                                                             
020099 PROGRAM-ID.     W3354A00.                                                
030099 AUTHOR.         STINA MOGREN.                                            
040099 DATE-WRITTEN.   03/10/20.                                                
050036 DATE-COMPILED.                                                           
060036                                                                          
070036*    FUNKTION:                                                            
080099*      - KOPIERAR PRISFIL INNEHÅLLANDE NYA SJÄLVKOSTPRISER TILL           
090099*        MÅNADSKURS MED 'USA-PRISFIL', OCH KOMPLETTERAD                   
100099*        MED DE NYA SJÄLVKOSTNADSPRISERNA,                                
110099*        PRISERNA RÄKNAS OM TILL MARKNADSBOLAGSVALUTA                     
160036*                                                                         
160037*                                                                         
170036*    ABENDKODER:                                                          
180036*        U0016 -  . . . .                                                 
190036                                                                          
200036     SKIP3                                                                
210036 ENVIRONMENT DIVISION.                                                    
220036     SKIP2                                                                
230036 INPUT-OUTPUT SECTION.                                                    
240036                                                                          
250036 FILE-CONTROL.                                                            
260036     SKIP2                                                                
261098*       --- UPPGIFT OM MARKNADSBOLAG                                      
262099     SELECT W335TYP                    ASSIGN TO W3354AD1.                
270099*       --- INFIL FRÅN W33540                                             
280099     SELECT W33540                     ASSIGN TO W3354AD2.                
290036     SKIP2                                                                
330099*       --- SAMMA SOM IN, MED ÄNDRAD SJÄLVKOST/STANDARD-PRIS              
340099     SELECT W3354A                     ASSIGN TO W3354AD3.                
350036     EJECT                                                                
360036 DATA DIVISION.                                                           
370036     SKIP2                                                                
380036 FILE SECTION.                                                            
390036     SKIP3                                                                
391099 FD  W335TYP                                                              
392098     LABEL RECORD STANDARD                                                
393098     RECORDING F                                                          
394098     BLOCK CONTAINS 0.                                                    
395098                                                                          
396098 01  FILLER                  PIC X(80).                                   
397098                                                                          
400099 FD  W33540                                                               
410099     RECORDING       V                                                    
420036     BLOCK CONTAINS  0.                                                   
430036                                                                          
431099*01  POST -COPY W335401A -PRE  IN1-  -L.                                  
432099     SKIP2                                                                
433099*01  POST -COPY W335402A -PRE  IN2-  -L.                                  
434099     SKIP2                                                                
435099*01  POST -COPY W335403A -PRE  IN3-  -L.                                  
450074     SKIP3                                                                
520099 FD  W3354A                                                               
530099     RECORDING       V                                                    
540000     BLOCK CONTAINS  0.                                                   
550000                                                                          
551099*01  POST -COPY W335401A -PRE  UT1-  -L.                                  
552099     SKIP2                                                                
553099*01  POST -COPY W335402A -PRE  UT2-  -L.                                  
554099     SKIP2                                                                
555099*01  POST -COPY W335403A -PRE  UT3-  -L.                                  
570000     EJECT                                                                
580000 WORKING-STORAGE SECTION.                                                 
590023                                                                          
600099 77  IDPGM                       PIC X(8)      VALUE 'W3354A00'.          
610026 77  JA                          PIC X         VALUE 'J'.                 
620026 77  NEJ                         PIC X         VALUE 'N'.                 
640076                                                                          
650076 77  WS-KDARTURS                 PIC 9(2)      VALUE ZERO.                
660076 77  WS-IDLEVNR                  PIC 9(5)      VALUE ZERO.                
670076 77  WS-IDLEVNR-LOC              PIC 9(5)      VALUE ZERO.                
680076                                                                          
690076 01  WS-PRARTSJK                 PIC 9(7)V9(2) VALUE ZERO.                
691093                                                                          
694093 77  WS-IDMARKBO                 PIC X      VALUE SPACE.                  
696093 77  WS-KDVALISO-COST            PIC X(3)   VALUE 'SEK'.                  
799699 77  WS-DATUM                    PIC 9(6)   VALUE ZERO.                   
394899                                                                          
394999 77  W33540-EOF-SW               PIC X         VALUE 'N'.                 
395099     88  END-OF-W33540                         VALUE 'J'.                 
395199                                                                          
395599 01  DAGENS-DATUM                PIC 9(8)      VALUE ZERO.                
395699     EJECT                                                                
395799 01  WS-AAAAMMDD.                                                         
395899     03  WS-SEKEL                       PIC 9(2).                         
395999     03  WS-AAMMDD                      PIC 9(6).                         
396099 01  WS-TIFINLV REDEFINES WS-AAAAMMDD   PIC 9(8).                         
396199*                                                                         
396299 01  DYNAMISKA-SUBPROGRAM.                                                
396399*                                                                         
396499     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
396599     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
396699     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
396799     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
396899     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
396999     03  W335CURR                PIC X(8)    VALUE 'W335CURR'.            
397099     SKIP2                                                                
397199                                                                          
397599* INFO OM KÖRTYP(M?) FRÅN CONSTANTMEDLEM VALD AV JCL'EN                   
397699* INFON KOMMER SOM FIL D1                                                 
397799                                                                          
397899 01  W335TYP-POST.                                                        
397999     03  TYP-PARAMETER        PIC X(2).                                   
398099         88 EJ-MARKNAD        VALUE 'M '.                                 
398199         88 ME-MARKNAD        VALUE 'ME'.                                 
398299     03  FILLER               REDEFINES TYP-PARAMETER.                    
398399       05  TYP-M              PIC X.                                      
398499       05  TYP-BOLAG          PIC X.                                      
398599     03  FILLER               PIC X(78).                                  
398699                                                                          
398799 01  FELTEXT.                                                             
398899     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
398999     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
399099     EJECT                                                                
399199*    --- PARAMETRAR TILL SUBPROGRAM W400ARTU                              
399299*01 -COPY W400ARTU                                                        
399399     EJECT                                                                
399499*    --- PARAMETRAR TILL SUBPROGRAM W335CURR                              
399599*01 -COPY W335CURR                                                        
399699     EJECT                                                                
399799*    --- PARAMETRAR TILL POSTSUM                                          
399899*                                                                         
399999*01  -COPY W0005   -PRE  POSTSUM-                                         
400099     EJECT                                                                
400199 01  FILLER                      PIC X(16) VALUE 'WDATAREA'.              
400299*01 -COPY WDATAREA                                                        
400399     EJECT                                                                
400499 01  FILLER                      PIC X(24)   VALUE 'IN-AREA'.             
400599 01  IN-AREA.                                                             
400699     03  IN-IDTYP                PIC X(3).                                
400799     03  FILLER                  PIC X(400).                              
400899*01  FILLER -COPY W335401A     -PRE IN1-   -RED  IN-AREA                  
400999*01  FILLER -COPY W335402A     -PRE IN2-   -RED  IN-AREA                  
401099*01  FILLER -COPY W335403A     -PRE IN3-   -RED  IN-AREA                  
401199     EJECT                                                                
401699 01  FILLER                      PIC X(24)   VALUE 'UT-AREA'.             
401799     SKIP2                                                                
401899 01  UT-AREA.                                                             
401999     03  UT-IDPTYP               PIC X(3).                                
402099     03  FILLER                  PIC X(400).                              
402199*01  FILLER -COPY W335401A     -PRE UT1-   -RED  UT-AREA                  
402299*01  FILLER -COPY W335402A     -PRE UT2-   -RED  UT-AREA                  
402399*01  FILLER -COPY W335403A     -PRE UT3-   -RED  UT-AREA                  
402499     EJECT                                                                
406899 LINKAGE SECTION.                                                         
407199     EJECT                                                                
407599 PROCEDURE DIVISION.                                                      
407699                                                                          
407799 MAIN SECTION.                                                            
407999                                                                          
408099     PERFORM A-INIT                                                       
408199                                                                          
408299     PERFORM S01-LAES-W33540                                              
408399     PERFORM UNTIL END-OF-W33540                                          
408499                                                                          
409099         PERFORM B-FLYTTA-SKRIV-UTPOST                                    
410099         PERFORM S01-LAES-W33540                                          
500000     END-PERFORM                                                          
510000                                                                          
520000     PERFORM Z-FINIT                                                      
530000                                                                          
540000     MOVE ZERO TO RETURN-CODE                                             
550000     GOBACK                                                               
560000     .                                                                    
570000     EJECT                                                                
580000 A-INIT SECTION.                                                          
590000                                                                          
600099     OPEN INPUT  W33540                                                   
602099          INPUT  W335TYP                                                  
610099          OUTPUT W3354A                                                   
620000                                                                          
632099     PERFORM S04-LAES-W335TYP                                             
640000     .                                                                    
650030     EJECT                                                                
660099 B-FLYTTA-SKRIV-UTPOST SECTION.                                           
670099                                                                          
671099     MOVE IN-AREA            TO UT-AREA                                   
672099     EVALUATE TRUE                                                        
673099     WHEN UT-IDPTYP = 401                                                 
980099                                                                          
990099       WRITE UT1-POST   FROM UT-AREA                                      
000099                                                                          
010099       MOVE 'W3354A'    TO POSTSUM-FDNAMN                                 
020099       MOVE 'W3354AD3'  TO POSTSUM-DDNAMN2                                
030099       CALL POSTSUM  USING POSTSUM-PARM                                   
030199                                                                          
031099     WHEN UT-IDPTYP = 402                                                 
031199       WRITE UT2-POST   FROM UT-AREA                                      
031299                                                                          
031399       MOVE 'W3354A'    TO POSTSUM-FDNAMN                                 
031499       MOVE 'W3354AD3'  TO POSTSUM-DDNAMN2                                
031599       CALL POSTSUM  USING POSTSUM-PARM                                   
031699                                                                          
032099     WHEN UT-IDPTYP = 403                                                 
032199       WRITE UT3-POST   FROM UT-AREA                                      
032299                                                                          
032399       MOVE 'W3354A'    TO POSTSUM-FDNAMN                                 
032499       MOVE 'W3354AD3'  TO POSTSUM-DDNAMN2                                
032599       CALL POSTSUM  USING POSTSUM-PARM                                   
033099     END-EVALUATE                                                         
040099     .                                                                    
060000 Z-FINIT SECTION.                                                         
061076                                                                          
070099     CLOSE W33540                                                         
080099           W3354A                                                         
081099           W335TYP                                                        
090076                                                                          
100099     MOVE 'S'           TO POSTSUM-OPKOD                                  
110099     CALL POSTSUM    USING POSTSUM-PARM                                   
120000     .                                                                    
130000     EJECT                                                                
140099 S01-LAES-W33540  SECTION.                                                
150074                                                                          
160099     READ W33540 INTO IN-AREA                                             
170000     AT END                                                               
190099        SET END-OF-W33540 TO TRUE                                         
200000     NOT AT END                                                           
210099        MOVE 'W33540'   TO POSTSUM-FDNAMN                                 
220099        MOVE 'W3354AD2' TO POSTSUM-DDNAMN2                                
280000        CALL POSTSUM USING POSTSUM-PARM                                   
290000     END-READ                                                             
300003     .                                                                    
310074     EJECT                                                                
861099 S04-LAES-W335TYP   SECTION.                                              
862093                                                                          
863099     READ W335TYP             INTO W335TYP-POST                           
864093     .                                                                    
865093     SKIP3                                                                
