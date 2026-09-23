010065 ID DIVISION.                                                             
020099 PROGRAM-ID.     W3354B00.                                                
030099 AUTHOR.         STINA MOGREN.                                            
040099 DATE-WRITTEN.   03/11/26.                                                
050036 DATE-COMPILED.                                                           
060036                                                                          
070036*    FUNKTION:                                                            
080099*      - KOPIERAR PRISFIL INNEHÅLLANDE NYA SJÄLVKOSTPRISER TILL           
090099*        MÅNADSKURS MED 'USA-PRISFIL', OCH KOMPLETTERAD                   
100099*        MED DE NYA SJÄLVKOSTNADSPRISERNA,                                
110099*        PRISERNA RÄKNAS OM TILL MARKNADSBOLAGSVALUTA                     
120099*        EN FIL SKAPAS FÖR VARJE MARKNADSBOLAG                            
160036*                                                                         
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
300099*       --- INFIL FRÅN W33540                                             
310099     SELECT W33540                     ASSIGN TO W3354BD1.                
320099     SKIP2                                                                
330099*       --- SAMMA SOM IN, MED ÄNDRAD SJÄLVKOST/STANDARD-PRIS              
340099     SELECT W3354BA                    ASSIGN TO W3354BD2.                
341099*       --- SAMMA SOM IN, MED ÄNDRAD SJÄLVKOST/STANDARD-PRIS              
342099     SELECT W3354BB                    ASSIGN TO W3354BD3.                
343099*       --- SAMMA SOM IN, MED ÄNDRAD SJÄLVKOST/STANDARD-PRIS              
344099     SELECT W3354BC                    ASSIGN TO W3354BD4.                
345099*       --- SAMMA SOM IN, MED ÄNDRAD SJÄLVKOST/STANDARD-PRIS              
346099     SELECT W3354BD                    ASSIGN TO W3354BD5.                
347099*       --- SAMMA SOM IN, MED ÄNDRAD SJÄLVKOST/STANDARD-PRIS              
348099     SELECT W3354BE                    ASSIGN TO W3354BD6.                
349099*       --- SAMMA SOM IN, MED ÄNDRAD SJÄLVKOST/STANDARD-PRIS              
349199     SELECT W3354BF                    ASSIGN TO W3354BD7.                
349299*       --- SAMMA SOM IN, MED ÄNDRAD SJÄLVKOST/STANDARD-PRIS              
349399     SELECT W3354BG                    ASSIGN TO W3354BD8.                
350036     EJECT                                                                
360036 DATA DIVISION.                                                           
370036     SKIP2                                                                
380036 FILE SECTION.                                                            
390036     SKIP3                                                                
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
520099 FD  W3354BA                                                              
530099     RECORDING       V                                                    
540000     BLOCK CONTAINS  0.                                                   
550000                                                                          
551099*01  POST -COPY W335401A -PRE  UTA1-  -L.                                 
552099     SKIP2                                                                
553099*01  POST -COPY W335402A -PRE  UTA2-  -L.                                 
554099     SKIP2                                                                
555099*01  POST -COPY W335403A -PRE  UTA3-  -L.                                 
570000     EJECT                                                                
571099 FD  W3354BB                                                              
572099     RECORDING       V                                                    
573099     BLOCK CONTAINS  0.                                                   
574099                                                                          
575099*01  POST -COPY W335401A -PRE  UTB1-  -L.                                 
576099     SKIP2                                                                
577099*01  POST -COPY W335402A -PRE  UTB2-  -L.                                 
578099     SKIP2                                                                
579099*01  POST -COPY W335403A -PRE  UTB3-  -L.                                 
579199     EJECT                                                                
579299 FD  W3354BC                                                              
579399     RECORDING       V                                                    
579499     BLOCK CONTAINS  0.                                                   
579599                                                                          
579699*01  POST -COPY W335401A -PRE  UTC1-  -L.                                 
579799     SKIP2                                                                
579899*01  POST -COPY W335402A -PRE  UTC2-  -L.                                 
579999     SKIP2                                                                
580099*01  POST -COPY W335403A -PRE  UTC3-  -L.                                 
580199     EJECT                                                                
580299 FD  W3354BD                                                              
580399     RECORDING       V                                                    
580499     BLOCK CONTAINS  0.                                                   
580599                                                                          
580699*01  POST -COPY W335401A -PRE  UTD1-  -L.                                 
580799     SKIP2                                                                
580899*01  POST -COPY W335402A -PRE  UTD2-  -L.                                 
580999     SKIP2                                                                
581099*01  POST -COPY W335403A -PRE  UTD3-  -L.                                 
581199     EJECT                                                                
581299 FD  W3354BE                                                              
581399     RECORDING       V                                                    
581499     BLOCK CONTAINS  0.                                                   
581599                                                                          
581699*01  POST -COPY W335401A -PRE  UTE1-  -L.                                 
581799     SKIP2                                                                
581899*01  POST -COPY W335402A -PRE  UTE2-  -L.                                 
581999     SKIP2                                                                
582099*01  POST -COPY W335403A -PRE  UTE3-  -L.                                 
582199     EJECT                                                                
582299 FD  W3354BF                                                              
582399     RECORDING       V                                                    
582499     BLOCK CONTAINS  0.                                                   
582599                                                                          
582699*01  POST -COPY W335401A -PRE  UTF1-  -L.                                 
582799     SKIP2                                                                
582899*01  POST -COPY W335402A -PRE  UTF2-  -L.                                 
582999     SKIP2                                                                
583099*01  POST -COPY W335403A -PRE  UTF3-  -L.                                 
583199     EJECT                                                                
583299 FD  W3354BG                                                              
583399     RECORDING       V                                                    
583499     BLOCK CONTAINS  0.                                                   
583599                                                                          
583699*01  POST -COPY W335401A -PRE  UTG1-  -L.                                 
583799     SKIP2                                                                
583899*01  POST -COPY W335402A -PRE  UTG2-  -L.                                 
583999     SKIP2                                                                
584099*01  POST -COPY W335403A -PRE  UTG3-  -L.                                 
584199     EJECT                                                                
585099 WORKING-STORAGE SECTION.                                                 
590023                                                                          
600099 77  IDPGM                       PIC X(8)      VALUE 'W3354B00'.          
610026 77  JA                          PIC X         VALUE 'J'.                 
620026 77  NEJ                         PIC X         VALUE 'N'.                 
640076                                                                          
650076 77  WS-KDARTURS                 PIC 9(2)      VALUE ZERO.                
660076 77  WS-IDLEVNR                  PIC 9(5)      VALUE ZERO.                
670076 77  WS-IDLEVNR-LOC              PIC 9(5)      VALUE ZERO.                
671099 77  WS-DATUM                    PIC 9(6)      VALUE ZERO.                
680076                                                                          
690076 01  WS-PRARTSJK                 PIC 9(7)V9(2) VALUE ZERO.                
691093                                                                          
692093 77  IX                          PIC S9(3)  VALUE ZERO COMP-3.            
692094                                                                          
710099 77  W33540-EOF-SW               PIC X         VALUE 'N'.                 
720099     88  END-OF-W33540                         VALUE 'J'.                 
730061                                                                          
740026 01  DAGENS-DATUM                PIC 9(8)      VALUE ZERO.                
750000     EJECT                                                                
760054 01  WS-AAAAMMDD.                                                         
770055     03  WS-SEKEL                       PIC 9(2).                         
780055     03  WS-AAMMDD                      PIC 9(6).                         
790055 01  WS-TIFINLV REDEFINES WS-AAAAMMDD   PIC 9(8).                         
800055*                                                                         
810000 01  DYNAMISKA-SUBPROGRAM.                                                
820000*                                                                         
830000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
840000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
850000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
870050     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
880000     SKIP2                                                                
881093                                                                          
883193                                                                          
883299* INFO OM KÖRTYP(M?) FRÅN CONSTANTMEDLEM VALD AV JCL'EN                   
883398* INFON KOMMER SOM FIL D1                                                 
883493                                                                          
890000 01  FELTEXT.                                                             
900000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
910000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
920034     EJECT                                                                
930034*    --- PARAMETRAR TILL SUBPROGRAM W400ARTU                              
940041*01 -COPY W400ARTU                                                        
950034     EJECT                                                                
951093*    --- PARAMETRAR TILL SUBPROGRAM W335CURR                              
952093*01 -COPY W335CURR                                                        
953093     EJECT                                                                
960000*    --- PARAMETRAR TILL POSTSUM                                          
970000*                                                                         
980000*01  -COPY W0005   -PRE  POSTSUM-                                         
990000     EJECT                                                                
000051 01  FILLER                      PIC X(16) VALUE 'WDATAREA'.              
010051*01 -COPY WDATAREA                                                        
020051     EJECT                                                                
030074 01  FILLER                      PIC X(24)   VALUE 'IN-AREA'.             
031099 01  IN-AREA.                                                             
031199     03  IN-IDTYP                PIC X(3).                                
032099     03  FILLER                  PIC X(400).                              
040099*01  FILLER -COPY W335401A     -PRE IN1-   -RED  IN-AREA                  
041099*01  FILLER -COPY W335402A     -PRE IN2-   -RED  IN-AREA                  
042099*01  FILLER -COPY W335403A     -PRE IN3-   -RED  IN-AREA                  
060074     EJECT                                                                
110074 01  FILLER                      PIC X(24)   VALUE 'UT-AREA'.             
131099     SKIP2                                                                
132099 01  UT-AREA.                                                             
133099     03  UT-IDPTYP               PIC X(3).                                
134099     03  FILLER                  PIC X(400).                              
135099*01  FILLER -COPY W335401A     -PRE UT1-   -RED  UT-AREA                  
136099*01  FILLER -COPY W335402A     -PRE UT2-   -RED  UT-AREA                  
137099*01  FILLER -COPY W335403A     -PRE UT3-   -RED  UT-AREA                  
140000     EJECT                                                                
149994                                                                          
152394 LINKAGE SECTION.                                                         
152999     EJECT                                                                
153099 PROCEDURE DIVISION.                                                      
153194                                                                          
154094 MAIN SECTION.                                                            
160000                                                                          
170000     PERFORM A-INIT                                                       
180000                                                                          
190099     PERFORM S01-LAES-W33540                                              
210099     PERFORM UNTIL END-OF-W33540                                          
224099                                                                          
360099         PERFORM B-FLYTTA-SKRIV-UTPOST                                    
380099         PERFORM S01-LAES-W33540                                          
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
610099          OUTPUT W3354BA                                                  
611099                 W3354BB                                                  
612099                 W3354BC                                                  
613099                 W3354BD                                                  
614099                 W3354BE                                                  
615099                 W3354BF                                                  
616099                 W3354BG                                                  
620000                                                                          
630003     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
630199                                                                          
640000     .                                                                    
650030     EJECT                                                                
660099 B-FLYTTA-SKRIV-UTPOST SECTION.                                           
670099                                                                          
671099     MOVE IN-AREA            TO UT-AREA                                   
672099     EVALUATE TRUE                                                        
673099     WHEN UT-IDPTYP = 401                                                 
675099       MOVE IN-AREA          TO UT-AREA                                   
676099       MOVE 1                TO IX                                        
980099                                                                          
990099       WRITE UTA1-POST    FROM UT-AREA                                    
000099                                                                          
010099       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
020099       MOVE 'W3354BD2'    TO POSTSUM-DDNAMN2                              
030099       CALL POSTSUM  USING POSTSUM-PARM                                   
030199*                                                                         
030299       MOVE IN-AREA          TO UT-AREA                                   
030399       MOVE 2                TO IX                                        
030599                                                                          
030699       WRITE UTB1-POST    FROM UT-AREA                                    
030799                                                                          
030899       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
030999       MOVE 'W3354BD3'    TO POSTSUM-DDNAMN2                              
031099       CALL POSTSUM  USING POSTSUM-PARM                                   
031199                                                                          
031299*                                                                         
031399       MOVE IN-AREA          TO UT-AREA                                   
031499       MOVE 3                TO IX                                        
031699                                                                          
031799       WRITE UTC1-POST    FROM UT-AREA                                    
031899                                                                          
031999       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
032099       MOVE 'W3354BD4'    TO POSTSUM-DDNAMN2                              
032199       CALL POSTSUM  USING POSTSUM-PARM                                   
032299                                                                          
032399*                                                                         
032499       MOVE IN-AREA          TO UT-AREA                                   
032599       MOVE 4                TO IX                                        
032799                                                                          
032899       WRITE UTD1-POST    FROM UT-AREA                                    
032999                                                                          
033099       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
033199       MOVE 'W3354BD5'    TO POSTSUM-DDNAMN2                              
033299       CALL POSTSUM  USING POSTSUM-PARM                                   
033399                                                                          
033499*                                                                         
033599       MOVE IN-AREA          TO UT-AREA                                   
033699       MOVE 5                TO IX                                        
033899                                                                          
033999       WRITE UTE1-POST    FROM UT-AREA                                    
034099                                                                          
034199       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
034299       MOVE 'W3354BD6'    TO POSTSUM-DDNAMN2                              
034399       CALL POSTSUM  USING POSTSUM-PARM                                   
034499                                                                          
034599*                                                                         
034699       MOVE IN-AREA          TO UT-AREA                                   
034799       MOVE 6                TO IX                                        
034999                                                                          
035099       WRITE UTF1-POST    FROM UT-AREA                                    
035199                                                                          
035299       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
035399       MOVE 'W3354BD7'    TO POSTSUM-DDNAMN2                              
035499       CALL POSTSUM  USING POSTSUM-PARM                                   
035599                                                                          
035699*                                                                         
035799       MOVE IN-AREA          TO UT-AREA                                   
035899       MOVE 7                TO IX                                        
036099                                                                          
036199       WRITE UTG1-POST    FROM UT-AREA                                    
036299                                                                          
036399       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
036499       MOVE 'W3354BD8'    TO POSTSUM-DDNAMN2                              
036599       CALL POSTSUM  USING POSTSUM-PARM                                   
036699                                                                          
036799     WHEN UT-IDPTYP = 402                                                 
036899       WRITE UTA2-POST    FROM UT-AREA                                    
036999                                                                          
037099       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
037199       MOVE 'W3354BD2'    TO POSTSUM-DDNAMN2                              
037299       CALL POSTSUM  USING POSTSUM-PARM                                   
037399*                                                                         
037499       WRITE UTB2-POST    FROM UT-AREA                                    
037599                                                                          
037699       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
037799       MOVE 'W3354BD3'    TO POSTSUM-DDNAMN2                              
037899       CALL POSTSUM  USING POSTSUM-PARM                                   
037999*                                                                         
038099       WRITE UTC2-POST    FROM UT-AREA                                    
038199                                                                          
038299       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
038399       MOVE 'W3354BD4'    TO POSTSUM-DDNAMN2                              
038499       CALL POSTSUM  USING POSTSUM-PARM                                   
038599*                                                                         
038699       WRITE UTD2-POST    FROM UT-AREA                                    
038799                                                                          
038899       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
038999       MOVE 'W3354BD5'    TO POSTSUM-DDNAMN2                              
039099       CALL POSTSUM  USING POSTSUM-PARM                                   
039199*                                                                         
039299       WRITE UTE2-POST    FROM UT-AREA                                    
039399                                                                          
039499       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
039599       MOVE 'W3354BD6'    TO POSTSUM-DDNAMN2                              
039699       CALL POSTSUM  USING POSTSUM-PARM                                   
039799*                                                                         
039899       WRITE UTF2-POST    FROM UT-AREA                                    
039999                                                                          
040099       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
040199       MOVE 'W3354BD7'    TO POSTSUM-DDNAMN2                              
040299       CALL POSTSUM  USING POSTSUM-PARM                                   
040399*                                                                         
040499       WRITE UTG2-POST    FROM UT-AREA                                    
040599                                                                          
040699       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
040799       MOVE 'W3354BD8'    TO POSTSUM-DDNAMN2                              
040899       CALL POSTSUM  USING POSTSUM-PARM                                   
040999                                                                          
041099     WHEN UT-IDPTYP = 403                                                 
041199       WRITE UTA3-POST    FROM UT-AREA                                    
041299                                                                          
041399       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
041499       MOVE 'W3354BD2'    TO POSTSUM-DDNAMN2                              
041599       CALL POSTSUM  USING POSTSUM-PARM                                   
041699*                                                                         
041799       WRITE UTB3-POST    FROM UT-AREA                                    
041899                                                                          
041999       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
042099       MOVE 'W3354BD3'    TO POSTSUM-DDNAMN2                              
042199       CALL POSTSUM  USING POSTSUM-PARM                                   
042299*                                                                         
042399       WRITE UTC3-POST    FROM UT-AREA                                    
042499                                                                          
042599       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
042699       MOVE 'W3354BD4'    TO POSTSUM-DDNAMN2                              
042799       CALL POSTSUM  USING POSTSUM-PARM                                   
042899*                                                                         
042999       WRITE UTD3-POST    FROM UT-AREA                                    
043099                                                                          
043199       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
043299       MOVE 'W3354BD5'    TO POSTSUM-DDNAMN2                              
043399       CALL POSTSUM  USING POSTSUM-PARM                                   
043499*                                                                         
043599       WRITE UTE3-POST    FROM UT-AREA                                    
043699                                                                          
043799       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
043899       MOVE 'W3354BD6'    TO POSTSUM-DDNAMN2                              
043999       CALL POSTSUM  USING POSTSUM-PARM                                   
044099*                                                                         
044199       WRITE UTF3-POST    FROM UT-AREA                                    
044299                                                                          
044399       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
044499       MOVE 'W3354BD7'    TO POSTSUM-DDNAMN2                              
044599       CALL POSTSUM  USING POSTSUM-PARM                                   
044699*                                                                         
044799       WRITE UTG3-POST    FROM UT-AREA                                    
044899                                                                          
044999       MOVE 'W3354B'      TO POSTSUM-FDNAMN                               
045099       MOVE 'W3354BD8'    TO POSTSUM-DDNAMN2                              
045199       CALL POSTSUM  USING POSTSUM-PARM                                   
045299                                                                          
046099     END-EVALUATE                                                         
050099     .                                                                    
060000 Z-FINIT SECTION.                                                         
061076                                                                          
071099     CLOSE W33540                                                         
080099           W3354BA                                                        
080199           W3354BB                                                        
080299           W3354BC                                                        
080399           W3354BD                                                        
080499           W3354BE                                                        
080599           W3354BF                                                        
080699           W3354BG                                                        
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
220099        MOVE 'W3354BD1' TO POSTSUM-DDNAMN2                                
280000        CALL POSTSUM USING POSTSUM-PARM                                   
290000     END-READ                                                             
300003     .                                                                    
310074     EJECT                                                                
