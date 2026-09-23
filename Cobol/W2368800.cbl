000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2368800.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   04/03/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄSER FIL TILL SPFU/VSIM                                         
001000*        SKRIVER KORRIGERAD FIL                                           
001100*                                                                         
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000*    SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200*                                                                         
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- INFIL                                                      
002600     SELECT INFIL                      ASSIGN TO W23688D1.                
250400     SKIP2                                                                
250500*          --- UTFIL                                                      
251000     SELECT UTFIL                      ASSIGN TO W23688D2.                
270000     EJECT                                                                
280000 DATA DIVISION.                                                           
290000     SKIP3                                                                
300000 FILE SECTION.                                                            
310100     SKIP3                                                                
310200 FD  INFIL                                                                
310300     RECORDING       F                                                    
310400     BLOCK CONTAINS  0.                                                   
310600*01  -COPY W23684      -L.                                                
310700     EJECT                                                                
310800 FD  UTFIL                                                                
310900     RECORDING       F                                                    
311000     BLOCK CONTAINS  0.                                                   
312000*01  POST -COPY W23684 -PRE  UT-  -L.                                     
320000     EJECT                                                                
330000 WORKING-STORAGE SECTION.                                                 
340000                                                                          
350000 77  IDPGM                       PIC X(8)    VALUE 'W2368800'.            
360000 77  JA                          PIC X       VALUE 'J'.                   
370000 77  NEJ                         PIC X       VALUE 'N'.                   
380000 77  SPAR-PART-ID                PIC X(10)   VALUE SPACE.                 
390000 77  SPAR-IDLEVNR                PIC X(5)    VALUE SPACE.                 
390100 77  SPAR-ORD-DATE               PIC X(10)   VALUE SPACE.                 
390200 77  SPAR-DELIVERED              PIC 9(8)    VALUE ZERO.                  
390300 77  SPAR-ORDERED                PIC 9(8)    VALUE ZERO.                  
390400 77  WS-DELIVERED                PIC 9(8)    VALUE ZERO.                  
390500 77  WS-ORDERED                  PIC 9(8)    VALUE ZERO.                  
390600 77  WS-STAT                     PIC X       VALUE SPACE.                 
390700                                                                          
390800 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
391000     88  END-OF-INFIL                        VALUE 'J'.                   
400000                                                                          
470000 01  DYNAMISKA-SUBPROGRAM.                                                
490000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
501000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
510000     EJECT                                                                
520000*    --- PARAMETRAR TILL ABEND                                            
540000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
550000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
560000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
570000     SKIP2                                                                
580000 01  FELTEXT.                                                             
590000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
600000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
610100     EJECT                                                                
610200*    --- PARAMETRAR TILL POSTSUM                                          
611000*01  -COPY W0005   -PRE  POSTSUM-                                         
630100     EJECT                                                                
630200 01  IN-AREA-START               PIC X(24)   VALUE                        
630300                                 'IN-AREA-START  '.                       
630600*01  AREA -COPY W23684     -PRE IN-                                       
630700     EJECT                                                                
630800 01  UT-AREA-START               PIC X(24)   VALUE                        
630900                                 'UT-AREA-START  '.                       
632000*01  AREA -COPY W23684     -PRE UT-                                       
640000     EJECT                                                                
650000 PROCEDURE DIVISION.                                                      
660000 MAIN SECTION.                                                            
690000                                                                          
700000     PERFORM A-INIT                                                       
711000     PERFORM S01-LAES-INFIL                                               
711100                                                                          
711200     IF END-OF-INFIL                                                      
711300        CONTINUE                                                          
711400     ELSE                                                                 
711500        MOVE IN-FILE-TYPE  TO UT-FILE-TYPE                                
711600        MOVE IN-PLANT      TO UT-PLANT                                    
711700        MOVE IN-SUP-UNIT   TO UT-SUP-UNIT                                 
711800        MOVE IN-ORDERED    TO UT-ORDERED                                  
711900        MOVE IN-STAT-DATE  TO UT-STAT-DATE                                
712000        MOVE IN-STAT-TIME  TO UT-STAT-TIME                                
712100        MOVE IN-PART-ID    TO SPAR-PART-ID                                
712200                              UT-PART-ID                                  
713000        MOVE IN-IDLEVNR    TO SPAR-IDLEVNR                                
713100                              UT-IDLEVNR                                  
714000        MOVE IN-ORD-DATE   TO SPAR-ORD-DATE                               
715000                              UT-ORD-DATE                                 
715100        MOVE IN-ORDERED    TO SPAR-ORDERED                                
715300        MOVE IN-DELIVERED  TO SPAR-DELIVERED                              
715500        MOVE IN-DATUM-YYMMDD TO UT-DATUM-YYMMDD                           
715600     END-IF                                                               
716000                                                                          
720000     PERFORM UNTIL END-OF-INFIL                                           
721000        IF IN-PART-ID   = SPAR-PART-ID                                    
730000        AND IN-IDLEVNR  = SPAR-IDLEVNR                                    
740000        AND IN-ORD-DATE = SPAR-ORD-DATE                                   
750000           ADD IN-DELIVERED TO WS-DELIVERED                               
751102           IF WS-ORDERED = ZERO                                           
751202             ADD IN-ORDERED   TO WS-ORDERED                               
751302           END-IF                                                         
751400           IF IN-STAT = 'A'                                               
752000              MOVE 'A' TO WS-STAT                                         
753000           END-IF                                                         
760000        ELSE                                                              
760100           MOVE WS-DELIVERED  TO UT-DELIVERED                             
760200           MOVE WS-ORDERED    TO UT-ORDERED                               
761000           IF WS-STAT = 'A'                                               
762000              MOVE WS-STAT TO UT-STAT                                     
763000           ELSE                                                           
764000              MOVE 'U'     TO UT-STAT                                     
765000           END-IF                                                         
770000           PERFORM S11-SKRIV-UTPOST                                       
770100           PERFORM S02-NOLLSTALL                                          
770200           MOVE IN-FILE-TYPE  TO UT-FILE-TYPE                             
770300           MOVE IN-PLANT      TO UT-PLANT                                 
770400           MOVE IN-SUP-UNIT   TO UT-SUP-UNIT                              
770500           MOVE IN-ORDERED    TO UT-ORDERED                               
770600           MOVE IN-STAT-DATE  TO UT-STAT-DATE                             
770700           MOVE IN-STAT-TIME  TO UT-STAT-TIME                             
771000           MOVE IN-PART-ID TO SPAR-PART-ID                                
771100                              UT-PART-ID                                  
772000           MOVE IN-IDLEVNR TO SPAR-IDLEVNR                                
772100                              UT-IDLEVNR                                  
773000           MOVE IN-ORD-DATE TO SPAR-ORD-DATE                              
773100                               UT-ORD-DATE                                
773200           MOVE IN-DELIVERED TO WS-DELIVERED                              
773300           MOVE IN-ORDERED   TO WS-ORDERED                                
773400           MOVE IN-DATUM-YYMMDD TO UT-DATUM-YYMMDD                        
773500           MOVE IN-ORDERED  TO SPAR-ORDERED                               
773700           MOVE IN-DELIVERED  TO SPAR-DELIVERED                           
773900           IF IN-STAT = 'A'                                               
774000              MOVE 'A' TO WS-STAT                                         
774100           END-IF                                                         
774500        END-IF                                                            
780000                                                                          
791000        PERFORM S01-LAES-INFIL                                            
800000     END-PERFORM                                                          
820000                                                                          
821000     IF UT-PART-ID = SPACE                                                
822000        CONTINUE                                                          
823000     ELSE                                                                 
823100        MOVE WS-DELIVERED  TO UT-DELIVERED                                
823200        MOVE WS-ORDERED    TO UT-ORDERED                                  
823300        IF WS-STAT = 'A'                                                  
823400           MOVE WS-STAT TO UT-STAT                                        
823500        ELSE                                                              
823600           MOVE 'U'     TO UT-STAT                                        
823700        END-IF                                                            
824000        PERFORM S11-SKRIV-UTPOST                                          
825000     END-IF                                                               
826000                                                                          
830000     PERFORM Z-FINIT                                                      
840000                                                                          
850000     MOVE ZERO TO RETURN-CODE                                             
860000     GOBACK                                                               
870000     .                                                                    
880000     EJECT                                                                
890000 A-INIT SECTION.                                                          
900100                                                                          
901000     OPEN INPUT  INFIL                                                    
911000          OUTPUT UTFIL                                                    
912000     PERFORM S02-NOLLSTALL                                                
950000     .                                                                    
960000     EJECT                                                                
970000 Z-FINIT SECTION.                                                         
980000                                                                          
980100     CLOSE INFIL                                                          
981000           UTFIL                                                          
990200     MOVE 'S' TO POSTSUM-OPKOD                                            
991000     CALL POSTSUM USING POSTSUM-PARM                                      
000000     .                                                                    
010100     EJECT                                                                
010200 S01-LAES-INFIL  SECTION.                                                 
010300                                                                          
010400     READ INFIL INTO IN-AREA                                              
010500     AT END                                                               
010600        MOVE HIGH-VALUE TO IN-AREA                                        
010700        SET END-OF-INFIL TO TRUE                                          
010800                                                                          
010900     NOT AT END                                                           
011000        MOVE 'INFIL'    TO POSTSUM-FDNAMN                                 
011100        MOVE 'W23688D1' TO POSTSUM-DDNAMN2                                
011300        MOVE SPACE      TO POSTSUM-TRANSTYP                               
011400        CALL POSTSUM USING POSTSUM-PARM                                   
011500     END-READ                                                             
012000     .                                                                    
020100     EJECT                                                                
020200 S02-NOLLSTALL SECTION.                                                   
020300                                                                          
020600     MOVE SPACE TO UT-IDLEVNR                                             
020700                   UT-PART-ID                                             
020800                   UT-ORD-DATE                                            
020900                   UT-STAT                                                
021000                   WS-STAT                                                
021100     MOVE ZERO  TO UT-SUP-UNIT                                            
021200                   UT-ORDERED                                             
021300                   WS-ORDERED                                             
021400                   UT-DELIVERED                                           
021500                   WS-DELIVERED                                           
021700                                                                          
021800     MOVE SPACE TO SPAR-PART-ID                                           
021900                   SPAR-IDLEVNR                                           
022000                   SPAR-ORD-DATE                                          
022100     MOVE ZERO  TO SPAR-DELIVERED                                         
022200                   SPAR-ORDERED                                           
022300     .                                                                    
022400     EJECT                                                                
022500 S11-SKRIV-UTPOST SECTION.                                                
022600                                                                          
023900     WRITE UT-POST FROM UT-AREA                                           
024000                                                                          
024100     MOVE SPACE       TO POSTSUM-TRANSTYP                                 
024200     MOVE 'UTFIL'     TO POSTSUM-FDNAMN                                   
024300     MOVE 'W23688D2'  TO POSTSUM-DDNAMN2                                  
024400     CALL POSTSUM USING POSTSUM-PARM                                      
025000     .                                                                    
