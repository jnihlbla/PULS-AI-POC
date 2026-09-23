010000 ID DIVISION.                                                             
020000 PROGRAM-ID.     W3350500.                                                
030000 AUTHOR.         INGVAR SKJELBRED.                                        
040000 DATE-WRITTEN.   99/06/02.                                                
050000 DATE-COMPILED.                                                           
060000                                                                          
070000*                                                                         
080000*    FUNKTION:                                                            
090000*        LÄGGER TILL TIDEN FÖR ALLA PRISFILER FRÅN ALLA                   
100000*        MARKNADSBOLAG                                                    
110000*                                                                         
120000*                                                                         
130000*    ABENDKODER:                                                          
140000*        U0016 -  . . . .                                                 
150000*        U1000 -  . . . .                                                 
160000*                                                                         
170000                                                                          
180000     SKIP3                                                                
190000 ENVIRONMENT DIVISION.                                                    
200000     SKIP2                                                                
210000 INPUT-OUTPUT SECTION.                                                    
220000                                                                          
230000 FILE-CONTROL.                                                            
240100     SKIP2                                                                
240200*          --- FIL MED NYA PRISER FRÅN MARKNADSBOLAG                      
240300     SELECT W33501                     ASSIGN TO W33505D1.                
240400     SKIP2                                                                
240500*          --- FIL MED NYA PRISER SAMT TID NÄR MAN SKICKAT PRISET         
241000     SELECT W33505                     ASSIGN TO W33505D2.                
260000     EJECT                                                                
270000 DATA DIVISION.                                                           
280000     SKIP3                                                                
290000 FILE SECTION.                                                            
300100     SKIP3                                                                
300706 FD  W33501                                                               
300806     RECORDING       V                                                    
300906     BLOCK CONTAINS  0.                                                   
301006*01  -COPY W335011A      -L.                                              
301200     SKIP3                                                                
301300 FD  W33505                                                               
301606     RECORDING       V                                                    
301706     BLOCK CONTAINS  0.                                                   
301800                                                                          
302000*01  POST -COPY W335011B -PRE  UT-  -L.                                   
310000     EJECT                                                                
320000 WORKING-STORAGE SECTION.                                                 
330000                                                                          
330104                                                                          
331004*    -- CHECKED BY WY2000                                                 
340000 77  IDPGM                       PIC X(8)    VALUE 'W3350500'.            
350000 77  JA                          PIC X       VALUE 'J'.                   
360000 77  NEJ                         PIC X       VALUE 'N'.                   
380100                                                                          
380200 77  W33501-EOF-SW               PIC X       VALUE 'N'.                   
381000     88  END-OF-W33501                       VALUE 'J'.                   
390000     EJECT                                                                
390100                                                                          
391000 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
392000                                                                          
400000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
410000 01  FILLER REDEFINES DAGENS-DATUM.                                       
420000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
430000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
440000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
450000     EJECT                                                                
460000 01  DYNAMISKA-SUBPROGRAM.                                                
470000*                                                                         
480000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
490100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
491000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
500000     SKIP2                                                                
510000*    --- PARAMETRAR TILL ABEND                                            
520000                                                                          
530000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
540000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
550000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
560000     SKIP2                                                                
570000 01  FELTEXT.                                                             
580000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
590000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
600100     EJECT                                                                
600200*    --- PARAMETRAR TILL POSTSUM                                          
600300*                                                                         
601000*01  -COPY W0005   -PRE  POSTSUM-                                         
610100     EJECT                                                                
611000*01  -COPY WDATAREA                                                       
620100     EJECT                                                                
620200 01  IN-AREA-START               PIC X(24)   VALUE                        
620300                                 'IN-AREA-START  '.                       
620400     SKIP2                                                                
620500                                                                          
620600*01  AREA -COPY W335011A     -PRE IN-                                     
620700     EJECT                                                                
620800 01  UT-AREA-START               PIC X(24)   VALUE                        
620900                                 'UT-AREA-START  '.                       
621000     SKIP2                                                                
621100                                                                          
622000*01  AREA -COPY W335011B     -PRE UT-                                     
630000     EJECT                                                                
640000 PROCEDURE DIVISION.                                                      
650000 MAIN SECTION.                                                            
662000*------------------------                                                 
670000     SKIP2                                                                
680000                                                                          
690000     PERFORM A-INIT                                                       
701000     PERFORM S01-LAES-W33501                                              
710002     PERFORM UNTIL END-OF-W33501                                          
721002       PERFORM C-BEARBETA                                                 
781000       PERFORM S01-LAES-W33501                                            
790000     END-PERFORM                                                          
800000                                                                          
810000                                                                          
820000     PERFORM Z-FINIT                                                      
830000                                                                          
840000     MOVE ZERO TO RETURN-CODE                                             
850000     GOBACK                                                               
860000     .                                                                    
870000     EJECT                                                                
880000 A-INIT SECTION.                                                          
890100                                                                          
891000     OPEN INPUT  W33501                                                   
900100                                                                          
901000     OPEN OUTPUT W33505                                                   
910000     SKIP2                                                                
920000     ACCEPT DAGENS-DATUM  FROM DATE                                       
930000     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
931000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
940000     .                                                                    
950000     EJECT                                                                
951001 C-BEARBETA SECTION.                                                      
951201                                                                          
951402     MOVE IN-IDVTYP                   TO UT-IDVTYP                        
951602     MOVE IN-IDMARKBO                 TO UT-IDMARKBO                      
951802     MOVE IN-IDARTNR                  TO UT-IDARTNR                       
952002     MOVE IN-KDRABATT                 TO UT-KDRABATT                      
952202     MOVE IN-PRARTBTO-MARK            TO UT-PRARTBTO-MARK                 
952402     MOVE IN-KDARTKAM                 TO UT-KDARTKAM                      
952505     MOVE WS-TTMMSSTH                 TO UT-TIKLOCK                       
952506     MOVE IN-KDARTRAB-ALT             TO UT-KDARTRAB-ALT                  
952603     PERFORM S11-SKRIV-W33505                                             
953201     .                                                                    
954001     EJECT                                                                
960000 Z-FINIT SECTION.                                                         
970100     CLOSE W33501                                                         
971000           W33505                                                         
980100     SKIP2                                                                
980200     MOVE 'S' TO POSTSUM-OPKOD                                            
981000     CALL POSTSUM USING POSTSUM-PARM                                      
990000     .                                                                    
000100     EJECT                                                                
000200 S01-LAES-W33501  SECTION.                                                
000300     READ W33501 INTO IN-AREA                                             
000400     AT END                                                               
000500        MOVE HIGH-VALUE TO IN-AREA                                        
000600        SET END-OF-W33501 TO TRUE                                         
000700                                                                          
000800     NOT AT END                                                           
000900        MOVE 'W33501' TO POSTSUM-FDNAMN                                   
001000        MOVE 'W33505D1' TO POSTSUM-DDNAMN2                                
001302        MOVE IN-IDVTYP TO POSTSUM-TRANSTYP                                
001400        CALL POSTSUM USING POSTSUM-PARM                                   
001500     END-READ                                                             
002000     .                                                                    
010100     EJECT                                                                
010200 S11-SKRIV-W33505 SECTION.                                                
010300                                                                          
010400     WRITE UT-POST FROM UT-AREA                                           
010500                                                                          
010604     MOVE UT-IDVTYP TO POSTSUM-TRANSTYP                                   
010700     MOVE 'W33505' TO POSTSUM-FDNAMN                                      
010800     MOVE 'W33505D2' TO POSTSUM-DDNAMN2                                   
010900     CALL POSTSUM USING POSTSUM-PARM                                      
011000     .                                                                    
030000     EJECT                                                                
040000 S99-ABEND SECTION.                                                       
050000                                                                          
060100     SKIP2                                                                
060200     MOVE 'S' TO POSTSUM-OPKOD                                            
061000     CALL POSTSUM USING POSTSUM-PARM                                      
070000     CALL ABEND USING RKOD-ABEND                                          
080000     .                                                                    
