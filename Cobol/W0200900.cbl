000100 ID DIVISION.                                                             
000201                                                                          
000301 PROGRAM-ID.     W0200900.                                                
000401 AUTHOR.         UMESH JAIN                                               
000501 DATE-WRITTEN.   21/03/22                                                 
000601 DATE-COMPILED.                                                           
000701                                                                          
000801*    FUNKTION:                                                            
000901*        READ W02009, W02031 AND CONVERT IT INTO EDITABLE FORMAT          
001001*        FOR WXTR FILE.                                                   
001401*                                                                         
001501*    ABENDKODER:                                                          
001601*        U0016 -  . . . .                                                 
001701*        U1000 -  . . . .                                                 
001801*                                                                         
001901                                                                          
002001     SKIP3                                                                
003001 ENVIRONMENT DIVISION.                                                    
003101     SKIP2                                                                
003201 INPUT-OUTPUT SECTION.                                                    
003301                                                                          
003401 FILE-CONTROL.                                                            
003501     SKIP2                                                                
003601     SELECT IN-W02009                ASSIGN TO W02009D1.                  
003701                                                                          
003801     SELECT IN-W02031                ASSIGN TO W02009D2.                  
003901                                                                          
004001     SELECT UT-W02009X              ASSIGN TO W02009D3.                   
004101                                                                          
004201     SELECT UT-W02031X              ASSIGN TO W02009D4.                   
004301                                                                          
004401 DATA DIVISION.                                                           
004501     SKIP2                                                                
004601 FILE SECTION.                                                            
004701     SKIP3                                                                
004801 FD  IN-W02009                                                            
004901     RECORDING       F                                                    
005001     BLOCK CONTAINS  0.                                                   
006001                                                                          
006101*01  POST -COPY W02009  -PRE IN09-                                        
006201                                                                          
006301 FD  IN-W02031                                                            
006401     RECORDING       F                                                    
006501     BLOCK CONTAINS  0.                                                   
006601                                                                          
006701*01  POST -COPY W02009  -PRE IN31-                                        
006801                                                                          
006901 FD  UT-W02009X                                                           
007001     RECORDING       F                                                    
007101     BLOCK CONTAINS  0.                                                   
007201                                                                          
007301*01  POST -COPY W02009X -PRE UT09X- -L.                                   
007401                                                                          
007402 FD  UT-W02031X                                                           
007403     RECORDING       F                                                    
007404     BLOCK CONTAINS  0.                                                   
007405                                                                          
007406*01  POST -COPY W02009X -PRE UT31X- -L.                                   
007407                                                                          
007501     EJECT                                                                
009101 WORKING-STORAGE SECTION.                                                 
009201*    -- CHECKED BY WY2000                                                 
009301 77  IDPGM                       PIC X(8)    VALUE 'W0200900'.            
009401 77  JA                          PIC X       VALUE 'J'.                   
009501 77  NEJ                         PIC X       VALUE 'N'.                   
009601                                                                          
009701 77  W02009-EOF-SW               PIC X       VALUE 'N'.                   
009801     88  END-OF-W02009                       VALUE 'J'.                   
009901                                                                          
010001 77  W02031-EOF-SW               PIC X       VALUE 'N'.                   
010101     88  END-OF-W02031                       VALUE 'J'.                   
010201                                                                          
011101 01  IN-AREA.                                                             
011201     03  -COPY W02009                                                     
011301     EJECT                                                                
011302 01  W02009X-AREA.                                                        
011303     03  -COPY W02009X                                                    
011304     EJECT                                                                
011305 01  W02009X-UT09-AREA.                                                   
011306     03  -COPY W02009X -PRE  UT09X-                                       
011307     EJECT                                                                
011401 01  W02031X-UT31-AREA.                                                   
011501     03  -COPY W02009X -PRE  UT31X-                                       
011601     EJECT                                                                
012301 01  DYNAMISKA-SUBPROGRAM.                                                
012401*                                                                         
012501     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012601     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012701     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012801     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012901     SKIP2                                                                
013001*    --- PARAMETRAR TILL ABEND                                            
013101                                                                          
013201 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013301 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013401     SKIP2                                                                
013501 01  FELTEXT.                                                             
013601     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013701     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013801     EJECT                                                                
013901*    --- PARAMETRAR TILL POSTSUM                                          
014001*                                                                         
014101*01  -COPY W0005   -PRE  POSTSUM-                                         
014201     EJECT                                                                
014301*    --- STATUS-KOD FRÅN IMS                                              
015001                                                                          
016001 PROCEDURE DIVISION.                                                      
017001 MAIN SECTION.                                                            
018001                                                                          
019001     PERFORM A-INIT                                                       
020001     PERFORM S01-READ-W02009                                              
021001     PERFORM UNTIL END-OF-W02009                                          
021101       MOVE CORR W02009         TO W02009X                                
021102       MOVE W02009X-AREA        TO W02009X-UT09-AREA                      
021201       PERFORM S10-SKRIV-W02009X                                          
022101       PERFORM S01-READ-W02009                                            
022201     END-PERFORM                                                          
022301                                                                          
022401     PERFORM S02-READ-W02031                                              
022501     PERFORM UNTIL END-OF-W02031                                          
022701       MOVE CORR W02009         TO W02009X                                
022702       MOVE W02009X-AREA        TO W02031X-UT31-AREA                      
022801       PERFORM S11-SKRIV-W02031X                                          
022901       PERFORM S02-READ-W02031                                            
023001     END-PERFORM                                                          
023101                                                                          
023201     PERFORM Z-FINIT                                                      
023301                                                                          
023401     MOVE ZERO TO RETURN-CODE                                             
023501     GOBACK                                                               
023601                                                                          
023701     .                                                                    
023801     EJECT                                                                
024001                                                                          
025001 A-INIT SECTION.                                                          
026001     OPEN INPUT  IN-W02009                                                
026101                 IN-W02031                                                
027001     OPEN OUTPUT UT-W02009X                                               
027101                 UT-W02031X                                               
027301                                                                          
027601     .                                                                    
027701     EJECT                                                                
027801                                                                          
035001 Z-FINIT SECTION.                                                         
036001     CLOSE IN-W02009                                                      
037001           IN-W02031                                                      
038001           UT-W02009X                                                     
039001           UT-W02031X                                                     
040001     MOVE 'S' TO POSTSUM-OPKOD                                            
041001     CALL POSTSUM USING POSTSUM-PARM                                      
042001     .                                                                    
043001     EJECT                                                                
044001                                                                          
045001 S01-READ-W02009 SECTION.                                                 
045101     SKIP2                                                                
045201     READ IN-W02009 RECORD INTO IN-AREA                                   
045301     AT END                                                               
045401        MOVE JA              TO W02009-EOF-SW                             
045501     END-READ                                                             
045601     .                                                                    
045701     EJECT                                                                
045801 S02-READ-W02031 SECTION.                                                 
045901     SKIP2                                                                
046001     READ IN-W02031 RECORD INTO IN-AREA                                   
046101     AT END                                                               
046201        MOVE JA              TO W02031-EOF-SW                             
046301     END-READ                                                             
046401     .                                                                    
046501     EJECT                                                                
046601                                                                          
046701 S10-SKRIV-W02009X SECTION.                                               
046801     WRITE UT09X-POST FROM W02009X-UT09-AREA                              
047001                                                                          
048001     MOVE 'W02009' TO POSTSUM-FDNAMN                                      
049001     MOVE 'W02009D1' TO POSTSUM-DDNAMN2                                   
050001     CALL POSTSUM USING POSTSUM-PARM                                      
060001     .                                                                    
061001     EJECT                                                                
062001 S11-SKRIV-W02031X SECTION.                                               
063001     WRITE UT31X-POST FROM W02031X-UT31-AREA                              
064001                                                                          
064101     MOVE 'W02009' TO POSTSUM-FDNAMN                                      
064201     MOVE 'W02009D2' TO POSTSUM-DDNAMN2                                   
064301     CALL POSTSUM USING POSTSUM-PARM                                      
064401     .                                                                    
064501     EJECT                                                                
