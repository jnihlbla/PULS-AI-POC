010000 ID DIVISION.                                                             
020000     SKIP2                                                                
030001 PROGRAM-ID.     WXTR9C00.                                                
040001*AUTHOR.         STEFANO GIOBBI                                           
050001*DATE-WRITTEN.   97/04/05.                                                
060000                                                                          
070000*                                                                         
080000*    FUNKTION:                                                            
090001*        PROGRAMMET LÄSER FIL MED ARTIKEL- OCH LAGERUPPGIFTER             
100001*        OCH SKAPAR ETT LAGERBAND FÖR CDC.                                
110000*                                                                         
120000*    ABENDKODER:                                                          
130000*        U0016 -  . . . .                                                 
140000*        U1000 -  . . . .                                                 
150000*                                                                         
160002*    E-TRACKER: 7450319  2008-HÖST  VOHF                                  
160003*    E-TRACKER:10254592  2015       DECOMISSION VOHF                      
170000     SKIP3                                                                
180000 ENVIRONMENT DIVISION.                                                    
190000     SKIP2                                                                
200000 INPUT-OUTPUT SECTION.                                                    
210000                                                                          
220000 FILE-CONTROL.                                                            
230000     SKIP2                                                                
240001*          --- ARTIKEL- OCH LAGERSPECIFIKA UPPGIFTER                      
250001     SELECT WXTR90-IN                  ASSIGN TO WXTR9CD1.                
260000     SKIP2                                                                
270001*          --- LAGERBAND FÖR CDC                                          
280001     SELECT WXTR9C                     ASSIGN TO WXTR9CD2.                
290000     EJECT                                                                
300000 DATA DIVISION.                                                           
310000     SKIP3                                                                
320000 FILE SECTION.                                                            
330000     SKIP3                                                                
340001 FD  WXTR90-IN                                                            
350000     RECORDING       F                                                    
360000     BLOCK CONTAINS  0.                                                   
370000                                                                          
380001*01  -COPY WXTR90      -L.                                                
390001                                                                          
400001                                                                          
410001 FD  WXTR9C                                                               
420000     RECORDING       F                                                    
430000     BLOCK CONTAINS  0.                                                   
440000                                                                          
450001*01  POST -COPY WCDCPART  -PRE WXTR9C-    -L.                             
460000     EJECT                                                                
470000 WORKING-STORAGE SECTION.                                                 
480001                                                                          
490001*    -- CHECKED BY WY2000                                                 
500001 77  IDPGM                       PIC X(8)    VALUE 'WXTR9C00'.            
510000 77  JA                          PIC X       VALUE 'J'.                   
520000 77  NEJ                         PIC X       VALUE 'N'.                   
530001                                                                          
540001 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
550001 01  FILLER REDEFINES DAGENS-DATUM.                                       
560001     03  DAGENS-DATUM-AAR        PIC 9(2).                                
570001     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
580001     03  DAGENS-DATUM-DAG        PIC 9(2).                                
590001                                                                          
600001 77  WXTR90-EOF-SW               PIC X       VALUE 'N'.                   
610001     88  END-OF-WXTR90                       VALUE 'J'.                   
620000                                                                          
630000     EJECT                                                                
640000 01  DYNAMISKA-SUBPROGRAM.                                                
650000*                                                                         
660000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
670000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
680000     SKIP2                                                                
690000*    --- PARAMETRAR TILL ABEND                                            
700000                                                                          
710000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
720000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
730000     SKIP2                                                                
740000 01  FELTEXT.                                                             
750000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
760000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
770000     EJECT                                                                
780000*    --- PARAMETRAR TILL POSTSUM                                          
790000*                                                                         
800000*01  -COPY W0005   -PRE  POSTSUM-                                         
810000     EJECT                                                                
820001 01  WXTR90-AREA-START           PIC X(24)   VALUE                        
830001                                 'WXTR90-AREA-START  '.                   
840000     SKIP2                                                                
850000                                                                          
860001*01  I90-AREA -COPY WXTR90                                                
870000     EJECT                                                                
880001 01  WXTR9C-AREA-START           PIC X(24)   VALUE                        
890001                                 'WXTR9C-AREA-START  '.                   
900001                                                                          
910001*01  CDC-AREA -COPY WCDCPART                                              
920000     EJECT                                                                
930000 PROCEDURE DIVISION.                                                      
940000                                                                          
950000     PERFORM A-INIT                                                       
960001     PERFORM S01-LAES-WXTR90                                              
970001     PERFORM UNTIL END-OF-WXTR90                                          
980001       INITIALIZE CDC-AREA                                                
990001       MOVE      IDARTNR      IN WXTR90 TO IDARTNR  IN WCDCPART           
000001       MOVE      REKSIFFR     IN WXTR90 TO REKSIFFR IN WCDCPART           
010001       MOVE      IDDC         IN WXTR90 TO IDDC     IN WCDCPART           
020001       MOVE CORR ARTIKEL-INFO IN WXTR90 TO ARTIKEL-INFO                   
030001                                                    IN WCDCPART           
040001       MOVE CORR LAGERINFO    IN WXTR90 TO CDC-INFO IN WCDCPART           
040101                                                                          
041001       MOVE TISKROT        IN WXTR90                                      
041101                             TO TISKROT  IN WCDCPART                      
041201       MOVE FLCDART        IN WXTR90                                      
041301                             TO FLCDART  IN WCDCPART                      
041401       MOVE ADLAGOMR-CD-1  IN WXTR90                                      
041501                             TO ADLAGOMR-CD-1  IN WCDCPART                
041601       MOVE ADGANG-CD-1    IN WXTR90                                      
041701                             TO ADGANG-CD-1    IN WCDCPART                
041801       MOVE ADPLATS-CD-1   IN WXTR90                                      
041901                             TO ADPLATS-CD-1   IN WCDCPART                
042001       MOVE ADLAGOMR-CD-2  IN WXTR90                                      
042101                             TO ADLAGOMR-CD-2  IN WCDCPART                
042201       MOVE ADGANG-CD-2    IN WXTR90                                      
042301                             TO ADGANG-CD-2    IN WCDCPART                
042401       MOVE ADPLATS-CD-2   IN WXTR90                                      
042501                             TO ADPLATS-CD-2   IN WCDCPART                
042601       MOVE ADLAGOMR-CD-3  IN WXTR90                                      
042701                             TO ADLAGOMR-CD-3  IN WCDCPART                
042801       MOVE ADGANG-CD-3    IN WXTR90                                      
042901                             TO ADGANG-CD-3    IN WCDCPART                
043001       MOVE ADPLATS-CD-3   IN WXTR90                                      
043101                             TO ADPLATS-CD-3   IN WCDCPART                
043201       MOVE ADLAGOMR-CD-4  IN WXTR90                                      
043301                             TO ADLAGOMR-CD-4  IN WCDCPART                
043401       MOVE ADGANG-CD-4    IN WXTR90                                      
043501                             TO ADGANG-CD-4    IN WCDCPART                
043601       MOVE ADPLATS-CD-4   IN WXTR90                                      
043701                             TO ADPLATS-CD-4   IN WCDCPART                
043801       MOVE KVLS-CD-1      IN WXTR90                                      
043901                             TO KVLS-CD-1      IN WCDCPART                
044001       MOVE KVLS-CD-2      IN WXTR90                                      
044101                             TO KVLS-CD-2      IN WCDCPART                
044201       MOVE KVLS-CD-3      IN WXTR90                                      
044301                             TO KVLS-CD-3      IN WCDCPART                
044401       MOVE KVLS-CD-4      IN WXTR90                                      
044501                             TO KVLS-CD-4      IN WCDCPART                
044601       MOVE KVRESS-CD-1    IN WXTR90                                      
044701                             TO KVRESS-CD-1    IN WCDCPART                
044801       MOVE KVRESS-CD-2    IN WXTR90                                      
044901                             TO KVRESS-CD-2    IN WCDCPART                
045001       MOVE KVRESS-CD-3    IN WXTR90                                      
045101                             TO KVRESS-CD-3    IN WCDCPART                
045201       MOVE KVRESS-CD-4    IN WXTR90                                      
045301                             TO KVRESS-CD-4    IN WCDCPART                
045901                                                                          
046001       MOVE IDPERSON-BUY IN WXTR90 TO IDPERSON-BUY IN WCDCPART            
046101       MOVE KVPB-NDC-TOT IN WXTR90 TO KVPB-NDC-TOT IN WCDCPART            
047001       MOVE KVPB-SDC-TOT IN WXTR90 TO KVPB-SDC-TOT IN WCDCPART            
050001       PERFORM S06-SKRIV-WXTR9C                                           
060001       PERFORM S01-LAES-WXTR90                                            
070000     END-PERFORM                                                          
080000     PERFORM Z-FINIT                                                      
090000                                                                          
100000     MOVE ZERO TO RETURN-CODE                                             
110000     GOBACK                                                               
120000     .                                                                    
130000     EJECT                                                                
140000 A-INIT SECTION.                                                          
150000                                                                          
160001     OPEN INPUT  WXTR90-IN                                                
170000                                                                          
180001     OPEN OUTPUT WXTR9C                                                   
190001                                                                          
200000     ACCEPT DAGENS-DATUM  FROM DATE                                       
210000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
220000     .                                                                    
230000     EJECT                                                                
240000 Z-FINIT SECTION.                                                         
250001                                                                          
260001     CLOSE WXTR90-IN                                                      
270001           WXTR9C                                                         
280001                                                                          
290000     MOVE 'S' TO POSTSUM-OPKOD                                            
300000     CALL POSTSUM USING POSTSUM-PARM                                      
310000     .                                                                    
320000     EJECT                                                                
330001 S01-LAES-WXTR90  SECTION.                                                
340001     READ WXTR90-IN INTO I90-AREA                                         
350000     AT END                                                               
360001        SET END-OF-WXTR90 TO TRUE                                         
370000                                                                          
380000     NOT AT END                                                           
390001        MOVE 'WXTR90' TO POSTSUM-FDNAMN                                   
400001        MOVE 'WXTR9CD1' TO POSTSUM-DDNAMN2                                
410000        CALL POSTSUM USING POSTSUM-PARM                                   
420000     END-READ                                                             
430000     .                                                                    
440000     EJECT                                                                
450001 S06-SKRIV-WXTR9C SECTION.                                                
460000                                                                          
470001     WRITE WXTR9C-POST FROM CDC-AREA                                      
480000                                                                          
490001     MOVE 'WXTR9C' TO POSTSUM-FDNAMN                                      
500001     MOVE 'WXTR9CD2' TO POSTSUM-DDNAMN2                                   
510000     CALL POSTSUM USING POSTSUM-PARM                                      
520000     .                                                                    
