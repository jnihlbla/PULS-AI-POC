000100                                                                          
001000                                                                          
010000 ID DIVISION.                                                             
020000 PROGRAM-ID.     W3303300.                                                
030000 AUTHOR.         INGVAR SKJELBRED.                                        
040000 DATE-WRITTEN.   99/07/01.                                                
050000 DATE-COMPILED.                                                           
060000                                                                          
070000*                                                                         
080000*    FUNKTION:                                                            
090000*        PROGRAMMET HÄMTAR IN SENDERTAGEN FRÅN ETT BESTÄMT                
100000*        MARKNADSBOLAG TILLSAMMANS MED BESTÄLLNINGSFILEN AV               
110000*        FÖRSÄLJNINGSTATISTIK DETTA GÖRS FÖR ATT KUNNA AVGÖRA             
120000*        VILKET MARKNADSBOLAG SOM BESTÄLLT VAD.                           
130000*                                                                         
140000*                                                                         
150000*    ABENDKODER:                                                          
160004*        U0016 -  FELAKTIGT MARKNADSBOLAG                                 
180000*                                                                         
190000                                                                          
200000     SKIP3                                                                
210000 ENVIRONMENT DIVISION.                                                    
220000     SKIP2                                                                
230000 INPUT-OUTPUT SECTION.                                                    
240000                                                                          
250000 FILE-CONTROL.                                                            
260100     SKIP2                                                                
260200*          --- BESTELLNINGSFIL MED SENDERTAG SOM FÖRSTA POST              
260300     SELECT W33033                     ASSIGN TO W33033D1.                
260400     SKIP2                                                                
260500*          --- BESTÄLLNING AV FÖRSÄLJNINGSSTATISTIK                       
261000     SELECT W33036                     ASSIGN TO W33033D2.                
280000     EJECT                                                                
290000 DATA DIVISION.                                                           
300000     SKIP3                                                                
310000 FILE SECTION.                                                            
320100     SKIP3                                                                
320200 FD  W33033                                                               
320300     RECORDING       V                                                    
320400     BLOCK CONTAINS  0.                                                   
320500                                                                          
320600*01  -COPY W33033A     -L.                                                
320700                                                                          
320800*01  -COPY W33033B     -L.                                                
320900     SKIP3                                                                
321000 FD  W33036                                                               
321100     RECORDING       V                                                    
321200     BLOCK CONTAINS  0.                                                   
321300                                                                          
322000*01  POST -COPY W33036 -PRE  UT-  -L.                                     
330000     EJECT                                                                
340000 WORKING-STORAGE SECTION.                                                 
350000                                                                          
350101                                                                          
351001*    -- CHECKED BY WY2000                                                 
360000 77  IDPGM                       PIC X(8)    VALUE 'W3303300'.            
370000 77  JA                          PIC X       VALUE 'J'.                   
380000 77  NEJ                         PIC X       VALUE 'N'.                   
400100                                                                          
400200 77  W33033-EOF-SW               PIC X       VALUE 'N'.                   
401000     88  END-OF-W33033                       VALUE 'J'.                   
410000     EJECT                                                                
420000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
430000 01  FILLER REDEFINES DAGENS-DATUM.                                       
440000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
450000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
460000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
470000     EJECT                                                                
471004 01  WS-IDMARKBO                 PIC X       VALUE SPACE.                 
480000 01  DYNAMISKA-SUBPROGRAM.                                                
490000*                                                                         
500000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
511000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
520000     SKIP2                                                                
530000*    --- PARAMETRAR TILL ABEND                                            
540000                                                                          
550000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
560000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
570000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
580000     SKIP2                                                                
590000 01  FELTEXT.                                                             
600000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
610000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
620100     EJECT                                                                
620200*    --- PARAMETRAR TILL POSTSUM                                          
620300*                                                                         
621000*01  -COPY W0005   -PRE  POSTSUM-                                         
640100     EJECT                                                                
640201 01  IN-AREA1-START               PIC X(24)   VALUE                       
640301                                 'IN-AREA1-START  '.                      
641001*01  AREA -COPY W33033A -PRE IN1-                                         
641201 01  IN-AREA2-START               PIC X(24)   VALUE                       
641301                                 'IN-AREA2-START  '.                      
641401*01  AREA -COPY W33033B -PRE IN2-                                         
641500     EJECT                                                                
641600 01  UT-AREA-START               PIC X(24)   VALUE                        
641700                                 'UT-AREA-START  '.                       
641800     SKIP2                                                                
643005*01  AREA -COPY W33036  -PRE UT-                                          
650000     EJECT                                                                
660000 PROCEDURE DIVISION.                                                      
670000 MAIN SECTION.                                                            
690000     SKIP2                                                                
700000                                                                          
710000     PERFORM A-INIT                                                       
721001     PERFORM S02-LAES-W33033                                              
730000     PERFORM UNTIL END-OF-W33033                                          
730100       PERFORM C-BEARBETA                                                 
801001       PERFORM S02-LAES-W33033                                            
810000     END-PERFORM                                                          
820000                                                                          
830000                                                                          
840000     PERFORM Z-FINIT                                                      
850000                                                                          
860000     MOVE ZERO TO RETURN-CODE                                             
870000     GOBACK                                                               
880000     .                                                                    
890000     EJECT                                                                
900000 A-INIT SECTION.                                                          
910100                                                                          
911000     OPEN INPUT  W33033                                                   
920100                                                                          
921000     OPEN OUTPUT W33036                                                   
930000     SKIP2                                                                
940000     ACCEPT DAGENS-DATUM  FROM DATE                                       
951000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
952001     PERFORM S01-LAES-W33033                                              
953009     PERFORM AA-HAMTA-MARKNADSBOLAG                                       
960000     .                                                                    
970000     EJECT                                                                
970101 AA-HAMTA-MARKNADSBOLAG SECTION.                                          
970204******************************************************************        
970304**   A M1 = SVERIGE          OBS ! DETTA ÄR EN TILLFÄLLIG       **        
970404**   B M2 = VCEM             LÖSNING OCH ETT EXEMPEL PÅ HUR     **        
970504**   C M3 = ASIEN            MAN ABSOLUT INTE FÅR GÖRA          **        
970604**   D M4 = VCSA             (NÖDLÖSNING FÖR ATT IDENTIFIERA    **        
970704**   E M5 = VCNA              MARKNADSBOLAGET)                  **        
970804**   F M0 = VCI-OVR                                             **        
970904**   G M6 = VCI                                                 **        
971004******************************************************************        
971103     EVALUATE IN1-VC-IDLANDX2                                             
971204        WHEN 'M0'                                                         
971304              MOVE  'F'   TO WS-IDMARKBO                                  
971404        WHEN 'M1'                                                         
971504              MOVE  'A'   TO WS-IDMARKBO                                  
971604        WHEN 'M2'                                                         
971704              MOVE  'B'   TO WS-IDMARKBO                                  
971804        WHEN 'M3'                                                         
971904              MOVE  'C'   TO WS-IDMARKBO                                  
972004        WHEN 'M4'                                                         
972104              MOVE  'D'   TO WS-IDMARKBO                                  
972204        WHEN 'M5'                                                         
972304              MOVE  'E'   TO WS-IDMARKBO                                  
972404        WHEN 'M6'                                                         
972504              MOVE  'G'   TO WS-IDMARKBO                                  
972610        WHEN 'TT'                                                         
972710              MOVE  'T'   TO WS-IDMARKBO                                  
972720        WHEN 'P0'                                                         
972730              MOVE  'T'   TO WS-IDMARKBO                                  
972804        WHEN OTHER                                                        
972904              DISPLAY ' MARKNADSBOLAG = ' IN1-VC-IDLANDX2                 
973004              MOVE 'FELAKTIGT MARKNADSBOLAG ' TO  FELTEXT-STR             
973104              PERFORM S99-ABEND                                           
973204     END-EVALUATE                                                         
973304     .                                                                    
973404     EJECT                                                                
973504 C-BEARBETA SECTION.                                                      
973604                                                                          
973707     MOVE WS-IDMARKBO     TO UT-BEST-IDMARKBO                             
973808     MOVE IN2-IDPROMR     TO UT-IDPROMR                                   
973906     PERFORM S11-SKRIV-W33036                                             
974404     .                                                                    
975000     EJECT                                                                
980000 Z-FINIT SECTION.                                                         
990100     CLOSE W33033                                                         
991000           W33036                                                         
000100     SKIP2                                                                
000200     MOVE 'S' TO POSTSUM-OPKOD                                            
001000     CALL POSTSUM USING POSTSUM-PARM                                      
010000     .                                                                    
020100     EJECT                                                                
020201 S01-LAES-W33033  SECTION.                                                
020301     SKIP2                                                                
020401     READ W33033 INTO IN1-AREA                                            
020501     AT END                                                               
020601        SET END-OF-W33033 TO TRUE                                         
020701                                                                          
020801     NOT AT END                                                           
020901        MOVE 'W33033'   TO POSTSUM-FDNAMN                                 
021001        MOVE 'W33033D1' TO POSTSUM-DDNAMN2                                
021101        MOVE 'IN1'      TO POSTSUM-TRANSTYP                               
021201        CALL POSTSUM USING POSTSUM-PARM                                   
021301     END-READ                                                             
021401     .                                                                    
021501     EJECT                                                                
021601 S02-LAES-W33033  SECTION.                                                
021701     SKIP2                                                                
021801     READ W33033 INTO IN2-AREA                                            
021901     AT END                                                               
022001        SET END-OF-W33033 TO TRUE                                         
023001                                                                          
024001     NOT AT END                                                           
025001        MOVE 'W33033'   TO POSTSUM-FDNAMN                                 
026001        MOVE 'W33033D1' TO POSTSUM-DDNAMN2                                
027001        MOVE 'IN2'      TO POSTSUM-TRANSTYP                               
028001        CALL POSTSUM USING POSTSUM-PARM                                   
029001     END-READ                                                             
030001     .                                                                    
030101     EJECT                                                                
030200 S11-SKRIV-W33036 SECTION.                                                
030300                                                                          
030400     WRITE UT-POST FROM UT-AREA                                           
030500                                                                          
030605     MOVE 'UT'      TO POSTSUM-TRANSTYP                                   
030700     MOVE 'W33036' TO POSTSUM-FDNAMN                                      
030800     MOVE 'W33033D2' TO POSTSUM-DDNAMN2                                   
030900     CALL POSTSUM USING POSTSUM-PARM                                      
031000     .                                                                    
050000     EJECT                                                                
060000 S99-ABEND SECTION.                                                       
070000                                                                          
080100     SKIP2                                                                
080200     MOVE 'S' TO POSTSUM-OPKOD                                            
081000     CALL POSTSUM USING POSTSUM-PARM                                      
091004     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
100000     .                                                                    
