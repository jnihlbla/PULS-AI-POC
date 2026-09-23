000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5704D00.                                                
000301 AUTHOR.         ANDERS HENRIKSSON.                                       
000401 DATE-WRITTEN.   NOV 2012.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNKTION:                                                            
000801*                                                                         
000901*       -PROGRAMMET LÄSER      WDH5                                       
001001*                                                                         
001101*    ABENDKODER:                                                          
001201*        U0016 -  . . . .                                                 
001301*        U1000 -  . . . .                                                 
001401*                                                                         
001501                                                                          
001601 ENVIRONMENT DIVISION.                                                    
001701                                                                          
001801 INPUT-OUTPUT SECTION.                                                    
001901                                                                          
002001 FILE-CONTROL.                                                            
002101*          --- INPUT FROM W57045                                          
002201     SELECT W57045B                    ASSIGN TO W5704DD1.                
002301                                                                          
002401*         --- OUTPUT WITH CNY USD AED AND KRW CURRENCY                    
002501     SELECT W57046B                    ASSIGN TO W5704DD2.                
002601                                                                          
002701     EJECT                                                                
002801                                                                          
002901 DATA DIVISION.                                                           
003001                                                                          
003101 FILE SECTION.                                                            
003201 FD  W57045B                                                              
003301     RECORDING       F                                                    
003401     BLOCK CONTAINS  0.                                                   
003501 01  IN-POST.                                                             
003601*    03  -COPY W57045    -PRE  IN-  -L.                                   
003701                                                                          
003801 FD  W57046B                                                              
003901     RECORDING       F                                                    
004001     BLOCK CONTAINS  0.                                                   
004101*01  POST   -COPY W57046 -PRE  UT-  -L.                                   
004201                                                                          
004301 WORKING-STORAGE SECTION.                                                 
004401 77  IDPGM                        PIC X(8)    VALUE 'W5704D00'.           
004501 77  JA                           PIC X       VALUE 'J'.                  
004601 77  NEJ                          PIC X       VALUE 'N'.                  
004701 77  FELTEXT                      PIC X(80).                              
005001 77  W57045B-EOF-SW               PIC X       VALUE 'N'.                  
005101     88  END-OF-W57045B                       VALUE 'J'.                  
006200                                                                          
006300 01  DYNAMISKA-SUBPROGRAM.                                                
006400     03  ABEND                    PIC X(8)    VALUE 'ABEND'.              
006500     03  CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.           
006600     03  FELLOG                   PIC X(8)    VALUE 'FELLOG  '.           
006700     03  POSTSUM                  PIC X(8)    VALUE 'POSTSUM'.            
006800                                                                          
008801                                                                          
008901*    --- PARAMETRAR TILL ABEND                                            
009001 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
009101 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
009201 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
009202 01  W510CURR                     PIC X(8)    VALUE 'W510CURR'.           
009301     EJECT                                                                
009401                                                                          
009501*    --- PARAMETRAR TILL POSTSUM                                          
009601*01  -COPY W0005   -PRE  POSTSUM-                                         
009701     EJECT                                                                
009702                                                                          
009703*01  -COPY W510CURR                                                       
009704     EJECT                                                                
009801                                                                          
009901*01  -COPY WWDC99                                                         
010001     EJECT                                                                
010101                                                                          
010201 01  IN-AREA-START               PIC X(24)   VALUE                        
010301                                 'IN-AREA-START  '.                       
010401                                                                          
010501*01  AREA -COPY W57045     -PRE IN-                                       
010601     EJECT                                                                
010701                                                                          
010801 01  UT-AREA-START               PIC X(24)   VALUE                        
010901                                 'UT-AREA-START  '.                       
011001                                                                          
011101*01  AREA -COPY W57046     -PRE UT-                                       
011201     EJECT                                                                
015201                                                                          
015301 PROCEDURE DIVISION.                                                      
015401                                                                          
015501 MAIN SECTION.                                                            
015701                                                                          
015801     PERFORM A-INIT                                                       
015901                                                                          
016001     PERFORM S01-READ-W57045B                                             
016101     PERFORM UNTIL END-OF-W57045B                                         
016401       PERFORM B-CONVERT-CURRENCY                                         
016901       PERFORM S01-READ-W57045B                                           
017001     END-PERFORM                                                          
017101                                                                          
017201     MOVE ZERO TO RETURN-CODE                                             
017301     GOBACK                                                               
017401     .                                                                    
017501     EJECT                                                                
017601                                                                          
017701 A-INIT SECTION.                                                          
017801     OPEN INPUT  W57045B                                                  
017901     OPEN OUTPUT W57046B                                                  
018201     .                                                                    
018301     EJECT                                                                
018401                                                                          
020203 B-CONVERT-CURRENCY SECTION.                                              
020301     COMPUTE IN-PRARTNTO ROUNDED =                                        
020401             (IN-PRARTNTO * IN-KVAVIS)                                    
020501     MOVE IN-IDARTNR                TO UT-IDARTNR                         
020601     MOVE IN-IDDC                   TO UT-IDDC                            
020701     MOVE IN-DAINLEV                TO UT-DAINLEV                         
020801     MOVE IN-KVAVIS                 TO UT-KVAVIS                          
020901     MOVE IN-PRARTNTO               TO UT-PRARTNTO                        
021001     MOVE IN-KDVALISO               TO UT-KDVALISO                        
021002     MOVE IN-KDTRADP                TO UT-KDTRADP                         
021101     PERFORM S02-WRITE-W57046B                                            
021201     .                                                                    
021301     EJECT                                                                
021401                                                                          
024501                                                                          
024601 S01-READ-W57045B SECTION.                                                
024701     READ W57045B INTO IN-AREA                                            
024801     AT END                                                               
024901        SET END-OF-W57045B TO TRUE                                        
025001                                                                          
025101     NOT AT END                                                           
025201        MOVE 'IN'         TO POSTSUM-TRANSTYP                             
025301        MOVE 'W57045B'     TO POSTSUM-FDNAMN                              
025401        MOVE 'W5704DD1'   TO POSTSUM-DDNAMN2                              
025501        CALL POSTSUM USING POSTSUM-PARM                                   
025601     END-READ                                                             
025701     .                                                                    
025801                                                                          
025901 S02-WRITE-W57046B SECTION.                                               
026001     WRITE UT-POST              FROM UT-AREA                              
026101                                                                          
026201     MOVE 'W57046B'              TO POSTSUM-FDNAMN                        
026301     MOVE 'W5704DD2'            TO POSTSUM-DDNAMN2                        
026401     CALL POSTSUM USING POSTSUM-PARM                                      
026501     .                                                                    
026601                                                                          
