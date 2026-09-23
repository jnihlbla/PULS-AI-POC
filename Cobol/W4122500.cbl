000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4122500.                                                
000400 AUTHOR.         UMESH JAIN.                                              
000500 DATE-WRITTEN.   09/DEC/2021.                                             
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        READS CUSTOMER DB WDB MED SB.                                    
001000*        CREATE A FILE WITH WDB601 TO BE SENT TO DATALAKE.                
002000*                                                                         
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600*          --- SAMTLIGA DATAELEMENT FRÅN WDB601                           
002700     SELECT W41225                     ASSIGN TO W41225D1.                
002710*          --- SAMTLIGA DATAELEMENT FRÅN WDB601                           
002720     SELECT W41225X                    ASSIGN TO W41225D2.                
002800                                                                          
004100 DATA DIVISION.                                                           
004200                                                                          
004300 FILE SECTION.                                                            
004400                                                                          
005100 FD  W41225                                                               
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400                                                                          
005500*01  POST -COPY WDB601  -PRE  W41225-  -L.                                
005600                                                                          
005610 FD  W41225X                                                              
005620     RECORDING       F                                                    
005630     BLOCK CONTAINS  0.                                                   
005640                                                                          
005650*01  POST -COPY WDB601X -PRE  W41225X-  -L.                               
005660                                                                          
005700 WORKING-STORAGE SECTION.                                                 
005800 77  IDPGM                       PIC X(8)    VALUE 'W4122500'.            
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
007000                                                                          
008000 01  ARBETSAREOR.                                                         
009000     03  WS-SPAR-IDARTNR         PIC S9(9)   VALUE ZERO COMP-3.           
009100     03  IX                      PIC S9(3)   VALUE ZERO COMP-3.           
009200     EJECT                                                                
009300 01  DYNAMISKA-SUBPROGRAM.                                                
009400*                                                                         
009500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009900     SKIP2                                                                
010000*    --- PARAMETRAR TILL ABEND                                            
011000                                                                          
012000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
012100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012200     SKIP2                                                                
012300 01  FELTEXT.                                                             
012400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012600     EJECT                                                                
012700                                                                          
012800*    --- PARAMETRAR TILL POSTSUM                                          
012900                                                                          
013000*01  -COPY W0005   -PRE  POSTSUM-                                         
013100     EJECT                                                                
013600 01  W41225-AREA.                                                         
013700     03  CUST-INFO.                                                       
013800         05 -COPY WDB601                                                  
013900                                                                          
013910 01  W41225X-AREA.                                                        
013920     03  CUST-INFO-X.                                                     
013930         05 -COPY WDB601X                                                 
013940                                                                          
014000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015000*                                                                         
016000     EJECT                                                                
017000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018000     SKIP3                                                                
019000     SKIP2                                                                
020000*    --- STATUS-KOD FRÅN IMS                                              
030000 01  STATUS-WS                   PIC XX.                                  
040000     88  SEGMENT-FINNS                       VALUE '  '.                  
041000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
042000     SKIP2                                                                
043000 01  GODK-STATUSKODER.                                                    
044000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
045000     SKIP3                                                                
045100 01  SSA1                        PIC X(64).                               
045200 01  SSA2                        PIC X(64).                               
045300     EJECT                                                                
045400*    --- IMS FUNKTIONSKODER                                               
045500*01  -COPY W0003                                                          
045600     EJECT                                                                
045700*    ---  DLI INPUT-OUTPUT AREA                                           
045800 01  FILLER                      PIC X(16)   VALUE 'DLI-4O-AREA'.         
045900     SKIP3                                                                
046000 01  DLI-IO-AREA.                                                         
046100     03  IO-AREA                 PIC X(657)  VALUE SPACE.                 
046200                                                                          
046300     03  WDB601    REDEFINES  IO-AREA.                                    
046400         05  -COPY WDB601                                                 
046500                                                                          
046600 LINKAGE SECTION.                                                         
046700                                                                          
046800*01  -COPY W0008  -PRE WDB6-                                              
046900     05  FILLER                  PIC X.                                   
047000                                                                          
048000 PROCEDURE DIVISION  USING WDB6-PCB.                                      
049000     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
049100                                                                          
049200     PERFORM A-INIT                                                       
049300     PERFORM IMS-GET-WDB6                                                 
049400     PERFORM UNTIL SEGMENT-SLUT                                           
049500        EVALUATE WDB6-SEG-NAME-FB                                         
049600           WHEN 'WDB601  '                                                
049700             INITIALIZE W41225X-AREA                                      
049900             PERFORM B-FLYTTA-WDB601                                      
050000             PERFORM S01-SKRIV-W41225                                     
050100             PERFORM S02-SKRIV-W41225X                                    
052000        END-EVALUATE                                                      
052100        PERFORM IMS-GET-WDB6                                              
052200     END-PERFORM                                                          
052300     PERFORM Z-FINIT                                                      
052400     MOVE ZERO TO RETURN-CODE                                             
052500     GOBACK                                                               
052600     .                                                                    
052700     EJECT                                                                
052800                                                                          
052900 A-INIT SECTION.                                                          
053000                                                                          
053100     OPEN OUTPUT W41225                                                   
053200                 W41225X                                                  
053300                                                                          
053400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
053500     .                                                                    
053600  B-FLYTTA-WDB601 SECTION.                                                
053700                                                                          
053810     MOVE CORR DCS-WDB601 IN DLI-IO-AREA TO                               
053900               DCS-WDB601 IN W41225-AREA                                  
053910                                                                          
053920     MOVE CORR DCS-WDB601 IN DLI-IO-AREA TO                               
053930               DCS-WDB601X IN W41225X-AREA                                
054000     .                                                                    
055000     EJECT                                                                
056000                                                                          
057000                                                                          
058000 Z-FINIT SECTION.                                                         
059000     CLOSE W41225                                                         
059100           W41225X                                                        
060000                                                                          
070100     MOVE 'S' TO POSTSUM-OPKOD                                            
070200     CALL POSTSUM USING POSTSUM-PARM                                      
070300     .                                                                    
070400     EJECT                                                                
070500                                                                          
070600                                                                          
071700 S01-SKRIV-W41225 SECTION.                                                
071800                                                                          
071900     WRITE W41225-POST FROM W41225-AREA                                   
072000                                                                          
072100     MOVE 'W41225' TO POSTSUM-FDNAMN                                      
072200     MOVE 'W41225D1' TO POSTSUM-DDNAMN2                                   
072300     CALL POSTSUM USING POSTSUM-PARM                                      
072400     .                                                                    
072500                                                                          
072510 S02-SKRIV-W41225X SECTION.                                               
072520                                                                          
072530     WRITE W41225X-POST FROM W41225X-AREA                                 
072540                                                                          
072550     MOVE 'W41225' TO POSTSUM-FDNAMN                                      
072560     MOVE 'W41225D2' TO POSTSUM-DDNAMN2                                   
072570     CALL POSTSUM USING POSTSUM-PARM                                      
072580     .                                                                    
072590                                                                          
072600* --- IMS SEKTIONER ---                                                   
072700                                                                          
072800 IMS-GET-WDB6   SECTION.                                                  
072900                                                                          
073000     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA                           
074000     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
075000     MOVE '  GAGKGBGE' TO GODK-STATUSKODER                                
076000     PERFORM IMS-STATUSKONTROLL                                           
077000     .                                                                    
078000     EJECT                                                                
079000 IMS-STATUSKONTROLL SECTION.                                              
079100                                                                          
079200     SET STATUS-IX TO 1                                                   
079300     SEARCH GODK-STATUS                                                   
079400       AT END                                                             
079500         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
079600         DISPLAY FELTEXT                                                  
079700         CALL FELLOG                                                      
079800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
079900         CONTINUE                                                         
080000     END-SEARCH                                                           
090000     .                                                                    
