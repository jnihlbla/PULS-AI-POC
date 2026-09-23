000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W0116800.                                                
000400 AUTHOR.         STEFAN KIHLBERG.                                         
000500 DATE-WRITTEN.   94/10/25.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER ORDER-ENTRY BASEN WDK6 MED SB.                             
001000*        SKAPAR FIL MED SAMTLIGA DATAELEMENT FRÅN WDK901                  
001100*        W01168   SAMTLIGA DATAELEMENT FRÅN WDK901                        
001200*                                                                         
001300*                                                                         
001400*        PROGRAMMET LÄSER      WLARTM (WDK9)                              
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- UTVALDA DATAELEMENT FRÅN WDK901                            
002900     SELECT W01168                     ASSIGN TO W01168D1.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W01168                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  POST -COPY W01168 -PRE  W01168-  -L.                                 
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004201                                                                          
004210*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'W0116800'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600     EJECT                                                                
004700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004800 01  FILLER REDEFINES DAGENS-DATUM.                                       
004900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005200     EJECT                                                                
005300 01  DYNAMISKA-SUBPROGRAM.                                                
005400*                                                                         
005500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005900     SKIP2                                                                
006000*    --- PARAMETRAR TILL ABEND                                            
006100                                                                          
006200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006400     SKIP2                                                                
006500 01  FELTEXT.                                                             
006600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006800     EJECT                                                                
006900*    --- PARAMETRAR TILL POSTSUM                                          
007000*                                                                         
007100*01  -COPY W0005   -PRE  POSTSUM-                                         
007200     EJECT                                                                
007300 01  W01168-AREA-START           PIC X(24)   VALUE                        
007400                                 'W01168-AREA-START  '.                   
007500     SKIP2                                                                
007600                                                                          
007700 01  W01168-AREA.                                                         
007800     03 ORDER-ENTRY-UPPGIFTER.                                            
007900        05 -COPY W01168                                                   
008000     EJECT                                                                
008100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008200*                                                                         
008300     EJECT                                                                
008400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008500     SKIP3                                                                
008600 01  NYCKLAR-TILL-DLI.                                                    
008700     03  W-IDARTNR-X.                                                     
008800         05  W-IDARTNR           PIC X(9)    VALUE SPACE.                 
008900     SKIP2                                                                
009000*    --- STATUS-KOD FRÅN IMS                                              
009100 01  STATUS-WS                   PIC XX.                                  
009200     88  SEGMENT-FINNS                       VALUE '  '.                  
009300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009400     SKIP2                                                                
009500 01  GODK-STATUSKODER.                                                    
009600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009700     SKIP3                                                                
009800 01  SSA1                        PIC X(64).                               
009900 01  SSA2                        PIC X(64).                               
010000     EJECT                                                                
010100*    --- IMS FUNKTIONSKODER                                               
010200*01  -COPY W0003                                                          
010300     EJECT                                                                
010400*    ---  DLI INPUT-OUTPUT AREA                                           
010500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010600     SKIP3                                                                
010700 01  DLI-IO-AREA.                                                         
010800     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
010900     SKIP3                                                                
011000     03  WDK901 REDEFINES IO-AREA.                                        
011100*        05  -COPY WDK901                                                 
011200     EJECT                                                                
011300 LINKAGE SECTION.                                                         
011400                                                                          
011500     EJECT                                                                
011600*01  -COPY W0008  -PRE WDK9-                                              
011700     05  FILLER                  PIC X.                                   
011800     EJECT                                                                
011900 PROCEDURE DIVISION  USING WDK9-PCB.                                      
012000     ENTRY 'DLITCBL' USING WDK9-PCB.                                      
012100                                                                          
012200     PERFORM A-INIT                                                       
012300     PERFORM IMS-GET-WDK9                                                 
012400     PERFORM UNTIL SEGMENT-SLUT                                           
012500        EVALUATE WDK9-SEG-NAME-FB                                         
012600           WHEN 'WDK901  '                                                
012700              PERFORM B-FLYTTA-WDK901                                     
012800              PERFORM S11-SKRIV-W01168                                    
012900         END-EVALUATE                                                     
013000         PERFORM IMS-GET-WDK9                                             
013100     END-PERFORM                                                          
013200     PERFORM Z-FINIT                                                      
013300     MOVE ZERO TO RETURN-CODE                                             
013400     GOBACK                                                               
013500     .                                                                    
015400     EJECT                                                                
015500                                                                          
015600                                                                          
015700 A-INIT SECTION.                                                          
015800                                                                          
015900     OPEN OUTPUT W01168                                                   
016000                                                                          
016100     ACCEPT DAGENS-DATUM  FROM DATE                                       
016200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016300     .                                                                    
016400     EJECT                                                                
016500                                                                          
016600                                                                          
016700 B-FLYTTA-WDK901 SECTION.                                                 
016710                                                                          
016720     MOVE CORR ART-WDK901   TO ART-W01168                                 
016800     .                                                                    
016900     EJECT                                                                
017000                                                                          
017100                                                                          
017200 Z-FINIT SECTION.                                                         
017300     CLOSE W01168                                                         
017400     SKIP2                                                                
017500     MOVE 'S' TO POSTSUM-OPKOD                                            
017600     CALL POSTSUM USING POSTSUM-PARM                                      
017700     .                                                                    
017800     EJECT                                                                
017900                                                                          
018000                                                                          
018100 S11-SKRIV-W01168 SECTION.                                                
018200                                                                          
018300     WRITE W01168-POST FROM W01168-AREA                                   
018400                                                                          
018600     MOVE 'W01168' TO POSTSUM-FDNAMN                                      
018700     MOVE 'W01168D1' TO POSTSUM-DDNAMN2                                   
018800     CALL POSTSUM USING POSTSUM-PARM                                      
018900     .                                                                    
019000     EJECT                                                                
019900* --- IMS SEKTIONER ---                                                   
020000     SKIP3                                                                
020100     EJECT                                                                
020200 IMS-GET-WDK9   SECTION.                                                  
020300                                                                          
020400     CALL CBLTDLI USING GN WDK9-PCB DLI-IO-AREA                           
020500     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
020600     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
020700     PERFORM IMS-STATUSKONTROLL                                           
020800     .                                                                    
020900     EJECT                                                                
021000 IMS-STATUSKONTROLL SECTION.                                              
021100                                                                          
021200     SET STATUS-IX TO 1                                                   
021300     SEARCH GODK-STATUS                                                   
021400       AT END                                                             
021500         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
021600         DISPLAY FELTEXT                                                  
021700         CALL FELLOG                                                      
021800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
021900         CONTINUE                                                         
022000     END-SEARCH                                                           
022100     .                                                                    
