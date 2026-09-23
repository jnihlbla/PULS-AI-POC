001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W4795800.                                                
001300*AUTHOR.         JAN-ERIK FRANTZEN.                                       
001400*DATE-WRITTEN.   93/08/10.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        LÄSER FAKTRAD OCH SKAPAR EN FIL KOMPLETTERAD MED                 
002000*        TIREGTID FRÅN WDE601.                                            
002100*                                                                         
002210*        PROGRAMMET LÄSER      WDE6                                       
002300*                                                                         
002400*    ABENDKODER:                                                          
002500*        U0016 -  . . . .                                                 
002600*        U1000 -  . . . .                                                 
002700*                                                                         
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003501     SKIP2                                                                
003502*          --- FAKTRAD                                                    
003503     SELECT W47955                     ASSIGN TO W47958D1.                
003504     SKIP2                                                                
003505*          --- UTFIL KOMPLETTERAD MED TIREGTID                            
003510     SELECT W47958                     ASSIGN TO W47958D2.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004101     SKIP3                                                                
004102 FD  W47955                                                               
004103     RECORDING       F                                                    
004104     BLOCK CONTAINS  0.                                                   
004105     SKIP2                                                                
004106*01  -COPY W479055      -L.                                               
004107     SKIP3                                                                
004108 FD  W47958                                                               
004109     RECORDING       F                                                    
004110     BLOCK CONTAINS  0.                                                   
004111     SKIP2                                                                
004120*01  POST -COPY W479058 -PRE  UT-  -L.                                    
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W4795800'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004901                                                                          
004902 77  W47955-EOF-SW               PIC X       VALUE 'N'.                   
004910     88  END-OF-W47955                       VALUE 'J'.                   
005000     SKIP2                                                                
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800*                                                                         
005900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006210     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006300     SKIP2                                                                
006400*    --- PARAMETRAR TILL ABEND                                            
006500                                                                          
006600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007201     EJECT                                                                
007202*    --- PARAMETRAR TILL POSTSUM                                          
007203*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007402 01  IN-AREA-START               PIC X(24)   VALUE                        
007403                                 'IN-AREA-START  '.                       
007404     SKIP2                                                                
007405                                                                          
007406*01  AREA -COPY W479055     -PRE IN-                                      
007407     EJECT                                                                
007408 01  UT-AREA-START               PIC X(24)   VALUE                        
007409                                 'UT-AREA-START  '.                       
007410     SKIP2                                                                
007411                                                                          
007420*01  AREA -COPY W479058     -PRE UT-                                      
007500     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008201     03  W-IDPRODNR-X.                                                    
008210         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
008300     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FINNS                       VALUE '  '.                  
008700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008900     SKIP2                                                                
009000 01  GODK-STATUSKODER.                                                    
009100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     SKIP3                                                                
009300 01  SSA1                        PIC X(64).                               
009400 01  SSA2                        PIC X(64).                               
009500     EJECT                                                                
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E601'.         
010300 01  DLI-IO-E601.                                                         
010510*    03  -COPY WDE601                                                     
010800     EJECT                                                                
010900 LINKAGE SECTION.                                                         
011000                                                                          
011101     EJECT                                                                
011102*01  -COPY W0008  -PRE WDE6-                                              
011110     05  FILLER                  PIC X.                                   
011200     EJECT                                                                
011301 PROCEDURE DIVISION  USING WDE6-PCB.                                      
011310     ENTRY 'DLITCBL' USING WDE6-PCB.                                      
011400                                                                          
011600     SKIP2                                                                
011700     PERFORM A-INIT                                                       
011810     PERFORM S01-LAES-W47955                                              
011900     PERFORM UNTIL END-OF-W47955                                          
012000        MOVE IN-IDPRODNR         TO W-IDPRODNR                            
012100        PERFORM IMS-GU-WDE601                                             
012200        IF SEGMENT-FINNS                                                  
012300           MOVE VORD-IDPRODNR     TO UT-IDPRODNR                          
012310           MOVE IN-IDKOLLI        TO UT-IDKOLLI                           
012320           MOVE IN-IDDC-LEV       TO UT-IDDC-LEV                          
012330           MOVE VORD-TIREGTID     TO UT-TIREGTID                          
012400           PERFORM S11-SKRIV-W47958                                       
012410        ELSE                                                              
012411           MOVE 'PRODNUMMER SAKNAS PÅ WDE601' TO FELTEXT-STR              
012412           DISPLAY 'PRODNUMMER ' W-IDPRODNR ' SAKNAS PÅ WDE601'           
012430        END-IF                                                            
012500                                                                          
012610        PERFORM S01-LAES-W47955                                           
012700     END-PERFORM                                                          
012800                                                                          
012900                                                                          
013000     PERFORM Z-FINIT                                                      
013100                                                                          
013200     MOVE ZERO TO RETURN-CODE                                             
013300     GOBACK                                                               
013400     .                                                                    
013500     EJECT                                                                
013600 A-INIT SECTION.                                                          
013701                                                                          
013710     OPEN INPUT  W47955                                                   
013801                                                                          
013810     OPEN OUTPUT W47958                                                   
013900     SKIP2                                                                
014110     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014300     .                                                                    
014400     EJECT                                                                
014500 Z-FINIT SECTION.                                                         
014601     CLOSE W47955                                                         
014610           W47958                                                         
014701     SKIP2                                                                
014702     MOVE 'S' TO POSTSUM-OPKOD                                            
014710     CALL POSTSUM USING POSTSUM-PARM                                      
014800     .                                                                    
014901     EJECT                                                                
014902 S01-LAES-W47955  SECTION.                                                
014903     SKIP2                                                                
014904     READ W47955 INTO IN-AREA                                             
014905     AT END                                                               
014907        SET END-OF-W47955 TO TRUE                                         
014908                                                                          
014909     NOT AT END                                                           
014910        MOVE 'W47955' TO POSTSUM-FDNAMN                                   
014911        MOVE 'W47958D1' TO POSTSUM-DDNAMN2                                
014913        CALL POSTSUM USING POSTSUM-PARM                                   
014914     END-READ                                                             
014920     .                                                                    
015001     EJECT                                                                
015002 S11-SKRIV-W47958 SECTION.                                                
015003     SKIP2                                                                
015004     WRITE UT-POST FROM UT-AREA                                           
015005                                                                          
015007     MOVE 'W47958' TO POSTSUM-FDNAMN                                      
015008     MOVE 'W47958D2' TO POSTSUM-DDNAMN2                                   
015009     CALL POSTSUM USING POSTSUM-PARM                                      
015010     .                                                                    
015200     EJECT                                                                
015900* --- IMS SEKTIONER ---                                                   
016000     SKIP3                                                                
016102 IMS-GU-WDE601 SECTION.                                                   
016103     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
016104          DELIMITED BY SIZE INTO SSA1                                     
016105     MOVE '  GE' TO GODK-STATUSKODER                                      
016106     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E601 SSA1                      
016107     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
016108     PERFORM IMS-STATUSKONTROLL                                           
016110     .                                                                    
016200     EJECT                                                                
016300 IMS-STATUSKONTROLL SECTION.                                              
016400     SKIP2                                                                
016500     SET STATUS-IX TO 1                                                   
016600     SEARCH GODK-STATUS                                                   
016700       AT END                                                             
016800         MOVE 'FEL STATUS-KOD FRÅN IMS' TO FELTEXT-STR                    
016900         DISPLAY FELTEXT                                                  
017000         CALL FELLOG                                                      
017100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017200         CONTINUE                                                         
017300     END-SEARCH                                                           
017400     .                                                                    
