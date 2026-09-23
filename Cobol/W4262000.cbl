001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W4262000.                                                
001300*AUTHOR.         GERRY CARMICHAEL.                                        
001400*DATE-WRITTEN.   91/10/07.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        PROGRAMMET LÄSER W6H7 MED SB OCH                                 
001910*        SKAPAR EN FIL MED ALLA SEGMENT.(UTOM W6H714;FELTEXTER).          
002000*                                                                         
002110*        PROGRAMMET LÄSER      W6KVAE (W6H7)                              
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401     SKIP2                                                                
003402*          --- UTFIL W42620                                               
003410     SELECT W42620                     ASSIGN TO W42620D1.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W42620                                                               
004004     RECORDING       V                                                    
004005     BLOCK CONTAINS  0.                                                   
004006     SKIP2                                                                
004110 01  W6H701-AREA.                                                         
004120    03 FILLER                    PIC X(3).                                
004130*   03  H01-AREA  -COPY W6H701  -L                                        
004150                                                                          
004160 01  W6H711-AREA.                                                         
004170    03 FILLER                    PIC X(3).                                
004180*   03  H11-AREA  -COPY W6H711  -L                                        
004191                                                                          
004192 01  W6H712-AREA.                                                         
004193    03 FILLER                    PIC X(3).                                
004194*   03  H12-AREA  -COPY W6H712  -L                                        
004195                                                                          
004196 01  W6H713-AREA.                                                         
004197    03 FILLER                    PIC X(3).                                
004198*   03  H13-AREA  -COPY W6H713  -L                                        
004199                                                                          
004200     EJECT                                                                
004210 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W4262000'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004900     EJECT                                                                
004910*    --- EOF-SWITCHAR                                                     
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM.                                       
005200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200     SKIP2                                                                
006300*    --- PARAMETRAR TILL ABEND                                            
006400                                                                          
006500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006700     SKIP2                                                                
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008700     SKIP2                                                                
008800 01  GODK-STATUSKODER.                                                    
008900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009000     SKIP3                                                                
009100 01  SSA1                        PIC X(64).                               
009300     EJECT                                                                
009400*    --- IMS FUNKTIONSKODER                                               
009500*01  -COPY W0003                                                          
009600     EJECT                                                                
009800*    ---  DLI INPUT-OUTPUT AREA                                           
009900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010000     SKIP3                                                                
010100 01  DLI-IO-AREA.                                                         
010200     03  IO-AREA                 PIC X(550)  VALUE SPACE.                 
010301     SKIP3                                                                
010302     03  W6KVAE01 REDEFINES IO-AREA.                                      
010303*        05  -COPY W6H701                                                 
010304     SKIP3                                                                
010305     03  W6KVAE11 REDEFINES IO-AREA.                                      
010310*        05  -COPY W6H711                                                 
010320     SKIP3                                                                
010330     03  W6KVAE12 REDEFINES IO-AREA.                                      
010340*        05  -COPY W6H712                                                 
010350     SKIP3                                                                
010360     03  W6KVAE13 REDEFINES IO-AREA.                                      
010370*        05  -COPY W6H713                                                 
010600     EJECT                                                                
010610 01  FILLER                  PIC X(16) VALUE 'UT-AREA'.                   
010620 01  UT-AREAN.                                                            
010630     03  UT-AREA             PIC X(550).                                  
010640     03  KR-AREA REDEFINES UT-AREA.                                       
010650         05  UT-KR-IDPTYP    PIC X(3).                                    
010660*        05  -COPY W6H701 -PRE UT-.                                       
010680     EJECT                                                                
010690     03  KOLL-AREA REDEFINES UT-AREA.                                     
010691         05  UT-KOLL-IDPTYP  PIC X(3).                                    
010692*        05  -COPY W6H711 -PRE UT-.                                       
010694     EJECT                                                                
010695     03  EK-AREA REDEFINES UT-AREA.                                       
010696         05  UT-EK-IDPTYP  PIC X(3).                                      
010697*        05  -COPY W6H712 -PRE UT-.                                       
010698     EJECT                                                                
010699     03  JUST-AREA REDEFINES UT-AREA.                                     
010700         05  UT-JUST-IDPTYP  PIC X(3).                                    
010701*        05  -COPY W6H713 -PRE UT-.                                       
010702     EJECT                                                                
010710 LINKAGE SECTION.                                                         
010800                                                                          
010901     EJECT                                                                
010902*01  -COPY W0008  -PRE W6H7-                                              
010910     05  FILLER                  PIC X.                                   
011000     EJECT                                                                
011101 PROCEDURE DIVISION  USING W6H7-PCB.                                      
011110     ENTRY 'DLITCBL' USING W6H7-PCB.                                      
011200                                                                          
011400     SKIP2                                                                
011500     PERFORM A-INIT                                                       
011600     PERFORM IMS-GET-W6H7                                                 
011610                                                                          
011700     PERFORM UNTIL SEGMENT-SLUT                                           
011800                                                                          
011860       EVALUATE W6H7-SEG-NAME-FB                                          
011870         WHEN 'W6H701'                                                    
011871           PERFORM S11-SKRIV-701-POST                                     
011892         WHEN 'W6H711'                                                    
011893           PERFORM S13-SKRIV-711-POST                                     
011894         WHEN 'W6H712'                                                    
011895           PERFORM S15-SKRIV-712-POST                                     
011896         WHEN 'W6H713'                                                    
011897           PERFORM S17-SKRIV-713-POST                                     
011900       END-EVALUATE                                                       
011901       PERFORM IMS-GET-W6H7                                               
011902                                                                          
011903     END-PERFORM                                                          
011904                                                                          
011905     PERFORM Z-FINIT                                                      
011906     MOVE ZERO TO RETURN-CODE                                             
011907     GOBACK                                                               
011908     .                                                                    
011909     EJECT                                                                
012300                                                                          
013400 A-INIT SECTION.                                                          
013601                                                                          
013610     OPEN OUTPUT W42620                                                   
013700     SKIP2                                                                
013800     ACCEPT DAGENS-DATUM  FROM DATE                                       
013910     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014100     .                                                                    
014200     EJECT                                                                
014300 Z-FINIT SECTION.                                                         
014410     CLOSE W42620                                                         
014501     SKIP2                                                                
014502     MOVE 'S' TO POSTSUM-OPKOD                                            
014510     CALL POSTSUM USING POSTSUM-PARM                                      
014600     .                                                                    
014801     EJECT                                                                
014802 S11-SKRIV-701-POST SECTION.                                              
014804     MOVE '701' TO UT-KR-IDPTYP                                           
014805     MOVE DLI-IO-AREA TO UT-KR-W6H701                                     
014806     WRITE W6H701-AREA FROM UT-AREAN                                      
014814                                                                          
014815     MOVE UT-KR-IDPTYP TO POSTSUM-TRANSTYP                                
014816     MOVE 'W42620' TO POSTSUM-FDNAMN                                      
014817     MOVE 'W42620D1' TO POSTSUM-DDNAMN2                                   
014818     CALL POSTSUM USING POSTSUM-PARM                                      
014820     .                                                                    
015000     EJECT                                                                
015010 S13-SKRIV-711-POST SECTION.                                              
015020     MOVE '711' TO UT-KOLL-IDPTYP                                         
015030     MOVE DLI-IO-AREA TO UT-KOLL-W6H711                                   
015040     WRITE W6H711-AREA FROM UT-AREAN                                      
015041                                                                          
015042     MOVE UT-KOLL-IDPTYP TO POSTSUM-TRANSTYP                              
015043     MOVE 'W42620' TO POSTSUM-FDNAMN                                      
015044     MOVE 'W42620D1' TO POSTSUM-DDNAMN2                                   
015045     CALL POSTSUM USING POSTSUM-PARM                                      
015050     .                                                                    
015060     EJECT                                                                
015070 S15-SKRIV-712-POST SECTION.                                              
015080     MOVE '712' TO UT-EK-IDPTYP                                           
015090     MOVE DLI-IO-AREA TO UT-EK-W6H712                                     
015100     WRITE W6H712-AREA FROM UT-AREAN                                      
015200                                                                          
015300     MOVE UT-EK-IDPTYP TO POSTSUM-TRANSTYP                                
015400     MOVE 'W42620' TO POSTSUM-FDNAMN                                      
015500     MOVE 'W42620D1' TO POSTSUM-DDNAMN2                                   
015600     CALL POSTSUM USING POSTSUM-PARM                                      
015610     .                                                                    
015620     EJECT                                                                
015630 S17-SKRIV-713-POST SECTION.                                              
015640     MOVE '713' TO UT-JUST-IDPTYP                                         
015650     MOVE DLI-IO-AREA TO UT-JUST-W6H713                                   
015660     WRITE W6H713-AREA FROM UT-AREAN                                      
015670                                                                          
015680     MOVE UT-JUST-IDPTYP TO POSTSUM-TRANSTYP                              
015690     MOVE 'W42620' TO POSTSUM-FDNAMN                                      
015691     MOVE 'W42620D1' TO POSTSUM-DDNAMN2                                   
015692     CALL POSTSUM USING POSTSUM-PARM                                      
015693     .                                                                    
015694     EJECT                                                                
015700* --- IMS SEKTIONER ---                                                   
015800     SKIP3                                                                
015901     EJECT                                                                
015902 IMS-GET-W6H7   SECTION.                                                  
015903     SKIP2                                                                
015904     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
015905     CALL CBLTDLI USING GN W6H7-PCB DLI-IO-AREA                           
015906     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
015907     PERFORM IMS-STATUSKONTROLL                                           
015910     .                                                                    
016000     EJECT                                                                
016100 IMS-STATUSKONTROLL SECTION.                                              
016200     SKIP2                                                                
016300     SET STATUS-IX TO 1                                                   
016400     SEARCH GODK-STATUS                                                   
016500       AT END                                                             
016600         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
016700         DISPLAY FELTEXT                                                  
016800         CALL FELLOG                                                      
016900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
017000     END-SEARCH                                                           
017100     .                                                                    
021100                                                                          
