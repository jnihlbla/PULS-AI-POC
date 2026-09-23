001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W9108200.                                                
001200 AUTHOR.         PER-ANDERS HELGEGREN.                                    
001300 DATE-WRITTEN.   97/08/28.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        LÄSER NED LEVINFO                                                
001800*                                                                         
001910*        PROGRAMMET LÄSER      WLLEVA (WDF1)                              
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201     SKIP2                                                                
003202*          --- LEVNR                                                      
003203     SELECT W91081                     ASSIGN TO W91082D1.                
003204     SKIP2                                                                
003205*          --- LEVUPPGIFTER                                               
003210     SELECT W91083                     ASSIGN TO W91082D2.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  W91081                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003806*01  -COPY W91081      -L.                                                
003807     SKIP3                                                                
003808 FD  W91083                                                               
003809     RECORDING       F                                                    
003810     BLOCK CONTAINS  0.                                                   
003811                                                                          
003820*01  POST -COPY W91083 -PRE  UT-  -L.                                     
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W9108200'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004601                                                                          
004602 77  W91081-EOF-SW               PIC X       VALUE 'N'.                   
004610     88  END-OF-W91081                       VALUE 'J'.                   
004700     EJECT                                                                
004800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004900 01  FILLER REDEFINES DAGENS-DATUM.                                       
005000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005300     EJECT                                                                
005400 01  DYNAMISKA-SUBPROGRAM.                                                
005500*                                                                         
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006000     SKIP2                                                                
006100*    --- PARAMETRAR TILL ABEND                                            
006200                                                                          
006300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006600     SKIP2                                                                
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007001     EJECT                                                                
007002*    --- PARAMETRAR TILL POSTSUM                                          
007003*                                                                         
007010*01  -COPY W0005   -PRE  POSTSUM-                                         
007201     EJECT                                                                
007202 01  IN-AREA-START               PIC X(24)   VALUE                        
007203                                 'IN-AREA-START  '.                       
007204     SKIP2                                                                
007205                                                                          
007206*01  AREA -COPY W91081     -PRE IN-                                       
007207     EJECT                                                                
007208 01  UT-AREA-START               PIC X(24)   VALUE                        
007209                                 'UT-AREA-START  '.                       
007210     SKIP2                                                                
007211                                                                          
007220*01  AREA -COPY W91083     -PRE UT-                                       
007300     EJECT                                                                
007400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007500*                                                                         
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  NYCKLAR-TILL-DLI.                                                    
008001     03  W-IDLEVNR-X.                                                     
008002         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
008003     03  W-IDLEVSUF-X.                                                    
008010         05  W-IDLEVSUF          PIC S9(1)   VALUE +1   COMP-3.           
008100     SKIP2                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008700     SKIP2                                                                
008800 01  GODK-STATUSKODER.                                                    
008900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009000     SKIP3                                                                
009100 01  SSA1                        PIC X(64).                               
009200 01  SSA2                        PIC X(64).                               
009300     EJECT                                                                
009400*    --- IMS FUNKTIONSKODER                                               
009500*01  -COPY W0003                                                          
009600     EJECT                                                                
009800*    ---  DLI INPUT-OUTPUT AREA                                           
009901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVA01'.                    
009902 01  DLI-IO-WLLEVA01.                                                     
009903*    03  -COPY WDF101  -PRE LEVA-                                         
009904     EJECT                                                                
009905 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVA14'.                    
009906 01  DLI-IO-WLLEVA14.                                                     
009910*    03  -COPY WDF106  -PRE LEVA-                                         
010200     EJECT                                                                
010300 LINKAGE SECTION.                                                         
010400                                                                          
010502*01  -COPY W0008  -PRE LEVA-                                              
010510     05  FILLER                  PIC X.                                   
010600     EJECT                                                                
010701 PROCEDURE DIVISION  USING LEVA-PCB.                                      
010702 MAIN SECTION.                                                            
010710     ENTRY 'DLITCBL' USING LEVA-PCB.                                      
010800                                                                          
011000                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011310     PERFORM S01-LAES-W91081                                              
011400     PERFORM UNTIL END-OF-W91081                                          
011500       MOVE IN-IDLEVNR  TO W-IDLEVNR UT-IDLEVNR                           
011600       MOVE IN-IDPTYP   TO UT-IDPTYP                                      
011610       MOVE IN-TIAAMMDD TO UT-TIAAMMDD                                    
011700       PERFORM IMS-GET-LEVA-ADR                                           
011800       IF SEGMENT-FINNS                                                   
011900          MOVE LEVA-ADR-BELEV         TO UT-BELEV                         
011910          MOVE LEVA-ADR-ADLEV-RAD1    TO UT-ADLEV-RAD1                    
011911          MOVE LEVA-ADR-ADLEV-RAD2    TO UT-ADLEV-RAD2                    
011912          MOVE LEVA-ADR-ADLEV-ORT     TO UT-ADLEV-ORT                     
011913          MOVE LEVA-ADR-ADLEVLND      TO UT-ADLEVLND                      
011920          MOVE LEVA-ADR-IDLEVTLF      TO UT-IDLEVTLF                      
011921          MOVE LEVA-ADR-IDLEVFAX      TO UT-IDLEVFAX                      
011922       ELSE                                                               
011923          MOVE SPACE                  TO UT-BELEV                         
011924                                         UT-ADLEV-RAD1                    
011925                                         UT-ADLEV-RAD2                    
011926                                         UT-ADLEV-ORT                     
011927                                         UT-ADLEVLND                      
011928                                         UT-IDLEVTLF                      
011929                                         UT-IDLEVFAX                      
011930       END-IF                                                             
012000       PERFORM S11-SKRIV-W91083                                           
012100                                                                          
012110       PERFORM S01-LAES-W91081                                            
012200     END-PERFORM                                                          
012300                                                                          
012400                                                                          
012500     PERFORM Z-FINIT                                                      
012600                                                                          
012700     MOVE ZERO TO RETURN-CODE                                             
012800     GOBACK                                                               
012900     .                                                                    
013000     EJECT                                                                
013100 A-INIT SECTION.                                                          
013201                                                                          
013210     OPEN INPUT  W91081                                                   
013301                                                                          
013310     OPEN OUTPUT W91083                                                   
013400                                                                          
013500     ACCEPT DAGENS-DATUM  FROM DATE                                       
013610     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013800     .                                                                    
013900     EJECT                                                                
014000 Z-FINIT SECTION.                                                         
014101     CLOSE W91081                                                         
014110           W91083                                                         
014201     SKIP2                                                                
014202     MOVE 'S' TO POSTSUM-OPKOD                                            
014210     CALL POSTSUM USING POSTSUM-PARM                                      
014300     .                                                                    
014401     EJECT                                                                
014402 S01-LAES-W91081  SECTION.                                                
014403     READ W91081 INTO IN-AREA                                             
014404     AT END                                                               
014406        SET END-OF-W91081 TO TRUE                                         
014407                                                                          
014408     NOT AT END                                                           
014409        MOVE 'W91081' TO POSTSUM-FDNAMN                                   
014410        MOVE 'W91082D1' TO POSTSUM-DDNAMN2                                
014413        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
014414        CALL POSTSUM USING POSTSUM-PARM                                   
014415     END-READ                                                             
014420     .                                                                    
014501     EJECT                                                                
014502 S11-SKRIV-W91083 SECTION.                                                
014503                                                                          
014504     WRITE UT-POST FROM UT-AREA                                           
014505                                                                          
014506     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
014507     MOVE 'W91083' TO POSTSUM-FDNAMN                                      
014508     MOVE 'W91082D2' TO POSTSUM-DDNAMN2                                   
014509     CALL POSTSUM USING POSTSUM-PARM                                      
014510     .                                                                    
014700     EJECT                                                                
014800 S99-ABEND SECTION.                                                       
014900                                                                          
015001     SKIP2                                                                
015002     MOVE 'S' TO POSTSUM-OPKOD                                            
015010     CALL POSTSUM USING POSTSUM-PARM                                      
015100     CALL ABEND USING RKOD-ABEND                                          
015200     .                                                                    
015300     EJECT                                                                
015400* --- IMS SEKTIONER ---                                                   
015500     SKIP3                                                                
015612 IMS-GET-LEVA-ADR SECTION.                                                
015613                                                                          
015614     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
015615          DELIMITED BY SIZE INTO SSA1                                     
015616     STRING 'WLLEVA14(IDLEVSUF =' W-IDLEVSUF-X ')'                        
015617          DELIMITED BY SIZE INTO SSA2                                     
015618     MOVE '  GE' TO GODK-STATUSKODER                                      
015619     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-WLLEVA14 SSA1 SSA2             
015620     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
015621     PERFORM IMS-STATUSKONTROLL                                           
015630     .                                                                    
015700     SKIP3                                                                
015800 IMS-STATUSKONTROLL SECTION.                                              
015900                                                                          
016000     SET STATUS-IX TO 1                                                   
016100     SEARCH GODK-STATUS                                                   
016200       AT END                                                             
016300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016400           DELIMITED BY SIZE INTO FELTEXT                                 
016500         DISPLAY FELTEXT                                                  
016600         CALL FELLOG                                                      
016700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016800         CONTINUE                                                         
016900     END-SEARCH                                                           
017000     .                                                                    
