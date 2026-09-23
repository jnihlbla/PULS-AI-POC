000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5550200.                                                
000300 AUTHOR.         KARL JOHAN HANSSON                                       
000400 DATE-WRITTEN.   00/02/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET LÄSER HELA BASEN WDL9/WLLOGA (SALDOUPPDAT)            
000900*        OCH SKAPAR EN UTFIL W55510 PÅ ALLA TRANSAR MED IDPGM             
001000*        W4752000 OCH W4752100.                                           
001100*                                                                         
001200*        PROGRAMMET LÄSER      WLLOGA (WDL9)                              
001300*                                                                         
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- ALLT PÅ BASEN WDL9                                         
002200     SELECT W55510                     ASSIGN TO W55510D1.                
002300                                                                          
002400 DATA DIVISION.                                                           
002500                                                                          
002600 FILE SECTION.                                                            
002700                                                                          
002800 FD  W55510                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  POST -COPY W55502 -PRE  UT-   -L.                                    
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(8)    VALUE 'W5551000'.            
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000                                                                          
004100 01  DYNAMISKA-SUBPROGRAM.                                                
004200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004600     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
004700                                                                          
004800*    --- PARAMETRAR TILL DATKORT                                          
004900                                                                          
005000 01  PROGRAM-NAMN            PIC X(8)    VALUE 'W55510'.                  
005100 01  DATUMKORT-ID            PIC X(8)    VALUE 'WDATUM'.                  
005200                                                                          
005300*01  -COPY WDATKORT                                                       
005400     EJECT                                                                
005500*    --- PARAMETRAR TILL ABEND                                            
005600                                                                          
005700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006000     SKIP2                                                                
006100 01  FELTEXT.                                                             
006200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006400     EJECT                                                                
006500*    --- PARAMETRAR TILL POSTSUM                                          
006600*                                                                         
006700*01  -COPY W0005   -PRE  POSTSUM-                                         
006800     EJECT                                                                
006900 01  FILLER                      PIC X(24)   VALUE 'UT-AREA'.             
007000                                                                          
007100*01  AREA -COPY W55502     -PRE UT-                                       
007200     EJECT                                                                
007300                                                                          
007400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007500*                                                                         
007600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007700     SKIP3                                                                
007800*    --- STATUS-KOD FRÅN IMS                                              
007900 01  STATUS-WS                   PIC XX.                                  
008000     88  SEGMENT-FINNS                       VALUE '  '.                  
008100     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
008200                                                                          
008300 01  GODK-STATUSKODER.                                                    
008400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008500                                                                          
008600 01  SSA1                        PIC X(64).                               
008700     EJECT                                                                
008800*    --- IMS FUNKTIONSKODER                                               
008900*01  -COPY W0003                                                          
009000     EJECT                                                                
009100*    ---  DLI INPUT-OUTPUT AREA                                           
009200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLOGA'.                      
009300 01  DLI-IO-WLLOGA.                                                       
009400                                                                          
009500*      05  -COPY WDL901                                                   
009600     EJECT                                                                
009700 LINKAGE SECTION.                                                         
009800                                                                          
009900     EJECT                                                                
010000*01  -COPY W0008  -PRE LOGA-                                              
010100     05  FILLER                  PIC X.                                   
010200     EJECT                                                                
010300 PROCEDURE DIVISION  USING LOGA-PCB.                                      
010400 MAIN SECTION.                                                            
010500     ENTRY 'DLITCBL' USING LOGA-PCB.                                      
010600                                                                          
010700     PERFORM A-INIT                                                       
010800                                                                          
010900     PERFORM IMS-GET-LOGA                                                 
011000     PERFORM UNTIL SEGMENT-SAKNAS                                         
011100      EVALUATE LOGA-SEG-NAME-FB                                           
011200        WHEN 'WDL901'                                                     
011300          IF (LOGG-IDPGM = 'W4752000' OR 'W4752100') AND                  
011400              LOGG-DAREGDAT-LADD = 0                                      
011500            MOVE DLI-IO-WLLOGA TO UT-AREA                                 
011600            PERFORM S01-SKRIV-W55510                                      
011700          END-IF                                                          
011800      END-EVALUATE                                                        
011900      PERFORM IMS-GET-LOGA                                                
012000     END-PERFORM                                                          
012100                                                                          
012200     PERFORM Z-FINIT                                                      
012300                                                                          
012400     MOVE ZERO TO RETURN-CODE                                             
012500     GOBACK                                                               
012600     .                                                                    
012700     EJECT                                                                
012800 A-INIT SECTION.                                                          
012900                                                                          
013000     OPEN OUTPUT W55510                                                   
013100                                                                          
013200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013300                                                                          
013400     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
013500     .                                                                    
013600     EJECT                                                                
013700 Z-FINIT SECTION.                                                         
013800                                                                          
013900     CLOSE W55510                                                         
014000                                                                          
014100     MOVE 'S' TO POSTSUM-OPKOD                                            
014200     CALL POSTSUM USING POSTSUM-PARM                                      
014300     .                                                                    
014400     EJECT                                                                
014500 S01-SKRIV-W55510 SECTION.                                                
014600                                                                          
014700     WRITE UT-POST FROM UT-AREA                                           
014800                                                                          
014900     MOVE 'SOL '     TO POSTSUM-TRANSTYP                                  
015000     MOVE 'W55510'   TO POSTSUM-FDNAMN                                    
015100     MOVE 'W55510D1' TO POSTSUM-DDNAMN2                                   
015200     CALL POSTSUM USING POSTSUM-PARM                                      
015300     .                                                                    
015400     EJECT                                                                
015500* --- IMS SEKTIONER ---                                                   
015600                                                                          
015700 IMS-GET-LOGA   SECTION.                                                  
015800                                                                          
015900     CALL CBLTDLI USING GN LOGA-PCB DLI-IO-WLLOGA                         
016000     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
016100     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
016200     PERFORM IMS-STATUSKONTROLL                                           
016300     .                                                                    
016400                                                                          
016500 IMS-STATUSKONTROLL SECTION.                                              
016600                                                                          
016700     SET STATUS-IX TO 1                                                   
016800     SEARCH GODK-STATUS                                                   
016900       AT END                                                             
017000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
017100           DELIMITED BY SIZE INTO FELTEXT                                 
017200         DISPLAY FELTEXT                                                  
017300         CALL FELLOG                                                      
017400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017500         CONTINUE                                                         
017600     END-SEARCH                                                           
017700     .                                                                    
