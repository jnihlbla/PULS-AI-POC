000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1121800.                                                
000400 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000500 DATE-WRITTEN.   96/09/20.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        LÄSER TÖMMER WLFILC (WDR3) SOM INNEHÅLLER FELPOSTER              
001010*        TILL UT-TRATTEN FRÅN W11210-PROGRAMMET.                          
001100*        FELPOSTERNA SKRIVS PÅ FIL W11201                                 
001200*                                                                         
001300*        PROGRAMMET ÄR EN BMP UTAN CHECKPOINTS. OM ALLTFÖR                
001310*        MÅNGA FELPOSTER FINNS PÅ WDR3 SÅ AVSLUTAS PROGRAMMET             
001320*        OCH SKRIVER EN POST PÅ EN HJÄLP-FIL, VILKET LEDER TILL           
001400*        ATT JOBBET BESTÄLLS OCH KÖRS EN GÅNG TILL.                       
001410*        (OM ALLA POSTER TÖMTS UT BLIR HJÄLPFILEN TOM)                    
001420*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- FEL OCH VARNINGSLISTA                                      
002800     SELECT W11201                     ASSIGN TO W11218D1.                
002810     SKIP2                                                                
002820*          --- HJÄLP-FIL                                                  
002830     SELECT W11218                     ASSIGN TO W11218D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W11201                                                               
003500     RECORDING       V                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  POST -COPY W11201 -PRE  UT-  -L.                                     
003810     SKIP3                                                                
003820 FD  W11218                                                               
003830     RECORDING       F                                                    
003840     BLOCK CONTAINS  0.                                                   
003850                                                                          
003860 01  W11218-POST      PIC X(10).                                          
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100     SKIP2                                                                
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W1121800'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004410                                                                          
004420 01  ARBETSAREOR.                                                         
004430     03  WS-ANT                  PIC S9(5) COMP-3 VALUE ZERO.             
004440     03  WS-ANT-MAX              PIC S9(5) COMP-3 VALUE  500.             
004500     SKIP2                                                                
004600 01  FELTEXT.                                                             
004700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004900     EJECT                                                                
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM.                                       
005200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100     EJECT                                                                
006200*    --- PARAMETRAR TILL POSTSUM                                          
006300*                                                                         
006400*01  -COPY W0005   -PRE  POSTSUM-                                         
006500     EJECT                                                                
006600 01  UT-AREA-START               PIC X(24)   VALUE                        
006700                                             'UT-AREA-START'.             
006800     SKIP2                                                                
006900 01  UT-AREA.                                                             
007200*    03  FILLER -COPY W11201  -PRE UT-                                    
007400     EJECT                                                                
007410 01  HJELP-AREA.                                                          
007430     03  FILLER                  PIC X(10)   VALUE 'KÖR IGEN!!'.          
007460     EJECT                                                                
007500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007600     SKIP3                                                                
007700 01  NYCKLAR-TILL-DLI.                                                    
007800     03  W-WDR301KY-X.                                                    
007900         05  W-WDR301KY          PIC X(27)    VALUE SPACE.                
008000     03  W-IDCPYTXT-X.                                                    
008100         05  W-IDCPYTXT          PIC X(08)    VALUE 'W11201  '.           
008200     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008900     88  IMS-EJ-OK                           VALUE 'XD'.                  
009000     SKIP2                                                                
009100 01  GODK-STATUSKODER.                                                    
009200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009300     SKIP3                                                                
009400 01  SSA1                        PIC X(64).                               
009500 01  SSA2                        PIC X(64).                               
009600     EJECT                                                                
009700*    --- IMS FUNKTIONSKODER                                               
009800*01  -COPY W0003                                                          
009900     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010200     SKIP3                                                                
010300 01  DLI-IO-AREA.                                                         
010400     03  IO-AREA                 PIC X(250)  VALUE SPACE.                 
010500     SKIP3                                                                
010600     03  WLFILC01 REDEFINES IO-AREA.                                      
010700*        05  -COPY WDR301                                                 
010800     EJECT                                                                
010900 LINKAGE SECTION.                                                         
011000                                                                          
011100*01  -COPY W0009   -PRE MSG-                                              
011200     EJECT                                                                
011300*01  -COPY W0008  -PRE FILC-                                              
011400     05  FILLER                  PIC X.                                   
011500     EJECT                                                                
011600 PROCEDURE DIVISION  USING MSG-PCB FILC-PCB.                              
011700 MAIN SECTION.                                                            
011800     ENTRY 'DLITCBL' USING MSG-PCB FILC-PCB.                              
011900                                                                          
012000                                                                          
012100     PERFORM A-INIT                                                       
012200     PERFORM IMS-GET-FILC-ROT                                             
012300     MOVE ZERO TO WS-ANT                                                  
012400                                                                          
012500     PERFORM UNTIL SEGMENT-SLUT OR WS-ANT > WS-ANT-MAX                    
012600        MOVE FIL-WDR301-DATA TO UT-AREA                                   
012700        PERFORM S11-SKRIV-W11201                                          
012800        ADD +1 TO WS-ANT                                                  
012900                                                                          
013000        PERFORM IMS-DLET-FILC                                             
013100                                                                          
013200        PERFORM IMS-GET-FILC-ROT                                          
013300     END-PERFORM                                                          
013400                                                                          
013500     IF SEGMENT-FINNS                                                     
013501*      -- DET FINNS MER POSTER PÅ WLFILC --                               
013502       WRITE W11218-POST FROM HJELP-AREA                                  
013503     END-IF                                                               
013510                                                                          
013600     PERFORM Z-FINIT                                                      
013700                                                                          
013800     MOVE ZERO TO RETURN-CODE                                             
013900     GOBACK                                                               
014000     .                                                                    
014100     EJECT                                                                
014200 A-INIT SECTION.                                                          
014300     SKIP2                                                                
014500     OPEN OUTPUT W11201 W11218                                            
014700                                                                          
014800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014900     .                                                                    
015000     EJECT                                                                
015100 Z-FINIT SECTION.                                                         
015200     SKIP2                                                                
015400     CLOSE W11201 W11218                                                  
015500                                                                          
015600     MOVE 'S' TO POSTSUM-OPKOD                                            
015700     CALL POSTSUM USING POSTSUM-PARM                                      
015800     .                                                                    
015900     EJECT                                                                
016000 S11-SKRIV-W11201 SECTION.                                                
016100     SKIP2                                                                
016200     WRITE UT-POST FROM UT-AREA                                           
016300                                                                          
016400     MOVE 'POST'     TO POSTSUM-TRANSTYP                                  
016500     MOVE 'W11201 '  TO POSTSUM-FDNAMN                                    
016600     MOVE 'W11218D1' TO POSTSUM-DDNAMN2                                   
016700     CALL POSTSUM USING POSTSUM-PARM                                      
016800     .                                                                    
016900     EJECT                                                                
017000* --- IMS SEKTIONER ---                                                   
017100     SKIP3                                                                
017200 IMS-GET-FILC-ROT SECTION.                                                
017300                                                                          
017400     STRING 'WLFILC01(IDCPYTXT =' W-IDCPYTXT-X ')'                        
017500          DELIMITED BY SIZE INTO SSA1                                     
017600     MOVE '  GB' TO GODK-STATUSKODER                                      
017700     CALL CBLTDLI USING GHN FILC-PCB DLI-IO-AREA SSA1                     
017800     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
017900     PERFORM IMS-STATUSKONTROLL                                           
018000     .                                                                    
018100     SKIP3                                                                
018200 IMS-DLET-FILC SECTION.                                                   
018300                                                                          
018400     MOVE '  ' TO GODK-STATUSKODER                                        
018500     CALL CBLTDLI USING DLET FILC-PCB DLI-IO-AREA                         
018600     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
018700     PERFORM IMS-STATUSKONTROLL                                           
018800     .                                                                    
018900     EJECT                                                                
019000 IMS-STATUSKONTROLL SECTION.                                              
019100     SKIP2                                                                
019200     SET STATUS-IX TO 1                                                   
019300     SEARCH GODK-STATUS                                                   
019400       AT END                                                             
019500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
019600           DELIMITED BY SIZE INTO FELTEXT                                 
019700         DISPLAY FELTEXT                                                  
019800         CALL FELLOG                                                      
019900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
020000         CONTINUE                                                         
020100     END-SEARCH                                                           
020200     .                                                                    
