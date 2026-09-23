000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2317700.                                                
000400*AUTHOR.         BODIL LINDAHL.                                           
000500*DATE-WRITTEN.   94/08/23.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET LÄSER WDK9 MED SB OCH SUMMERAR KVOKS-BULK,            
001100*        KVOKS-DAG OCH KVOKS-VOR PER ARTIKEL .                            
001200*                                                                         
001300*        PROGRAMMET LÄSER WLARTM (WDK9)                                   
001400*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- SUMMERAD OKS PER ARTIKEL       , OSORTERAT                 
002800     SELECT W23177                     ASSIGN TO W23177D1.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W23177                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700     SKIP2                                                                
003800*01  POST -COPY W231771A   -PRE W23177-  -L.                              
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100     SKIP2                                                                
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W2317700'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004410 77  WS-KVOKS-C1                 PIC S9(7)   VALUE ZERO COMP-3.           
004500                                                                          
004600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004700 01  FILLER REDEFINES DAGENS-DATUM.                                       
004800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005100     EJECT                                                                
005200 01  DYNAMISKA-SUBPROGRAM.                                                
005300*                                                                         
005500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL POSTSUM                                          
007200*                                                                         
007300*01  -COPY W0005   -PRE  POSTSUM-                                         
007400     EJECT                                                                
007401 01  W23177-AREA-START           PIC X(24)   VALUE                        
007402                                 'W23177-AREA-START  '.                   
007403     SKIP2                                                                
007404                                                                          
007405*01  AREA -COPY W231771A   -PRE W23177-                                   
007406     EJECT                                                                
007407*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007408*                                                                         
007409     EJECT                                                                
007410 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007420     SKIP3                                                                
007430*    --- STATUS-KOD FRÅN IMS                                              
007440 01  STATUS-WS                   PIC XX.                                  
007450     88  SEGMENT-FINNS                       VALUE '  '.                  
007460     88  SEGMENT-SLUT                        VALUE 'GB'.                  
007470     SKIP2                                                                
007480 01  GODK-STATUSKODER.                                                    
007490     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007500     SKIP3                                                                
007600 01  SSA1                        PIC X(64).                               
007700 01  SSA2                        PIC X(64).                               
007800     EJECT                                                                
007900*    --- IMS FUNKTIONSKODER                                               
008000*01  -COPY W0003                                                          
008100     EJECT                                                                
008200*    ---  DLI INPUT-OUTPUT AREA                                           
008300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
008400     SKIP3                                                                
008500 01  DLI-IO-AREA.                                                         
008600     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
008700     SKIP3                                                                
008800     03  WLARTM01 REDEFINES IO-AREA.                                      
008900*        05  -COPY WDK901  -PRE ARTM-                                     
009000     EJECT                                                                
009100 LINKAGE SECTION.                                                         
009200                                                                          
009300     EJECT                                                                
009400*01  -COPY W0008  -PRE ARTM-                                              
009500     05  FILLER                  PIC X.                                   
009600     EJECT                                                                
009700 PROCEDURE DIVISION  USING ARTM-PCB.                                      
009800     ENTRY 'DLITCBL' USING ARTM-PCB.                                      
009900                                                                          
010000     SKIP2                                                                
010100     PERFORM A-INIT                                                       
010200     PERFORM IMS-GET-WDK9                                                 
010300     PERFORM UNTIL SEGMENT-SLUT                                           
010400        EVALUATE ARTM-SEG-NAME-FB                                         
010500           WHEN 'WLARTM01'                                                
010600              MOVE ZERO TO WS-KVOKS-C1                                    
010700              COMPUTE WS-KVOKS-C1 = ARTM-ART-KVOKS-BULK    +              
010800                                    ARTM-ART-KVOKS-DAG     +              
010900                                    ARTM-ART-KVOKS-VOR                    
011300              MOVE ARTM-ART-IDARTNR TO W23177-IDARTNR                     
011400              MOVE WS-KVOKS-C1      TO W23177-KVOKS-C1                    
011500              MOVE ZERO             TO W23177-KVOKS-C2                    
011600              PERFORM S11-SKRIV-W23177                                    
011700           WHEN OTHER                                                     
011800              CONTINUE                                                    
011900        END-EVALUATE                                                      
012000        PERFORM IMS-GET-WDK9                                              
012100     END-PERFORM                                                          
012200                                                                          
012300                                                                          
012400     PERFORM Z-FINIT                                                      
012500                                                                          
012600     MOVE ZERO TO RETURN-CODE                                             
012700     GOBACK                                                               
012800     .                                                                    
012900     EJECT                                                                
013000 A-INIT SECTION.                                                          
013100                                                                          
013200     OPEN OUTPUT W23177                                                   
013300                                                                          
013400     ACCEPT DAGENS-DATUM FROM DATE                                        
013500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013600     .                                                                    
013700     EJECT                                                                
013800 Z-FINIT SECTION.                                                         
013810                                                                          
013900     CLOSE W23177                                                         
014000                                                                          
014100     MOVE 'S' TO POSTSUM-OPKOD                                            
014200     CALL POSTSUM USING POSTSUM-PARM                                      
014300     .                                                                    
014400     EJECT                                                                
014500 S11-SKRIV-W23177 SECTION.                                                
014600                                                                          
014700     WRITE W23177-POST FROM W23177-AREA                                   
014800                                                                          
014900     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
014901     MOVE 'W23177'   TO POSTSUM-FDNAMN                                    
014902     MOVE 'W23177D1' TO POSTSUM-DDNAMN2                                   
014903     CALL POSTSUM USING POSTSUM-PARM                                      
014904     .                                                                    
014905     EJECT                                                                
014906* --- IMS SEKTIONER ---                                                   
014907     SKIP3                                                                
014909 IMS-GET-WDK9 SECTION.                                                    
014910     SKIP2                                                                
014920     CALL CBLTDLI USING GN ARTM-PCB DLI-IO-AREA                           
014930     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
014940     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
014950     PERFORM IMS-STATUSKONTROLL                                           
014960     .                                                                    
014970     SKIP3                                                                
014980 IMS-STATUSKONTROLL SECTION.                                              
014990     SKIP2                                                                
015000     SET STATUS-IX TO 1                                                   
015100     SEARCH GODK-STATUS                                                   
015200       AT END                                                             
015500         CALL FELLOG                                                      
015600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
015700         CONTINUE                                                         
015800     END-SEARCH                                                           
015900     .                                                                    
