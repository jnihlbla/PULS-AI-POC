000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W3302800.                                        
000400 AUTHOR.                 KARL JOHAN HANSSON.                              
000500     DATE-WRITTEN.       NOV  1989.                                       
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET UPPDATERAR LADDNINGSDATUM PÅ BASEN WDG3               
001100*        NÄR LADDNINGEN AV DB2-BASERNA FSG2 FSG3 OCH FSG4 ÅT              
001200*        ARTIKELSTATISTIKEN SKER.                                         
001300     SKIP2                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500                                                                          
001600 INPUT-OUTPUT SECTION.                                                    
001700 FILE-CONTROL.                                                            
001800                                                                          
001900 DATA DIVISION.                                                           
002000 FILE SECTION.                                                            
002100     EJECT                                                                
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002301                                                                          
002310*    -- CHECKED BY WY2000                                                 
002400 77  PROGRAM-NAMN            PIC X(8) VALUE 'W3302800'.                   
002500                                                                          
002600 77  JA                      PIC X       VALUE 'J'.                       
002700 77  NEJ                     PIC X       VALUE 'N'.                       
002800                                                                          
002900 01  LADD-DATUM              PIC 9(6).                                    
003000 01  FILLER REDEFINES LADD-DATUM.                                         
003100     03  LADD-AA             PIC 99.                                      
003200     03  LADD-MM             PIC 99.                                      
003300     03  LADD-DD             PIC 99.                                      
003400                                                                          
003500 01  DYNAMISKA-SUBPROGRAM.                                                
003600   03  DATKORT               PIC X(8)    VALUE 'DATKORT '.                
003700   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
003800   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
003900   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
004000                                                                          
004100 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16  COMP SYNC.            
004200                                                                          
004300                                                                          
004400 01  NYCKLAR-TILL-DLI.                                                    
004500     03  W-WDGX3137-KEY.                                                  
004600         05  W-IDHTYP        PIC X(4)    VALUE '3137'.                    
004700         05  FILLER          PIC X(26)   VALUE LOW-VALUE.                 
004800                                                                          
004900*    ---- STATUSKOD FRÅN IMS                                              
005000                                                                          
005100 01  STATUS-WS               PIC XX.                                      
005200     88  SEGMENT-FINNS                    VALUE '  '.                     
005300     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
005400     88  SEGMENT-FINNS-REDAN              VALUE 'II'.                     
005500                                                                          
005600 01  GODK-STATUSKODER.                                                    
005700   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
005800     SKIP2                                                                
005900 01  SSA1                    PIC X(64).                                   
006000 01  SSA2                    PIC X(64).                                   
006100     EJECT                                                                
006200 01  DATUMKORT-ID            PIC X(6)  VALUE 'WDATUM'.                    
006300     SKIP2                                                                
006400*01  -COPY WDATKORT                                                       
006600     EJECT                                                                
006700*01  -COPY W0003                                                          
006900     EJECT                                                                
007000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
007100     SKIP3                                                                
007200 01  DLI-IO-AREA.                                                         
007300*  03  RLXX11  -COPY WDGX3138 -PRE WDGX-.                                 
007400*++INCLUDE WDGX3138C0                                                     
007500     EJECT                                                                
007600 LINKAGE SECTION.                                                         
007700                                                                          
007800*01  -COPY W0008 -PRE  MSG-                                               
008000       05  FILLER                PIC X.                                   
008100*01  -COPY W0008 -PRE  WDGX-                                              
008300       05  FILLER                PIC X.                                   
008400     EJECT                                                                
008500 PROCEDURE DIVISION  USING  MSG-PCB WDGX-PCB.                             
008600     ENTRY 'DLITCBL' USING  MSG-PCB WDGX-PCB.                             
008700                                                                          
008800     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
008900     MOVE D-AAR           TO LADD-AA                                      
009000     MOVE D-MAANAD        TO LADD-MM                                      
009100     MOVE D-DAG           TO LADD-DD                                      
009200                                                                          
009300     PERFORM IMS-GHU-WDGX11                                               
009400     IF SEGMENT-FINNS                                                     
009500         MOVE LADD-DATUM  TO WDGX-3138-TIUPPDAT                           
009600         PERFORM IMS-REPL-WDGX11                                          
009700     END-IF                                                               
009800                                                                          
009900     MOVE ZERO TO RETURN-CODE                                             
010000     GOBACK                                                               
010100     .                                                                    
010200     EJECT                                                                
010300 IMS-GHU-WDGX11  SECTION.                                                 
010400                                                                          
010500     STRING 'WLXXCJ01(WDG3KEY  =' W-WDGX3137-KEY ')'                      
010600                 DELIMITED BY SIZE INTO SSA1                              
010700     MOVE 'WLXXCJ11 '           TO SSA2                                   
010800     MOVE '  GE'                TO GODK-STATUSKODER                       
010900     CALL CBLTDLI USING GHU   WDGX-PCB DLI-IO-AREA SSA1 SSA2              
011000     MOVE WDGX-STATUS-CODE      TO STATUS-WS                              
011100     PERFORM IMS-STATUSKONTROLL                                           
011200     .                                                                    
011300     SKIP2                                                                
011400 IMS-REPL-WDGX11 SECTION.                                                 
011500                                                                          
011600     MOVE '  '                  TO GODK-STATUSKODER                       
011700     CALL CBLTDLI USING REPL WDGX-PCB DLI-IO-AREA                         
011800     MOVE WDGX-STATUS-CODE      TO STATUS-WS                              
011900     PERFORM IMS-STATUSKONTROLL                                           
012000     .                                                                    
012100     SKIP2                                                                
012200 IMS-STATUSKONTROLL SECTION.                                              
012300                                                                          
012400     SET STATUS-IX TO 1                                                   
012500     SEARCH GODK-STATUS                                                   
012600       AT END CALL FELLOG                                                 
012700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
012800     END-SEARCH                                                           
012900     .                                                                    
