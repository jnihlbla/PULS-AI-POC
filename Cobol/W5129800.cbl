000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W5129800.                                            
000300 AUTHOR.             ANDER HENRIKSSON                                     
000400 DATE-WRITTEN.       2020-11-18                                           
000500     EJECT                                                                
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*                                                                         
001000*    READ WDJ2 TO GET KIT PARTS AND THE PARTS IN THE KITS                 
001100*                                                                         
001200*    UTFIL: W51298                                                        
001300*                                                                         
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600 INPUT-OUTPUT SECTION.                                                    
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900                                                                          
002000     SELECT W51298           ASSIGN TO      W51298D1.                     
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300 FILE SECTION.                                                            
002400     SKIP2                                                                
002500 FD  W51298                                                               
002600     LABEL RECORD STANDARD                                                
002700     RECORDING F                                                          
002800     BLOCK CONTAINS 0.                                                    
002900*01  W51298-POST -COPY W51298     -L                                      
003000 WORKING-STORAGE SECTION.                                                 
003100     SKIP2                                                                
003200                                                                          
003300 77  PROGRAM-NAMN                PIC X(08)   VALUE 'W5129800'.            
003400 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003410 77  WS-SATS-IDARTNR             PIC S9(9) COMP-3 VALUE ZEROS.            
003500                                                                          
003600*      --- VALID IDDC CODES                                               
003700*                                                                         
003800*01    -COPY WWDCKONS                                                     
003900       EJECT                                                              
004000*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
004100                                                                          
004200 01  DYNAMISKA-SUBPROGRAM.                                                
004300   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
004400   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI'.             
004500   03  FELLOG                    PIC X(8)    VALUE 'FELLOG '.             
004600     EJECT                                                                
004700*    ---- PARAMETRAR TILL POSTSUM                                         
004800                                                                          
004900*01  -COPY W0005      -PRE POSTSUM-.                                      
005000                                                                          
005100*    ---- SKRIVAREA.                                                      
005200*01  AREA  -COPY W51298    -PRE UT-                                       
005300                                                                          
005400*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
005500                                                                          
005600 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
005700                                                                          
005800*    ---- STATUSKOD FRÅN IMS                                              
005900                                                                          
006000 01  STATUS-WS                   PIC XX.                                  
006100     88  SEGMENT-FOUND                       VALUE '  '.                  
006200     88  SEGMENT-END-OF-DB                   VALUE 'GB'.                  
006300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
006400     SKIP3                                                                
006500 01  GODK-STATUSKODER.                                                    
006600   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
006700     SKIP3                                                                
006800 01  SSA1                        PIC X(64).                               
006810 01  SSA2                        PIC X(64).                               
006900     EJECT                                                                
006901                                                                          
006902 01 W-IDORDNST-X.                                                         
006920     03  W-IDORDNSB              PIC S9(5)   VALUE ZERO COMP-3.           
006930     03  W-IDORDNSS              PIC S9(1)   VALUE ZERO COMP-3.           
007000*01      -COPY W0003.                                                     
007100     EJECT                                                                
007294 01  FILLER                      PIC X(16)  VALUE                         
007295                                            'DLI-IO-AREA'.                
007296 01  DLI-IO-AREA1.                                                        
007297     03  IO-AREA1                PIC X(150)  VALUE SPACE.                 
007298     SKIP3                                                                
007299     03  WLSATG01 REDEFINES IO-AREA1.                                     
007300*        05  -COPY WDJ201                                                 
007400     EJECT                                                                
007500 01  DLI-IO-AREA2.                                                        
007600     03  IO-AREA2                PIC X(150)  VALUE SPACE.                 
007700     SKIP3                                                                
007800     03  WLSATG11 REDEFINES IO-AREA2.                                     
007810*        05  -COPY WDJ211                                                 
007891     EJECT                                                                
007900 LINKAGE SECTION.                                                         
008000     SKIP2                                                                
008100*    -COPY W0008 -PRE SATG-.                                              
008200   05  FILLER                  PIC X.                                     
008300     EJECT                                                                
008400 PROCEDURE DIVISION  USING SATG-PCB.                                      
008500     ENTRY 'DLITCBL' USING SATG-PCB.                                      
008600     SKIP2                                                                
008700     PERFORM A-INIT                                                       
008800                                                                          
008900     PERFORM IMS-GN-WDJ201                                                
009000     PERFORM UNTIL SEGMENT-END-OF-DB OR SEGMENT-MISSING                   
009010       MOVE SHUV-IDORDNSB    TO W-IDORDNSB                                
009020       MOVE SHUV-IDORDNSS    TO W-IDORDNSS                                
009100       MOVE SHUV-IDARTNR     TO WS-SATS-IDARTNR                           
009102       PERFORM IMS-GN-WDJ211                                              
009103       PERFORM UNTIL SEGMENT-END-OF-DB OR SEGMENT-MISSING                 
009152          PERFORM S01-SKRIV-UTPOST                                        
009153          PERFORM IMS-GN-WDJ211                                           
009154       END-PERFORM                                                        
009400       PERFORM IMS-GN-WDJ201                                              
009500                                                                          
009600     END-PERFORM                                                          
009700                                                                          
009800     PERFORM Z-FINIT                                                      
009900     MOVE ZERO TO RETURN-CODE                                             
010000     GOBACK                                                               
010100                                                                          
010200     .                                                                    
010300     EJECT                                                                
010400 A-INIT SECTION.                                                          
010500                                                                          
010600     OPEN OUTPUT W51298                                                   
010700     MOVE PROGRAM-NAMN       TO POSTSUM-PROGNAMN                          
010800     MOVE 'W51298'           TO POSTSUM-FDNAMN                            
010900     MOVE 'W51298D1'         TO POSTSUM-DDNAMN2                           
011100     .                                                                    
011200     EJECT                                                                
011210                                                                          
011300 Z-FINIT SECTION.                                                         
011400                                                                          
011500     CLOSE W51298                                                         
011600     MOVE 'S' TO POSTSUM-OPKOD                                            
011700     CALL POSTSUM USING POSTSUM-PARM                                      
011800                                                                          
011900     .                                                                    
012000     EJECT                                                                
012100 S01-SKRIV-UTPOST SECTION.                                                
012200                                                                          
012302     MOVE WS-SATS-IDARTNR            TO  UT-SATS-IDARTNR                  
012310     MOVE SRAD-IDARTNR               TO  UT-SRAD-IDARTNR                  
012400     MOVE SRAD-REANTPSA              TO  UT-SRAD-REANTPSA                 
012600     WRITE W51298-POST FROM UT-AREA                                       
012700                                                                          
012800     .                                                                    
012900     EJECT                                                                
013000*    ---- IMS SEKTIONER                                                   
013100 IMS-GN-WDJ201 SECTION.                                                   
013200                                                                          
013500     CALL CBLTDLI USING GN SATG-PCB DLI-IO-AREA1                          
013501     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
013510     MOVE '  GEGB' TO GODK-STATUSKODER                                    
013700     PERFORM IMS-STATUSKONTROLL                                           
013800     .                                                                    
013900     SKIP3                                                                
013901 IMS-GN-WDJ211 SECTION.                                                   
013902     STRING 'WLSATG01(IDORDNST =' W-IDORDNST-X ')'                        
013903          DELIMITED BY SIZE INTO SSA1                                     
013904     MOVE   'WLSATG11'   TO SSA2                                          
013905     MOVE '  GEGB' TO GODK-STATUSKODER                                    
013906     CALL CBLTDLI USING GN SATG-PCB DLI-IO-AREA2 SSA1 SSA2                
013907     MOVE SATG-STATUS-CODE TO STATUS-WS                                   
013908     PERFORM IMS-STATUSKONTROLL                                           
013909     .                                                                    
013910     SKIP3                                                                
014000 IMS-STATUSKONTROLL SECTION.                                              
014100                                                                          
014200     SET STATUS-IX TO 1                                                   
014300     SEARCH GODK-STATUS                                                   
014400     AT END                                                               
014500     STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                               
014600     DELIMITED BY SIZE INTO FELTEXT                                       
014700     CALL FELLOG                                                          
014800     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
014900     END-SEARCH                                                           
015000     .                                                                    
