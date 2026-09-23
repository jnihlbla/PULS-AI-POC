000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W3301700.                                    
000300 AUTHOR.                     RONNY STENHOLM                               
000400     DATE-WRITTEN.           NOV 1989.                                    
000500*                                                                         
000600*    REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.   LÄSER WDD3 OCH SKRIVER UT ARTIKELNUMMER SAMT             
000900*                ARTIKELBENÄMNING PÅ SVENSKA OCH ENGELSKA.                
001000*                                                                         
001100     EJECT                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300 INPUT-OUTPUT SECTION.                                                    
001400 FILE-CONTROL.                                                            
001500     SKIP2                                                                
001600*    ---- ARTIKELFIL-FIL W33017    OUTPUT                                 
001700                                                                          
001800     SELECT W33017           ASSIGN TO      W33017D1.                     
001900     EJECT                                                                
002000 DATA DIVISION.                                                           
002100 FILE SECTION.                                                            
002200     SKIP2                                                                
002300 FD  W33017                                                               
002400     LABEL RECORD STANDARD                                                
002500     RECORDING F                                                          
002600     BLOCK CONTAINS 0.                                                    
002700*01  POST -COPY W33017 -PRE W33017-   -L.                                 
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000     SKIP2                                                                
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300*    ---- GENERELLA KONSTANTER                                            
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700*    ---- ARBETSFÄLT                                                      
003800 77  UTSKRIFT                    PIC X       VALUE 'N'.                   
003900                                                                          
004000*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
004100                                                                          
004200 01  DYNAMISKA-SUBPROGRAM.                                                
004300   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
004400   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI'.             
004500   03  FELLOG                    PIC X(8)    VALUE 'FELLOG '.             
004600     SKIP3                                                                
004700*    ---- PARAMETRAR TILL POSTSUM                                         
004800                                                                          
004900*01  -COPY W0005      -PRE POSTSUM-.                                      
005000     EJECT                                                                
005100*    ---- UTAREA FÖR W33017-POST                                          
005200                                                                          
005300*01  FILLER                      PIC X(7)   VALUE 'UT-AREA'.              
005400                                                                          
005500*01  AREA -COPY W33017     -PRE UT-.                                      
005600     EJECT                                                                
005700                                                                          
005800*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
005900                                                                          
006000 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
006100                                                                          
006200*    ---- STATUSKOD FRÅN IMS                                              
006300                                                                          
006400 01  STATUS-WS                   PIC XX.                                  
006500     88  SEGMENT-FINNS                      VALUE '  '.                   
006600     88  SEGMENT-SLUT                       VALUE 'GB'.                   
006700     SKIP3                                                                
006800                                                                          
006900 01  GODK-STATUSKODER.                                                    
007000   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
007100     SKIP3                                                                
007200                                                                          
007300 01  SSA1                        PIC X(40).                               
007400     SKIP3                                                                
007500                                                                          
007600*01      -COPY W0003.                                                     
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)  VALUE                         
007900                                            'DLI-IO-AREA'.                
008000 01  DLI-IO-AREA.                                                         
008100   03  IO-AREA                   PIC X(120).                              
008200*                                                                         
008300*  03  WDD311 -COPY WDD311  -PRE WDD311-   -RED IO-AREA.                  
008400     EJECT                                                                
008500*  03  WDD312 -COPY WDD312  -PRE WDD312-   -RED IO-AREA.                  
008600     EJECT                                                                
008700                                                                          
008800 LINKAGE SECTION.                                                         
008900     SKIP2                                                                
009000*    -COPY W0008 -PRE WDD3-.                                              
009100    05  FILLER                   PIC XX.                                  
009200     EJECT                                                                
009300                                                                          
009400 PROCEDURE DIVISION  USING WDD3-PCB.                                      
009500     ENTRY 'DLITCBL' USING WDD3-PCB.                                      
009600 STYR SECTION.                                                            
009700     PERFORM A-INIT                                                       
009800     PERFORM IMS-GET-WDD3                                                 
009900     PERFORM UNTIL SEGMENT-SLUT                                           
010000       EVALUATE WDD3-SEG-NAME-FB                                          
010100         WHEN 'WDD311  '                                                  
010200           IF WDD311-TEXT-IDSKYLT = 'S  '                                 
010300             MOVE WDD311-TEXT-BEART TO UT-BEART-SVE                       
010400           END-IF                                                         
010500           IF WDD311-TEXT-IDSKYLT = 'GB '                                 
010600             MOVE WDD311-TEXT-BEART TO UT-BEART-ENG                       
010700           END-IF                                                         
010800         WHEN 'WDD312  '                                                  
010900           MOVE WDD312-ART-IDARTNR TO UT-IDARTNR                          
011000           PERFORM B-SKRIV-UTPOST                                         
011100       END-EVALUATE                                                       
011200       PERFORM IMS-GET-WDD3                                               
011300     END-PERFORM                                                          
011400                                                                          
011500     PERFORM Z-FINIT                                                      
011600     MOVE ZERO TO RETURN-CODE                                             
011700     GOBACK                                                               
011800     .                                                                    
011900     EJECT                                                                
012000                                                                          
012100 A-INIT SECTION.                                                          
012200     SKIP2                                                                
012300     OPEN OUTPUT W33017                                                   
012400     MOVE 'W3301700'         TO POSTSUM-PROGNAMN                          
012500     MOVE 'W33017D1'         TO POSTSUM-DDNAMN2                           
012600     MOVE 'W33017  '         TO POSTSUM-FDNAMN                            
012700     .                                                                    
012800     EJECT                                                                
012900                                                                          
013000 B-SKRIV-UTPOST SECTION.                                                  
013100     WRITE W33017-POST FROM UT-AREA                                       
013200     CALL POSTSUM USING POSTSUM-PARM                                      
013300     .                                                                    
013400     EJECT                                                                
013500                                                                          
013600 Z-FINIT SECTION.                                                         
013700     SKIP2                                                                
013800     CLOSE W33017                                                         
013900     MOVE 'S' TO POSTSUM-OPKOD                                            
014000     CALL POSTSUM USING POSTSUM-PARM                                      
014100     .                                                                    
014200     EJECT                                                                
014300                                                                          
014400*    ---- IMS SEKTIONER                                                   
014500 IMS-GET-WDD3 SECTION.                                                    
014600                                                                          
014700     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
014800     CALL CBLTDLI USING GN WDD3-PCB DLI-IO-AREA                           
014900     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
015000     PERFORM IMS-STATUSKONTROLL                                           
015100     .                                                                    
015200     SKIP3                                                                
015300 IMS-STATUSKONTROLL SECTION.                                              
015400                                                                          
015500     SET STATUS-IX TO 1                                                   
015600     SEARCH GODK-STATUS                                                   
015700       AT END CALL FELLOG                                                 
015800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
015900     END-SEARCH                                                           
016000     .                                                                    
